package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.facebook.buck_project_builder.DebugOutput;
import com.facebook.buck_project_builder.FileSystem;
import com.facebook.buck_project_builder.SimpleLogger;
import com.facebook.buck_project_builder.cache.BuilderCache;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

public final class BuildTargetsBuilder {

  private final long startTime;
  private final String buckRoot;
  private final String outputDirectory;
  private final ImmutableList<String> targets;
  private final BuilderCache cache;
  /** key: output path, value: source path */
  private final Map<Path, Path> sources = new HashMap<>();

  private final Set<String> unsupportedGeneratedSources = new HashSet<>();
  private final Set<String> pythonWheelUrls = new HashSet<>();
  private final Set<ThriftLibraryTarget> thriftLibraryTargets = new HashSet<>();
  private final Set<String> swigLibraryBuildCommands = new HashSet<>();
  private final Set<String> antlr4LibraryBuildCommands = new HashSet<>();

  private final Set<String> conflictingFiles = new HashSet<>();
  private final Set<String> unsupportedFiles = new HashSet<>();

  public BuildTargetsBuilder(
      long startTime, String buckRoot, String outputDirectory, ImmutableList<String> targets) {
    this.startTime = startTime;
    this.buckRoot = buckRoot;
    this.outputDirectory = outputDirectory;
    this.targets = targets;
    this.cache = BuilderCache.readFromCache(targets);
  }

  private static void logCodeGenerationIOException(IOException exception) {
    SimpleLogger.error("IOException during python code generation: " + exception.getMessage());
  }

  private void buildPythonSources() {
    SimpleLogger.info("Building " + this.sources.size() + " python sources...");
    long start = System.currentTimeMillis();
    this.sources
        .entrySet()
        .forEach(mapping -> FileSystem.addSymbolicLink(mapping.getKey(), mapping.getValue()));
    long time = System.currentTimeMillis() - start;
    SimpleLogger.info("Built python sources in " + time + "ms.");
  }

  private void buildPythonWheels() {
    if (pythonWheelUrls.isEmpty()) {
      return;
    }
    SimpleLogger.info("Building " + this.pythonWheelUrls.size() + " python wheels...");
    long start = System.currentTimeMillis();
    new File(BuilderCache.WHEEL_CACHE_PATH).mkdirs();
    File outputDirectoryFile = new File(outputDirectory);
    this.pythonWheelUrls
        .parallelStream()
        .forEach(
            url -> {
              try {
                ImmutableSet<String> conflictingFiles =
                    FileSystem.unzipRemoteFile(
                        url, BuilderCache.WHEEL_CACHE_PATH, outputDirectoryFile);
                this.conflictingFiles.addAll(conflictingFiles);
              } catch (IOException firstException) {
                try {
                  ImmutableSet<String> conflictingFiles =
                      FileSystem.unzipRemoteFile(
                          url, BuilderCache.WHEEL_CACHE_PATH, outputDirectoryFile);
                  this.conflictingFiles.addAll(conflictingFiles);
                } catch (IOException secondException) {
                  SimpleLogger.error(
                      String.format(
                          "Cannot fetch and unzip remote python dependency at `%s` after 1 retry.",
                          url));
                  SimpleLogger.error("First IO Exception: " + firstException);
                  SimpleLogger.error("Second IO Exception: " + secondException);
                }
              }
            });
    long time = System.currentTimeMillis() - start;
    SimpleLogger.info("Built python wheels in " + time + "ms.");
  }

  private <T> void runBuildCommands(
      Collection<T> commands, String buildRuleType, CommandRunner<T> commandRunner)
      throws BuilderException {
    int numberOfSuccessfulRuns =
        commands
            .parallelStream()
            .mapToInt(
                command -> {
                  boolean buildIsSuccessful = commandRunner.run(command);
                  if (buildIsSuccessful) {
                    return 1;
                  }
                  SimpleLogger.error("Failed to build: " + command);
                  return 0;
                })
            .sum();
    if (numberOfSuccessfulRuns < commands.size()) {
      throw new BuilderException(
          String.format(
              "Failed to build some %s targets. Read the log above for more information.",
              buildRuleType));
    }
  }

  private void buildThriftLibraries() throws BuilderException {
    this.thriftLibraryTargets.removeIf(
        target -> {
          String commandString = target.getCommand();
          return commandString.contains("py:")
              && this.thriftLibraryTargets.contains(
                  new ThriftLibraryTarget(
                      commandString.replace("py:", "mstch_pyi:"),
                      target.getBaseModulePath(),
                      target.getSources()));
        });
    if (this.thriftLibraryTargets.isEmpty()) {
      return;
    }
    new File(BuilderCache.THRIFT_CACHE_PATH).mkdirs();
    int totalNumberOfThriftLibraries = this.thriftLibraryTargets.size();
    SimpleLogger.info("Building " + totalNumberOfThriftLibraries + " thrift libraries...");
    AtomicInteger numberOfBuiltThriftLibraries = new AtomicInteger(0);
    long start = System.currentTimeMillis();
    // First pass: build thrift library in cache path.
    runBuildCommands(
        this.thriftLibraryTargets,
        "thrift_library",
        target -> {
          boolean successfullyBuilt = target.build(this.buckRoot, this.cache);
          if (successfullyBuilt) {
            int builtThriftLibrariesSoFar = numberOfBuiltThriftLibraries.addAndGet(1);
            if (builtThriftLibrariesSoFar % 100 == 0) {
              // Log progress for every 100 built thrift library.
              SimpleLogger.info(
                  String.format(
                      "Built %d/%d thrift libraries.",
                      builtThriftLibrariesSoFar, totalNumberOfThriftLibraries));
            }
          }
          return successfullyBuilt;
        });
    // Second pass: establishing symbolic links
    this.thriftLibraryTargets.forEach(
        command -> {
          String baseModulePath = command.getBaseModulePath();
          Path generatedCodeRoot =
              Paths.get(BuilderCache.THRIFT_CACHE_PATH, DigestUtils.md5Hex(baseModulePath));
          try {
            Files.walk(Paths.get(generatedCodeRoot.toString(), baseModulePath))
                .forEach(
                    absolutePath -> {
                      if (absolutePath.toFile().isDirectory()) {
                        return;
                      }
                      if (absolutePath.endsWith("__init__.py") || absolutePath.endsWith("__init__.pyi")) {
                        return;
                      }
                      String relativePath = generatedCodeRoot.relativize(absolutePath).toString();
                      FileSystem.addSymbolicLink(
                          Paths.get(this.outputDirectory, relativePath), absolutePath);
                    });
          } catch (IOException exception) {
            SimpleLogger.warning(
                String.format(
                    "Cannot find generated python code because the namespace directive in the thrift file"
                        + " does not match the base_module %s specified in the TARGETS file.",
                    baseModulePath));
          }
        });
    long time = System.currentTimeMillis() - start;
    SimpleLogger.info("Built thrift libraries in " + time + "ms.");
  }

  private void buildSwigLibraries() throws BuilderException {
    if (this.swigLibraryBuildCommands.isEmpty()) {
      return;
    }
    SimpleLogger.info("Building " + this.swigLibraryBuildCommands.size() + " swig libraries...");
    String builderExecutable;
    try {
      builderExecutable =
          GeneratedBuildRuleRunner.getBuiltTargetExecutable(
              "//third-party-buck/platform007/tools/swig:bin/swig", this.buckRoot);
    } catch (IOException exception) {
      logCodeGenerationIOException(exception);
      return;
    }
    if (builderExecutable == null) {
      SimpleLogger.error("Unable to build any swig libraries because its builder is not found.");
      return;
    }
    long start = System.currentTimeMillis();
    // Swig command contains buck run, so it's better not to make it run in parallel.
    this.swigLibraryBuildCommands
        .parallelStream()
        .forEach(
            command -> {
              try {
                GeneratedBuildRuleRunner.runBuilderCommand(
                    builderExecutable + command, this.buckRoot);
              } catch (IOException exception) {
                logCodeGenerationIOException(exception);
              }
            });
    long time = System.currentTimeMillis() - start;
    SimpleLogger.info("Built swig libraries in " + time + "ms.");
  }

  private void buildAntlr4Libraries() throws BuilderException {
    if (this.antlr4LibraryBuildCommands.isEmpty()) {
      return;
    }
    SimpleLogger.info(
        "Building " + this.antlr4LibraryBuildCommands.size() + " ANTLR4 libraries...");
    String wrapperExecutable;
    String builderExecutable;
    try {
      wrapperExecutable =
          GeneratedBuildRuleRunner.getBuiltTargetExecutable(
              "//tools/antlr4:antlr4_wrapper", this.buckRoot);
      builderExecutable =
          GeneratedBuildRuleRunner.getBuiltTargetExecutable("//tools/antlr4:antlr4", this.buckRoot);
    } catch (IOException exception) {
      logCodeGenerationIOException(exception);
      return;
    }
    if (builderExecutable == null || wrapperExecutable == null) {
      SimpleLogger.error("Unable to build any ANTLR4 libraries because its builder is not found.");
      return;
    }
    String builderPrefix =
        String.format("%s --antlr4_command=\"%s\"", wrapperExecutable, builderExecutable);
    long start = System.currentTimeMillis();
    runBuildCommands(
        this.antlr4LibraryBuildCommands,
        "antlr4_library",
        command -> {
          try {
            return GeneratedBuildRuleRunner.runBuilderCommand(
                builderPrefix + command, this.buckRoot);
          } catch (IOException exception) {
            logCodeGenerationIOException(exception);
            return false;
          }
        });
    long time = System.currentTimeMillis() - start;
    SimpleLogger.info("Built ANTLR4 libraries in " + time + "ms.");
  }

  private void generateEmptyStubs() {
    SimpleLogger.info("Generating empty stubs...");
    long start = System.currentTimeMillis();
    Path outputPath = Paths.get(outputDirectory);
    this.unsupportedGeneratedSources
        .forEach(
            source -> {
              String pyiSource = source.endsWith(".py") ? source + "i" : source;
              File outputFile = new File(pyiSource);
              if (outputFile.exists()) {
                // Do not generate stubs for files that has already been handled.
                return;
              }
              String relativeUnsupportedFilename =
                  outputPath.relativize(Paths.get(source)).normalize().toString();
              this.unsupportedFiles.add(relativeUnsupportedFilename);
              outputFile.getParentFile().mkdirs();
              try {
                FileUtils.write(outputFile, "# pyre-placeholder-stub\n", Charset.defaultCharset());
              } catch (IOException exception) {
                logCodeGenerationIOException(exception);
              }
            });
    long time = System.currentTimeMillis() - start;
    SimpleLogger.info("Generate empty stubs in " + time + "ms.");
  }

  public String getBuckRoot() {
    return buckRoot;
  }

  public String getOutputDirectory() {
    return outputDirectory;
  }

  public ImmutableList<String> getTargets() {
    return targets;
  }

  @VisibleForTesting
  Map<Path, Path> getSources() {
    return sources;
  }

  @VisibleForTesting
  Set<ThriftLibraryTarget> getThriftLibraryTargets() {
    return thriftLibraryTargets;
  }

  @VisibleForTesting
  Set<String> getSwigLibraryBuildCommands() {
    return swigLibraryBuildCommands;
  }

  @VisibleForTesting
  Set<String> getAntlr4LibraryBuildCommands() {
    return antlr4LibraryBuildCommands;
  }

  @VisibleForTesting
  Set<String> getUnsupportedGeneratedSources() {
    return unsupportedGeneratedSources;
  }

  @VisibleForTesting
  Set<String> getPythonWheelUrls() {
    return pythonWheelUrls;
  }

  void addSourceMapping(Path sourcePath, Path outputPath) {
    Path existingSourcePath = this.sources.get(outputPath);
    if (existingSourcePath != null && !existingSourcePath.equals(sourcePath)) {
      this.conflictingFiles.add(
          Paths.get(this.outputDirectory).relativize(outputPath).normalize().toString());
      return;
    }
    this.sources.put(outputPath, sourcePath);
  }

  void addUnsupportedGeneratedSource(String generatedSourcePath) {
    unsupportedGeneratedSources.add(generatedSourcePath);
  }

  void addPythonWheelUrl(String url) {
    pythonWheelUrls.add(url);
  }

  void addThriftLibraryTarget(ThriftLibraryTarget thriftLibraryTarget) {
    thriftLibraryTargets.add(thriftLibraryTarget);
  }

  void addSwigLibraryBuildCommand(String command) {
    swigLibraryBuildCommands.add(command);
  }

  void addAntlr4LibraryBuildCommand(String command) {
    antlr4LibraryBuildCommands.add(command);
  }

  public DebugOutput buildTargets() throws BuilderException {
    this.buildThriftLibraries();
    this.buildSwigLibraries();
    this.buildAntlr4Libraries();
    this.buildPythonSources();
    this.buildPythonWheels();
    this.generateEmptyStubs();
    new BuilderCache(startTime, thriftLibraryTargets).writeToCache(this.targets);
    return new DebugOutput(this.conflictingFiles, this.unsupportedFiles);
  }

  private interface CommandRunner<T> {
    boolean run(T command);
  }
}
