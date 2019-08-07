package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuckCells;
import com.facebook.buck_project_builder.BuckQuery;
import com.facebook.buck_project_builder.BuilderException;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public final class BuildTargetsCollector {

  private final String buckRoot;
  private final String outputDirectory;
  private final PlatformSelector selector;
  private final CommandRewriter rewriter;
  private final Set<String> requiredRemoteFiles = new HashSet<>();

  /** key: output path, value: source path */
  private final Map<Path, Path> sources = new HashMap<>();

  private final Set<String> unsupportedGeneratedSources = new HashSet<>();
  private final Set<String> pythonWheelUrls = new HashSet<>();
  private final Set<ThriftLibraryTarget> thriftLibraryTargets = new HashSet<>();
  private final Set<String> swigLibraryBuildCommands = new HashSet<>();
  private final Set<String> antlr4LibraryBuildCommands = new HashSet<>();

  private final Set<String> conflictingFiles = new HashSet<>();

  public BuildTargetsCollector(
      String buckRoot,
      String outputDirectory,
      PlatformSelector selector,
      CommandRewriter rewriter) {
    this.buckRoot = buckRoot;
    this.outputDirectory = outputDirectory;
    this.selector = selector;
    this.rewriter = rewriter;
  }

  @VisibleForTesting
  Map<Path, Path> getSources() {
    return sources;
  }

  @VisibleForTesting
  Set<String> getPythonWheelUrls() {
    return pythonWheelUrls;
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

  public Set<String> getConflictingFiles() {
    return conflictingFiles;
  }

  /** @return a builder that contains all the target information necessary for building. */
  public BuildTargetsBuilder getBuilder(long startTime, ImmutableList<String> targets)
      throws BuilderException {
    collectBuildTargets(BuckCells.getCellMappings(), BuckQuery.getBuildTargetJson(targets));
    // Filter thrift libraries
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
    return new BuildTargetsBuilder(
        startTime,
        this.buckRoot,
        this.outputDirectory,
        targets,
        ImmutableMap.copyOf(this.sources),
        ImmutableSet.copyOf(this.unsupportedGeneratedSources),
        ImmutableSet.copyOf(this.pythonWheelUrls),
        ImmutableSet.copyOf(this.thriftLibraryTargets),
        ImmutableSet.copyOf(this.swigLibraryBuildCommands),
        ImmutableSet.copyOf(this.antlr4LibraryBuildCommands));
  }

  @VisibleForTesting
  void collectBuildTargets(ImmutableMap<String, String> cellMappings, JsonObject targetJsonMap) {
    // The first pass collects python_library that refers to a remote python_file
    for (Map.Entry<String, JsonElement> entry : targetJsonMap.entrySet()) {
      JsonObject targetJsonObject = entry.getValue().getAsJsonObject();
      addRemotePythonFiles(targetJsonObject, targetJsonMap);
    }
    // The second pass parses all normal targets and remote_file target with version information
    // collected above.
    for (Map.Entry<String, JsonElement> entry : targetJsonMap.entrySet()) {
      String buildTargetName = entry.getKey();
      String cellPath = BuckCells.getCellPath(buildTargetName, cellMappings);
      JsonObject targetJsonObject = entry.getValue().getAsJsonObject();
      collectBuildTarget(targetJsonObject, cellPath, buildTargetName);
    }
  }

  private void collectBuildTarget(
      JsonObject targetJsonObject, @Nullable String cellPath, String buildTargetName) {
    String type = targetJsonObject.get("buck.type").getAsString();
    switch (type) {
      case "python_binary":
      case "python_library":
      case "python_test":
        PythonTarget pythonTarget =
            PythonTarget.parse(
                cellPath, this.buckRoot, this.outputDirectory, this.selector, targetJsonObject);
        if (pythonTarget != null) {
          pythonTarget.getSources().forEach(this::addSourceMapping);
          pythonTarget
              .getUnsupportedGeneratedSources()
              .forEach(this::addUnsupportedGeneratedSource);
        }
        return;
      case "genrule":
        // Thrift library targets have genrule rule type.
        ThriftLibraryTarget thriftLibraryTarget =
            ThriftLibraryTarget.parse(cellPath, this.buckRoot, this.rewriter, targetJsonObject);
        if (thriftLibraryTarget != null) {
          this.thriftLibraryTargets.add(thriftLibraryTarget);
          return;
        }
        String antlr4LibraryCommand =
            Antlr4LibraryTarget.parseCommand(
                cellPath, this.buckRoot, this.outputDirectory, this.rewriter, targetJsonObject);
        if (antlr4LibraryCommand != null) {
          this.antlr4LibraryBuildCommands.add(antlr4LibraryCommand);
        }
        return;
      case "cxx_genrule":
        // Swig library targets have cxx_genrule rule type.
        String swigLibraryCommand =
            SwigLibraryTarget.parseCommand(
                cellPath, this.buckRoot, this.outputDirectory, this.rewriter, targetJsonObject);
        if (swigLibraryCommand != null) {
          this.swigLibraryBuildCommands.add(swigLibraryCommand);
        }
        return;
      case "remote_file":
        if (this.requiredRemoteFiles.contains(buildTargetName)) {
          this.pythonWheelUrls.add(targetJsonObject.get("url").getAsString());
        }
        return;
      default:
    }
  }

  private void addSourceMapping(Path outputPath, Path sourcePath) {
    Path existingSourcePath = this.sources.get(outputPath);
    if (existingSourcePath != null && !existingSourcePath.equals(sourcePath)) {
      this.conflictingFiles.add(
          Paths.get(this.outputDirectory).relativize(outputPath).normalize().toString());
      return;
    }
    this.sources.put(outputPath, sourcePath);
  }

  private void addUnsupportedGeneratedSource(String generatedSourcePath) {
    unsupportedGeneratedSources.add(generatedSourcePath);
  }

  private void addRemotePythonFiles(JsonObject targetJsonObject, JsonObject targetJsonMap) {
    if (!targetJsonObject.get("buck.type").getAsString().equals("python_library")
        || targetJsonObject.get("deps") != null) {
      // remote_file's corresponding top level python_library must not have deps field.
      return;
    }
    JsonArray platformDependenciesField = targetJsonObject.getAsJsonArray("platform_deps");
    if (platformDependenciesField == null) {
      return;
    }
    String versionedSuffix =
        this.selector.getSupportedPlatformDependency(platformDependenciesField);
    if (versionedSuffix == null) {
      return;
    }
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonObject versionedTarget = targetJsonMap.getAsJsonObject("//" + basePath + versionedSuffix);
    if (versionedTarget == null) {
      return;
    }
    String wheelSuffix =
        this.selector.getSupportedPlatformDependency(versionedTarget.get("platform_deps"));
    if (wheelSuffix == null) {
      return;
    }
    this.requiredRemoteFiles.add("//" + basePath + wheelSuffix + "-remote");
  }
}
