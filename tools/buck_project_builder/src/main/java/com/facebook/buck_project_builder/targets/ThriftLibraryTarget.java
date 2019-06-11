package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.commons.io.FileUtils;

import javax.annotation.Nullable;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public final class ThriftLibraryTarget implements BuildTarget {

  private static final Logger LOGGER = Logger.getGlobal();

  private final String basePath;
  private final String command;
  private final ImmutableList<String> sources;

  ThriftLibraryTarget(String basePath, String command, ImmutableList<String> sources) {
    this.basePath = basePath;
    this.command = command;
    this.sources = sources;
  }

  static @Nullable ThriftLibraryTarget parse(JsonObject targetJsonObject) {
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonElement labelsField = targetJsonObject.get("labels");
    if (labelsField == null) {
      return null;
    }
    JsonArray labelsJson = labelsField.getAsJsonArray();
    boolean isThriftLibraryTarget =
        StreamSupport.stream(labelsJson.spliterator(), false)
            .anyMatch(
                labelJson -> {
                  String label = labelJson.getAsString();
                  return label.equals("thrift_library=py/compile")
                      || label.equals("thrift_library=pyi/compile");
                });
    if (!isThriftLibraryTarget) {
      return null;
    }
    JsonElement commandField = targetJsonObject.get("cmd");
    if (commandField == null) {
      return null;
    }
    String command = commandField.getAsString();
    JsonArray sourcesField = targetJsonObject.get("srcs").getAsJsonArray();
    ImmutableList.Builder<String> sourcesBuilder = ImmutableList.builder();
    for (JsonElement sourceElement : sourcesField.getAsJsonArray()) {
      sourcesBuilder.add(sourceElement.getAsString());
    }
    return new ThriftLibraryTarget(basePath, command, sourcesBuilder.build());
  }

  private void buildThriftStubs(String buckRoot, String outputDirectory) throws IOException {
    String basePathPrefixedSources =
        this.sources.stream()
            .map(source -> Paths.get(this.basePath, source).toString())
            .collect(Collectors.joining(" "));
    // Replace buck cmd macro with predefined values.
    String builderCommand =
        this.command
            .replace("$(exe //thrift/compiler:thrift)", "thrift")
            .replace(
                "$(location //thrift/compiler/generate/templates:templates)",
                "thrift/compiler/generate/templates")
            .replaceFirst("-I \\$\\(location .*\\)", "-I .")
            .replace("\"$OUT\"", outputDirectory)
            .replace("\"$SRCS\"", basePathPrefixedSources)
            .replaceFirst(" &&.*", "");
    /*
     * We log the errors of thrift building failure but do not stop the building workflow.
     * We do not throw error because builds will be parallelized so the error can never be caught at main thread.
     */
    try (InputStream thriftErrorStream =
        // Run the command in replaced cmd directly.
        Runtime.getRuntime()
            .exec(
                builderCommand,
                /* environment variables */ null,
                /* working directory */ new File(buckRoot))
            .getErrorStream()) {
      new BufferedReader(new InputStreamReader(thriftErrorStream))
          .lines()
          .forEach(thriftErrorLine -> LOGGER.warning("[thrift-error]: " + thriftErrorLine));
    }
  }

  /**
   * Thrift puts all generated python code at [supplied output directory]/gen-py, so we must copy
   * all python files by ourselves. This function copies all necessary python files for type
   * checking to desired location.
   */
  private void copyThriftStubs(String temporaryThriftOutputDirectory, String outputDirectory)
      throws IOException {
    File generatedPythonCodeDirectory =
        Paths.get(temporaryThriftOutputDirectory, "gen-py").toFile();
    if (generatedPythonCodeDirectory.exists()) {
      FileUtils.copyDirectory(
          generatedPythonCodeDirectory,
          new File(outputDirectory),
          file -> {
            if (file.isDirectory()) {
              return true;
            }
            /*
             * Include only py and pyi files, but not __init__.py(i) files.
             * An init.py file is needed in each python2 directory, but this restriction was removed in python3.
             * __init__.py(i) files are generated to make python2 happy and pyre doesn't need them.
             */
            return file.getPath().matches("((?!__init__).)+\\.py(i?)");
          });
    } else {
      LOGGER.warning("Failed to compile following thrift sources: " + sources);
    }
  }

  @Override
  public void build(String buckRoot, String outputDirectory) {
    try {
      String temporaryThriftOutputDirectory = Files.createTempDirectory("thrift_temp_").toString();
      buildThriftStubs(buckRoot, temporaryThriftOutputDirectory);
      copyThriftStubs(temporaryThriftOutputDirectory, outputDirectory);
      new File(temporaryThriftOutputDirectory).delete();
    } catch (IOException exception) {
      LOGGER.warning("IOException during thrift stub generation: " + exception.getMessage());
    }
  }

  @Override
  public String toString() {
    return String.format("{basePath=%s, command=%s, sources=%s}", basePath, command, sources);
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (this == other) {
      return true;
    }
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    ThriftLibraryTarget thriftLibraryTarget = (ThriftLibraryTarget) other;
    return basePath.equals(thriftLibraryTarget.basePath)
        && command.equals(thriftLibraryTarget.command)
        && sources.equals(thriftLibraryTarget.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(basePath, command, sources);
  }
}
