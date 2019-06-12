package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.FileSystem;
import com.google.common.collect.ImmutableMap;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Logger;

public final class PythonTarget implements BuildTarget {

  private static final Logger LOGGER = Logger.getGlobal();

  private final String ruleName;
  private final String basePath;
  private final @Nullable String baseModule;
  private final ImmutableMap<String, String> sources;

  PythonTarget(
      String ruleName,
      String basePath,
      @Nullable String baseModule,
      ImmutableMap<String, String> sources) {
    this.ruleName = ruleName;
    this.basePath = basePath;
    this.baseModule = baseModule;
    this.sources = sources;
  }

  private static ImmutableMap<String, String> parseSources(JsonElement sourcesField) {
    ImmutableMap.Builder<String, String> sourcesBuilder = ImmutableMap.builder();
    if (sourcesField.isJsonObject()) {
      // Parse srcs of the form { "a": "b", "c": "d" } into Java Map.
      for (Map.Entry<String, JsonElement> entry : sourcesField.getAsJsonObject().entrySet()) {
        sourcesBuilder.put(entry.getValue().getAsString(), entry.getKey());
      }
    } else if (sourcesField.isJsonArray()) {
      // Parse srcs of the form ["a", "b", "c"] into { "a": "a", "b": "b", "c": "c" }
      for (JsonElement sourceElement : sourcesField.getAsJsonArray()) {
        String source = sourceElement.getAsString();
        sourcesBuilder.put(source, source);
      }
    } else {
      throw new Error(
          "The srcs field should either be map or list. "
              + "It's likely that buck changes it's behavior. "
              + "Unexpected srcs field: "
              + sourcesField);
    }
    return sourcesBuilder.build();
  }

  private static @Nullable ImmutableMap<String, String> parseVersionedSources(
      JsonArray versionedSourcesArray) {
    // Pick the highest possible python minor version for consistency.
    int highestPythonMinorVersion = -1;
    JsonObject sourceSet = null;
    for (JsonElement versionedSourceElement : versionedSourcesArray) {
      JsonArray versionedSourcePair = versionedSourceElement.getAsJsonArray();
      JsonObject versions = versionedSourcePair.get(0).getAsJsonObject();
      JsonElement pythonVersionValue =
          versions.get("//third-party-buck/platform007/build/python:__project__");
      if (pythonVersionValue == null) {
        // Ignore versions on unsupported platforms.
        continue;
      }
      String pythonVersion = pythonVersionValue.getAsString();
      if (!pythonVersion.startsWith("3")) {
        // Only python 3 is supported.
        LOGGER.finest("Only python3 is supported. Unsupported version: " + pythonVersionValue);
        continue;
      }
      String[] versionParts = pythonVersion.split("\\.");
      int pythonMinorVersion;
      if (versionParts.length < 2) {
        pythonMinorVersion = 0;
      } else {
        pythonMinorVersion = Integer.parseInt(versionParts[1]);
      }
      if (pythonMinorVersion > highestPythonMinorVersion) {
        highestPythonMinorVersion = pythonMinorVersion;
        sourceSet = versionedSourcePair.get(1).getAsJsonObject();
      }
    }
    if (sourceSet == null) {
      return null;
    }
    ImmutableMap.Builder<String, String> sourcesBuilder = ImmutableMap.builder();
    for (Map.Entry<String, JsonElement> entry : sourceSet.entrySet()) {
      // Versioned sources entries: key => output, value => source, so it's inverted here.
      sourcesBuilder.put(entry.getValue().getAsString(), entry.getKey());
    }
    return sourcesBuilder.build();
  }

  static @Nullable PythonTarget parse(String ruleName, JsonObject targetJsonObject) {
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    ImmutableMap<String, String> sources;
    // Ignore any target that does not have srcs or versioned_srcs
    JsonElement sourcesField = targetJsonObject.get("srcs");
    if (sourcesField != null) {
      sources = parseSources(sourcesField);
    } else {
      JsonElement versionedSourcesField = targetJsonObject.get("versioned_srcs");
      sources =
          versionedSourcesField == null
              ? null
              : parseVersionedSources(versionedSourcesField.getAsJsonArray());
    }
    if (sources == null) {
      return null;
    }
    JsonElement baseModuleField = targetJsonObject.get("base_module");
    String baseModule = baseModuleField == null ? null : baseModuleField.getAsString();
    return new PythonTarget(ruleName, basePath, baseModule, sources);
  }

  @Override
  public void build(String buckRoot, String outputDirectory) {
    String sourceDirectory = Paths.get(buckRoot, basePath).toString();
    String outputBasePath =
        baseModule == null ? basePath : Paths.get(".", baseModule.split("\\.")).toString();
    outputDirectory = Paths.get(outputDirectory, outputBasePath).toString();
    Map<String, String> sourceMapping =
        FileSystem.resolveSourceMapping(sourceDirectory, outputDirectory, sources);
    for (Map.Entry<String, String> entry : sourceMapping.entrySet()) {
      FileSystem.addSymbolicLink(Paths.get(entry.getValue()), Paths.get(entry.getKey()));
    }
  }

  @Override
  public String toString() {
    return String.format(
        "{ruleName=%s, basePath=%s, baseModule=%s, sources=%s}",
        ruleName, basePath, baseModule, sources);
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (this == other) {
      return true;
    }
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    PythonTarget pythonTarget = (PythonTarget) other;
    return ruleName.equals(pythonTarget.ruleName)
        && basePath.equals(pythonTarget.basePath)
        && Objects.equals(baseModule, pythonTarget.baseModule)
        && sources.equals(pythonTarget.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ruleName, basePath, baseModule, sources);
  }
}
