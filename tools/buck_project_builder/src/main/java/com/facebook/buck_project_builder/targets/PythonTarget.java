package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.FileSystem;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;

public final class PythonTarget implements BuildTarget {

  private static final String[] SUPPORTED_PLATFORMS = {
    "//third-party-buck/platform007/build/python:__project__",
    "//third-party-buck/platform007/build/python:python"
  };
  private static final String[] SUPPORTED_PYTHON_VERSIONS = {
    "3.6", "3.7", "ouroboros.3.6", "cinder.3.6"
  };

  private final @Nullable String cellPath;
  private final String basePath;
  private final @Nullable String baseModule;
  private final ImmutableMap<String, String> sources;
  private final ImmutableSet<String> unsupportedGeneratedSources;

  PythonTarget(
      @Nullable String cellPath,
      String basePath,
      @Nullable String baseModule,
      ImmutableMap<String, String> sources,
      ImmutableSet<String> unsupportedGeneratedSources) {
    this.cellPath = cellPath;
    this.basePath = basePath;
    this.baseModule = baseModule;
    this.sources = sources;
    this.unsupportedGeneratedSources = unsupportedGeneratedSources;
  }

  PythonTarget(String basePath, @Nullable String baseModule, ImmutableMap<String, String> sources) {
    this(null, basePath, baseModule, sources, ImmutableSet.of());
  }

  private static void addSources(
      JsonElement sourcesField,
      ImmutableMap.Builder<String, String> sourcesBuilder,
      ImmutableSet.Builder<String> unsupportedGeneratedSourcesBuilder) {
    if (sourcesField.isJsonObject()) {
      // Parse srcs of the form { "a": "b", "c": "d" } into Java Map.
      for (Map.Entry<String, JsonElement> entry : sourcesField.getAsJsonObject().entrySet()) {
        String source = entry.getValue().getAsString();
        String outputFile = entry.getKey();
        if (source.contains("//") || source.startsWith(":")) {
          if (outputFile.endsWith(".py") || outputFile.endsWith(".pyi")) {
            // We only care about unsupported generated python code.
            unsupportedGeneratedSourcesBuilder.add(outputFile);
          }
        } else {
          sourcesBuilder.put(source, outputFile);
        }
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
  }

  private static @Nullable JsonElement getSupportedVersionedSources(
      JsonArray versionedSourcesArray) {
    for (String supportedPlatform : SUPPORTED_PLATFORMS) {
      for (String supportedPythonVersion : SUPPORTED_PYTHON_VERSIONS) {
        for (JsonElement versionedSourceElement : versionedSourcesArray) {
          JsonArray versionedSourcePair = versionedSourceElement.getAsJsonArray();
          JsonObject versions = versionedSourcePair.get(0).getAsJsonObject();
          JsonElement pythonVersionValue = versions.get(supportedPlatform);
          if (pythonVersionValue == null) {
            continue;
          }
          String pythonVersion = pythonVersionValue.getAsString();
          if (!pythonVersion.equals(supportedPythonVersion)) {
            continue;
          }
          return versionedSourcePair.get(1);
        }
      }
    }
    return null;
  }

  private static void addVersionedSources(
      JsonArray versionedSourcesArray,
      ImmutableMap.Builder<String, String> sourcesBuilder,
      ImmutableSet.Builder<String> unsupportedGeneratedSourcesBuilder) {
    JsonElement sourceSet = getSupportedVersionedSources(versionedSourcesArray);
    if (sourceSet == null) {
      return;
    }
    addSources(sourceSet, sourcesBuilder, unsupportedGeneratedSourcesBuilder);
  }

  private static void addPlatformSources(
      JsonArray platformSourcesArray,
      ImmutableMap.Builder<String, String> sourcesBuilder,
      ImmutableSet.Builder<String> unsupportedGeneratedSourcesBuilder) {
    for (JsonElement platformSourceElement : platformSourcesArray) {
      JsonArray platformSourcePair = platformSourceElement.getAsJsonArray();
      if (platformSourcePair.get(0).getAsString().equals("py3")) {
        addSources(platformSourcePair.get(1), sourcesBuilder, unsupportedGeneratedSourcesBuilder);
      }
    }
  }

  static @Nullable PythonTarget parse(@Nullable String cellPath, JsonObject targetJsonObject) {
    ImmutableMap.Builder<String, String> sourcesBuilder = ImmutableMap.builder();
    ImmutableSet.Builder<String> unsupportedGeneratedSourcesBuilder = ImmutableSet.builder();
    // Both `srcs` and `versioned_srcs` might be present in a target.
    JsonElement sourcesField = targetJsonObject.get("srcs");
    if (sourcesField != null) {
      addSources(sourcesField, sourcesBuilder, unsupportedGeneratedSourcesBuilder);
    }
    JsonElement versionedSourcesField = targetJsonObject.get("versioned_srcs");
    if (versionedSourcesField != null) {
      addVersionedSources(
          versionedSourcesField.getAsJsonArray(),
          sourcesBuilder,
          unsupportedGeneratedSourcesBuilder);
    }
    JsonElement platformSourcesField = targetJsonObject.get("platform_srcs");
    if (platformSourcesField != null) {
      addPlatformSources(
          platformSourcesField.getAsJsonArray(),
          sourcesBuilder,
          unsupportedGeneratedSourcesBuilder);
    }
    ImmutableMap<String, String> sources = sourcesBuilder.build();
    ImmutableSet<String> unsupportedGeneratedSources = unsupportedGeneratedSourcesBuilder.build();
    // Ignore any target that contains no sources.
    if (sources.isEmpty() && unsupportedGeneratedSources.isEmpty()) {
      return null;
    }
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonElement baseModuleField = targetJsonObject.get("base_module");
    String baseModule = baseModuleField == null ? null : baseModuleField.getAsString();
    return new PythonTarget(cellPath, basePath, baseModule, sources, unsupportedGeneratedSources);
  }

  @Override
  public void addToBuilder(BuildTargetsBuilder builder) {
    String sourceDirectory =
        Paths.get(this.cellPath != null ? this.cellPath : builder.getBuckRoot(), basePath)
            .toString();
    String outputBasePath =
        baseModule == null ? basePath : Paths.get(".", baseModule.split("\\.")).toString();
    String outputDirectory = Paths.get(builder.getOutputDirectory(), outputBasePath).toString();
    Map<String, String> sourceMapping =
        FileSystem.resolveSourceMapping(sourceDirectory, outputDirectory, sources);
    for (Map.Entry<String, String> entry : sourceMapping.entrySet()) {
      builder.addSourceMapping(Paths.get(entry.getKey()), Paths.get(entry.getValue()));
    }
    for (String unsupportedGeneratedSource : unsupportedGeneratedSources) {
      builder.addUnsupportedGeneratedSource(
          Paths.get(outputDirectory, unsupportedGeneratedSource).toString());
    }
  }

  @Override
  public String toString() {
    return String.format(
        "{cellPath=%s, basePath=%s, baseModule=%s, sources=%s, unsupportedGeneratedSources=%s}",
        cellPath, basePath, baseModule, sources, unsupportedGeneratedSources);
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
    return Objects.equals(cellPath, pythonTarget.cellPath)
        && basePath.equals(pythonTarget.basePath)
        && Objects.equals(baseModule, pythonTarget.baseModule)
        && sources.equals(pythonTarget.sources)
        && unsupportedGeneratedSources.equals(pythonTarget.unsupportedGeneratedSources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(cellPath, basePath, baseModule, sources, unsupportedGeneratedSources);
  }
}
