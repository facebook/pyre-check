package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;

public final class PythonTarget {

  private final ImmutableMap<Path, Path> sources;
  private final ImmutableSet<String> unsupportedGeneratedSources;

  PythonTarget(ImmutableMap<Path, Path> sources, ImmutableSet<String> unsupportedGeneratedSources) {
    this.sources = sources;
    this.unsupportedGeneratedSources = unsupportedGeneratedSources;
  }

  PythonTarget(ImmutableMap<Path, Path> sources) {
    this(sources, ImmutableSet.of());
  }

  private static void addSources(
      JsonElement sourcesField,
      String sourceRoot,
      String outputRoot,
      ImmutableMap.Builder<Path, Path> sourcesBuilder,
      ImmutableSet.Builder<String> unsupportedGeneratedSourcesBuilder) {
    if (sourcesField.isJsonObject()) {
      // Parse srcs of the form { "a": "b", "c": "d" } into Java Map.
      for (Map.Entry<String, JsonElement> entry : sourcesField.getAsJsonObject().entrySet()) {
        String source = entry.getValue().getAsString();
        String outputFile = entry.getKey();
        if (source.contains("//") || source.startsWith(":")) {
          if (outputFile.endsWith(".py") || outputFile.endsWith(".pyi")) {
            // We only care about unsupported generated python code.
            unsupportedGeneratedSourcesBuilder.add(Paths.get(outputRoot, outputFile).toString());
          }
        } else {
          sourcesBuilder.put(Paths.get(outputRoot, outputFile), Paths.get(sourceRoot, source));
        }
      }
    } else if (sourcesField.isJsonArray()) {
      // Parse srcs of the form ["a", "b", "c"] into { "a": "a", "b": "b", "c": "c" }
      for (JsonElement sourceElement : sourcesField.getAsJsonArray()) {
        String source = sourceElement.getAsString();
        sourcesBuilder.put(Paths.get(outputRoot, source), Paths.get(sourceRoot, source));
      }
    } else {
      throw new Error(
          "The srcs field should either be map or list. "
              + "It's likely that buck changes it's behavior. "
              + "Unexpected srcs field: "
              + sourcesField);
    }
  }

  private static void addVersionedSources(
      JsonArray versionedSourcesArray,
      String sourceRoot,
      String outputRoot,
      PlatformSelector selector,
      ImmutableMap.Builder<Path, Path> sourcesBuilder,
      ImmutableSet.Builder<String> unsupportedGeneratedSourcesBuilder) {
    JsonElement sourceSet = selector.getSupportedVersionedSources(versionedSourcesArray);
    if (sourceSet == null) {
      return;
    }
    addSources(
        sourceSet, sourceRoot, outputRoot, sourcesBuilder, unsupportedGeneratedSourcesBuilder);
  }

  private static void addPlatformSources(
      JsonArray platformSourcesArray,
      String sourceRoot,
      String outputRoot,
      ImmutableMap.Builder<Path, Path> sourcesBuilder,
      ImmutableSet.Builder<String> unsupportedGeneratedSourcesBuilder) {
    for (JsonElement platformSourceElement : platformSourcesArray) {
      JsonArray platformSourcePair = platformSourceElement.getAsJsonArray();
      if (platformSourcePair.get(0).getAsString().equals("py3")) {
        addSources(
            platformSourcePair.get(1),
            sourceRoot,
            outputRoot,
            sourcesBuilder,
            unsupportedGeneratedSourcesBuilder);
      }
    }
  }

  static @Nullable PythonTarget parse(
      @Nullable String cellPath,
      String buckRoot,
      String outputDirectory,
      PlatformSelector platformSelector,
      JsonObject targetJsonObject) {
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    String sourceRoot = Paths.get(cellPath != null ? cellPath : buckRoot, basePath).toString();
    JsonElement baseModuleField = targetJsonObject.get("base_module");
    String outputBasePath =
        baseModuleField == null
            ? basePath
            : Paths.get(".", baseModuleField.getAsString().split("\\.")).toString();
    String outputRoot = Paths.get(outputDirectory, outputBasePath).toString();
    ImmutableMap.Builder<Path, Path> sourcesBuilder = ImmutableMap.builder();
    ImmutableSet.Builder<String> unsupportedGeneratedSourcesBuilder = ImmutableSet.builder();
    // Both `srcs` and `versioned_srcs` might be present in a target.
    JsonElement sourcesField = targetJsonObject.get("srcs");
    if (sourcesField != null) {
      addSources(
          sourcesField, sourceRoot, outputRoot, sourcesBuilder, unsupportedGeneratedSourcesBuilder);
    }
    JsonElement versionedSourcesField = targetJsonObject.get("versioned_srcs");
    if (versionedSourcesField != null) {
      addVersionedSources(
          versionedSourcesField.getAsJsonArray(),
          sourceRoot,
          outputRoot,
          platformSelector,
          sourcesBuilder,
          unsupportedGeneratedSourcesBuilder);
    }
    JsonElement platformSourcesField = targetJsonObject.get("platform_srcs");
    if (platformSourcesField != null) {
      addPlatformSources(
          platformSourcesField.getAsJsonArray(),
          sourceRoot,
          outputRoot,
          sourcesBuilder,
          unsupportedGeneratedSourcesBuilder);
    }
    ImmutableMap<Path, Path> sources = sourcesBuilder.build();
    ImmutableSet<String> unsupportedGeneratedSources = unsupportedGeneratedSourcesBuilder.build();
    // Ignore any target that contains no sources.
    if (sources.isEmpty() && unsupportedGeneratedSources.isEmpty()) {
      return null;
    }
    return new PythonTarget(sources, unsupportedGeneratedSources);
  }

  public ImmutableMap<Path, Path> getSources() {
    return sources;
  }

  public ImmutableSet<String> getUnsupportedGeneratedSources() {
    return unsupportedGeneratedSources;
  }

  @Override
  public String toString() {
    return String.format(
        "{sources=%s, unsupportedGeneratedSources=%s}", sources, unsupportedGeneratedSources);
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
    return sources.equals(pythonTarget.sources)
        && unsupportedGeneratedSources.equals(pythonTarget.unsupportedGeneratedSources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sources, unsupportedGeneratedSources);
  }
}
