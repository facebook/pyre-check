package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.FileSystem;
import com.google.common.collect.ImmutableMap;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;

public final class PythonTarget implements BuildTarget {

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

  static @Nullable PythonTarget parse(String ruleName, JsonObject targetJsonObject) {
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonElement sourcesField = targetJsonObject.get("srcs");
    // Ignore any target that does not have srcs
    if (sourcesField == null) {
      return null;
    }
    ImmutableMap.Builder<String, String> sourcesBuilder = ImmutableMap.builder();
    if (sourcesField.isJsonObject()) {
      // Parse srcs of the form { "a": "b", "c": "d" } into Java Map.
      for (Map.Entry<String, JsonElement> entry : sourcesField.getAsJsonObject().entrySet()) {
        sourcesBuilder.put(entry.getKey(), entry.getValue().getAsString());
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
    JsonElement baseModuleField = targetJsonObject.get("base_module");
    String baseModule = baseModuleField == null ? null : baseModuleField.getAsString();
    return new PythonTarget(ruleName, basePath, baseModule, sourcesBuilder.build());
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
