package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.FileSystem;
import com.google.common.collect.ImmutableMap;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Objects;
import java.util.stream.StreamSupport;

final class PythonTarget {

  private final String ruleName;
  private final String basePath;
  private final ImmutableMap<String, String> sources;

  PythonTarget(String ruleName, String basePath, ImmutableMap<String, String> sources) {
    this.ruleName = ruleName;
    this.basePath = basePath;
    this.sources = sources;
  }

  static PythonTarget parse(String ruleName, JsonObject targetJsonObject) {
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonElement labelsField = targetJsonObject.get("labels");
    // exclude targets that has { labels: [ ..., "generated", ... ]}
    if (labelsField != null) {
      boolean containsGeneratedField =
          StreamSupport.stream(labelsField.getAsJsonArray().spliterator(), false)
              .anyMatch(labelElement -> labelElement.getAsString().equals("generated"));
      if (containsGeneratedField) {
        return null;
      }
    }
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
    return new PythonTarget(ruleName, basePath, sourcesBuilder.build());
  }

  public void build(String buckRoot, String outputDirectory) {
    outputDirectory = Paths.get(outputDirectory, basePath).toString();
    Map<String, String> sourceMapping =
        FileSystem.resolveSourceMapping(basePath, outputDirectory, sources);
    for (Map.Entry<String, String> entry : sourceMapping.entrySet()) {
      FileSystem.addSymbolicLink(Paths.get(entry.getValue()), Paths.get(buckRoot, entry.getKey()));
    }
  }

  @Override
  public String toString() {
    return String.format("{ruleName=%s, basePath=%s, sources=%s}", ruleName, basePath, sources);
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
        && sources.equals(pythonTarget.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ruleName, basePath, sources);
  }
}
