package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.util.Objects;
import java.util.stream.StreamSupport;

public final class ThriftLibraryTarget implements BuildTarget {

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

  @Override
  public void build(String buckRoot, String outputDirectory) {}

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
