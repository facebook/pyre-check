package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.SimpleLogger;
import com.facebook.buck_project_builder.cache.BuilderCache;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.commons.io.FileUtils;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.StreamSupport;

public final class ThriftLibraryTarget implements BuildTarget {

  private static final Pattern BASE_MODULE_PATH_PATTERN =
      Pattern.compile("(?<=/gen-py/).*(?=/ttypes.pyi)");

  private final String command;
  private final String baseModulePath;
  private final List<String> sources;

  public ThriftLibraryTarget(String command, String baseModulePath, List<String> sources) {
    this.command = command;
    this.baseModulePath = baseModulePath;
    this.sources = sources;
  }

  /** Constructor for Gson. */
  private ThriftLibraryTarget() {
    this("", "", ImmutableList.of());
  }

  @VisibleForTesting
  static @Nullable String extractBaseModulePath(String command) {
    Matcher matcher = BASE_MODULE_PATH_PATTERN.matcher(command);
    return matcher.find() ? matcher.group(0) : null;
  }

  static @Nullable ThriftLibraryTarget parse(
      @Nullable String cellPath, String buckRoot, JsonObject targetJsonObject) {
    JsonElement labelsField = targetJsonObject.get("labels");
    if (labelsField == null) {
      return null;
    }
    JsonArray labelsJson = labelsField.getAsJsonArray();
    boolean isThriftLibraryTarget =
        StreamSupport.stream(labelsJson.spliterator(), false)
            .anyMatch(label -> label.getAsString().matches("thrift_library=py(.*)/compile"));
    if (!isThriftLibraryTarget) {
      return null;
    }
    JsonElement commandField = targetJsonObject.get("cmd");
    if (commandField == null) {
      return null;
    }
    String command = commandField.getAsString();
    String baseModulePath = extractBaseModulePath(command);
    if (baseModulePath == null) {
      return null;
    }
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonArray sourcesField = targetJsonObject.get("srcs").getAsJsonArray();
    ImmutableList<String> sources =
        GeneratedBuildRuleRunner.buildSources(cellPath, buckRoot, basePath, sourcesField);
    if (sources == null) {
      return null;
    }
    // Replace buck cmd macro with predefined values.
    command =
        command
            .replace("$(exe //thrift/compiler:thrift)", "thrift")
            .replace(
                "$(location //thrift/compiler/generate/templates:templates)",
                "thrift/compiler/generate/templates")
            .replaceFirst("-I \\$\\(location .*\\)", "-I .")
            .replace("-o \"$OUT\"", "-out " + BuilderCache.THRIFT_CACHE_PATH)
            .replace("\"$SRCS\"", String.join(" ", sources))
            .replaceFirst(" &&.*", "");
    return new ThriftLibraryTarget(command, baseModulePath, sources);
  }

  @Override
  public void addToBuilder(BuildTargetsBuilder builder) {
    builder.addThriftLibraryBuildCommand(this.command);
  }

  @Override
  public String toString() {
    return String.format(
        "{command=%s, baseModulePath=%s, sources=%s}", command, baseModulePath, sources);
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
    return command.equals(thriftLibraryTarget.command)
        && baseModulePath.equals(thriftLibraryTarget.baseModulePath)
        && sources.equals(thriftLibraryTarget.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(command, baseModulePath, sources);
  }
}
