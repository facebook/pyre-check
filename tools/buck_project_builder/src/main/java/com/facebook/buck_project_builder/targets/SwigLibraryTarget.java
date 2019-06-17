package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.io.IOException;
import java.util.Objects;
import java.util.logging.Logger;

public final class SwigLibraryTarget implements BuildTarget {

  private static final Logger LOGGER = Logger.getGlobal();

  private final @Nullable String cellPath;
  private final String basePath;
  private final String command;
  private final ImmutableList<String> sources;

  SwigLibraryTarget(
          @Nullable String cellPath, String basePath, String command, ImmutableList<String> sources) {
    this.cellPath = cellPath;
    this.basePath = basePath;
    this.command = command;
    this.sources = sources;
  }

  static @Nullable SwigLibraryTarget parse(String cellPath, JsonObject targetJsonObject) {
    if (!targetJsonObject.get("name").getAsString().endsWith("-py-gen")) {
      // Generated swig library rule names always end with -py-gen. We use this to identify swig
      // library rules.
      return null;
    }
    String command = targetJsonObject.get("cmd").getAsString();
    JsonArray sourcesField = targetJsonObject.get("srcs").getAsJsonArray();
    ImmutableList.Builder<String> sourcesBuilder = ImmutableList.builder();
    for (JsonElement sourceElement : sourcesField.getAsJsonArray()) {
      sourcesBuilder.add(sourceElement.getAsString());
    }
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    return new SwigLibraryTarget(cellPath, basePath, command, sourcesBuilder.build());
  }

  @Override
  public void build(String buckRoot, String outputDirectory) {
    String basePathPrefixedSources =
        GeneratedBuildRuleRunner.getBasePathPrefixedSources(
            this.cellPath, this.basePath, this.sources);
    String builderCommand =
        this.command
            .replaceFirst(
                "mkdir .+\\$\\(exe //third-party-buck/platform007/tools/swig:bin/swig\\)",
                "buck run //third-party-buck/platform007/tools/swig:bin/swig --")
            .replaceFirst(
                " -I- -I.+$",
                String.format(
                    " -I- -I. -outdir %s -o %s -oh %s %s",
                    outputDirectory,
                    outputDirectory + "/temp.cc",
                    outputDirectory + "/temp.h",
                    basePathPrefixedSources))
            .replaceAll("'", "");
    try {
      GeneratedBuildRuleRunner.runBuilderCommand(builderCommand, buckRoot);
    } catch (IOException exception) {
      LOGGER.warning("IOException during thrift stub generation: " + exception.getMessage());
    }
  }

  @Override
  public String toString() {
    return String.format(
        "{cellPath=%s, basePath=%s, command=%s, sources=%s}", cellPath, basePath, command, sources);
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (this == other) {
      return true;
    }
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    SwigLibraryTarget swigLibraryTarget = (SwigLibraryTarget) other;
    return Objects.equals(cellPath, swigLibraryTarget.cellPath)
        && basePath.equals(swigLibraryTarget.basePath)
        && command.equals(swigLibraryTarget.command)
        && sources.equals(swigLibraryTarget.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(cellPath, basePath, command, sources);
  }
}
