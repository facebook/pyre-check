package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.util.Objects;

public final class SwigLibraryTarget implements BuildTarget {

  private final String command;
  private final ImmutableList<String> sources;

  SwigLibraryTarget(String command, ImmutableList<String> sources) {
    this.command = command;
    this.sources = sources;
  }

  static @Nullable SwigLibraryTarget parse(
      String cellPath, String buckRoot, JsonObject targetJsonObject) {
    if (!targetJsonObject.get("name").getAsString().endsWith("-py-gen")) {
      // Generated swig library rule names always end with -py-gen. We use this to identify swig
      // library rules.
      return null;
    }
    String command = targetJsonObject.get("cmd").getAsString();
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonArray sourcesField = targetJsonObject.get("srcs").getAsJsonArray();
    ImmutableList<String> sources =
        GeneratedBuildRuleRunner.buildSources(cellPath, buckRoot, basePath, sourcesField);
    if (sources == null) {
      return null;
    }
    return new SwigLibraryTarget(command, sources);
  }

  @Override
  public void addToBuilder(BuildTargetsBuilder builder) {
    String outputDirectory = builder.getOutputDirectory();
    String builderCommand =
        this.command
            .replaceFirst(
                "mkdir .+\\$\\(exe //third-party-buck/platform007/tools/swig:bin/swig\\)", "")
            .replaceFirst(
                " -I- -I.+$",
                String.format(
                    " -I- -I. -outdir %s -o %s -oh %s %s",
                    outputDirectory,
                    outputDirectory + "/temp.cc",
                    outputDirectory + "/temp.h",
                    String.join(" ", sources)))
            .replaceAll("'", "");
    builder.addSwigLibraryBuildCommand(builderCommand);
  }

  @Override
  public String toString() {
    return String.format("{command=%s, sources=%s}", command, sources);
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
    return command.equals(swigLibraryTarget.command) && sources.equals(swigLibraryTarget.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(command, sources);
  }
}
