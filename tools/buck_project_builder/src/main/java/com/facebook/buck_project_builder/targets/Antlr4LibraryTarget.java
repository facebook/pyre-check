package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.nio.file.Paths;
import java.util.Objects;

public final class Antlr4LibraryTarget implements BuildTarget {

  private final String command;
  private final String basePath;
  private final ImmutableList<String> sources;

  Antlr4LibraryTarget(String command, String basePath, ImmutableList<String> sources) {
    this.command = command;
    this.basePath = basePath;
    this.sources = sources;
  }

  static @Nullable Antlr4LibraryTarget parse(
      @Nullable String cellPath, String buckRoot, JsonObject targetJsonObject) {
    JsonElement commandField = targetJsonObject.get("cmd");
    if (commandField == null) {
      return null;
    }
    String command = commandField.getAsString();
    if (!command.contains("--antlr4_command=") || !command.contains("--language=Python")) {
      // Only build generated ANTLR4 python parser.
      return null;
    }
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonArray sourcesField = targetJsonObject.get("srcs").getAsJsonArray();
    ImmutableList<String> sources =
        GeneratedBuildRuleRunner.buildSources(cellPath, buckRoot, basePath, sourcesField);
    if (sources == null) {
      return null;
    }
    return new Antlr4LibraryTarget(command, basePath, sources);
  }

  @Override
  public void addToBuilder(BuildTargetsBuilder builder) {
    String outputDirectory = builder.getOutputDirectory();
    String builderCommand =
        this.command
            .replaceFirst("mkdir .+\\$\\(exe //tools/antlr4:antlr4_wrapper\\)", "")
            .replace(
                "--install_dir=\"$OUT\"",
                String.format("--install_dir=\"%s\"", Paths.get(outputDirectory, basePath)))
            .replace("--antlr4_command=$(location //tools/antlr4:antlr4)", "")
            .replaceFirst("--grammars .+$", "--grammars " + String.join(" ", sources));
    builder.addAntlr4LibraryBuildCommand(builderCommand);
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
    Antlr4LibraryTarget antlr4LibraryTarget = (Antlr4LibraryTarget) other;
    return command.equals(antlr4LibraryTarget.command)
        && sources.equals(antlr4LibraryTarget.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(command, sources);
  }
}
