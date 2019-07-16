package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.util.Objects;

public final class Antlr4LibraryTarget implements BuildTarget {

  private final @Nullable String cellPath;
  private final String basePath;
  private final String command;
  private final ImmutableList<String> sources;

  Antlr4LibraryTarget(
      @Nullable String cellPath, String basePath, String command, ImmutableList<String> sources) {
    this.cellPath = cellPath;
    this.basePath = basePath;
    this.command = command;
    this.sources = sources;
  }

  static @Nullable Antlr4LibraryTarget parse(String cellPath, JsonObject targetJsonObject) {
    JsonElement commandField = targetJsonObject.get("cmd");
    if (commandField == null) {
      return null;
    }
    String command = commandField.getAsString();
    if (!command.contains("--antlr4_command=") || !command.contains("--language=Python")) {
      // Only build generated ANTLR4 python parser.
      return null;
    }
    JsonArray sourcesField = targetJsonObject.get("srcs").getAsJsonArray();
    ImmutableList.Builder<String> sourcesBuilder = ImmutableList.builder();
    for (JsonElement sourceElement : sourcesField.getAsJsonArray()) {
      sourcesBuilder.add(sourceElement.getAsString());
    }
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    return new Antlr4LibraryTarget(cellPath, basePath, command, sourcesBuilder.build());
  }

  @Override
  public void addToBuilder(BuildTargetsBuilder builder) {
    String basePathPrefixedSources =
        GeneratedBuildRuleRunner.getBasePathPrefixedSources(
            this.cellPath, this.basePath, this.sources);
    String outputDirectory = builder.getOutputDirectory();
    String builderCommand =
        this.command
            .replaceFirst("mkdir .+\\$\\(exe //tools/antlr4:antlr4_wrapper\\)", "")
            .replace(
                "--install_dir=\"$OUT\"", String.format("--install_dir=\"%s\"", outputDirectory))
            .replace("--antlr4_command=$(location //tools/antlr4:antlr4)", "")
            .replaceFirst("--grammars .+$", "--grammars " + basePathPrefixedSources);
    builder.addAntlr4LibraryBuildCommand(builderCommand);
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
    Antlr4LibraryTarget antlr4LibraryTarget = (Antlr4LibraryTarget) other;
    return Objects.equals(cellPath, antlr4LibraryTarget.cellPath)
        && basePath.equals(antlr4LibraryTarget.basePath)
        && command.equals(antlr4LibraryTarget.command)
        && sources.equals(antlr4LibraryTarget.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(cellPath, basePath, command, sources);
  }
}
