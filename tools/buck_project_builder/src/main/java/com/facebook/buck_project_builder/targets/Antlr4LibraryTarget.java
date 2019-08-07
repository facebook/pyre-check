package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;

final class Antlr4LibraryTarget {

  static @Nullable String parseCommand(
      @Nullable String cellPath,
      String buckRoot,
      String outputDirectory,
      CommandRewriter rewriter,
      JsonObject targetJsonObject) {
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
    return rewriter.rewriteAntlr4LibraryBuildCommand(command, basePath, outputDirectory, sources);
  }
}
