package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;

final class SwigLibraryTarget {

  static @Nullable String parseCommand(
      String cellPath,
      String buckRoot,
      String outputDirectory,
      CommandRewriter rewriter,
      JsonObject targetJsonObject) {
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
    return rewriter.rewriteSwigLibraryBuildCommand(command, outputDirectory, sources);
  }
}
