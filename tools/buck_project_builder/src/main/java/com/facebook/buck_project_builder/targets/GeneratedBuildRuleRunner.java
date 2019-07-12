package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.CommandLine;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

import javax.annotation.Nullable;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Paths;
import java.util.stream.Collectors;

final class GeneratedBuildRuleRunner {

  private GeneratedBuildRuleRunner() {}

  static String getBasePathPrefixedSources(
      @Nullable String cellPath, String basePath, ImmutableList<String> sources) {
    String cellAndBasePathPrefix =
        Paths.get(cellPath != null ? cellPath : ".", basePath).toString();
    return sources.stream()
        .map(source -> Paths.get(cellAndBasePathPrefix, source).toString())
        .collect(Collectors.joining(" "));
  }

  static void runBuilderCommand(String builderCommand, String buckRoot) throws IOException {
    // Run the command in replaced cmd directly.
    Process process =
        Runtime.getRuntime()
            .exec(
                builderCommand,
                /* environment variables */ null,
                /* working directory */ new File(buckRoot));
    try {
      int exitCode = process.waitFor();
      if (exitCode == 0) {
        return;
      }
      try (InputStream errorStream = process.getErrorStream()) {
        new BufferedReader(new InputStreamReader(errorStream)).lines().forEach(System.err::println);
      }
    } catch (InterruptedException interruptedException) {
      throw new IOException(interruptedException.getMessage());
    }
  }

  static @Nullable String getBuiltTargetExecutable(String builderTarget, String buckRoot)
      throws IOException {
    try (InputStream inputStream =
        CommandLine.getCommandLineOutput(
            new File(buckRoot), "buck", "build", "--show-json-output", builderTarget)) {
      JsonElement builtOutputElement =
          new JsonParser()
              .parse(new InputStreamReader(inputStream))
              .getAsJsonObject()
              .get(builderTarget);
      return builtOutputElement == null ? null : builtOutputElement.getAsString();
    }
  }
}
