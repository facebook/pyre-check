package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.CommandLine;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
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

  static boolean runBuilderCommand(String builderCommand, String buckRoot) throws IOException {
    Process process =
        new ProcessBuilder()
            .command(org.apache.commons.exec.CommandLine.parse(builderCommand).toStrings())
            .redirectError(ProcessBuilder.Redirect.INHERIT)
            .redirectOutput(ProcessBuilder.Redirect.to(new File("/dev/null")))
            .directory(new File(buckRoot))
            .start();
    try {
      boolean hasTimedOut = !process.waitFor(60, TimeUnit.SECONDS);
      if (hasTimedOut) {
        return false;
      }
      return process.exitValue() == 0;
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
