// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import javax.annotation.Nullable;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.ProcessBuilder;
import java.util.List;
import java.util.stream.Collectors;

public final class CommandLine {

  private CommandLine() {}

  public static InputStream getCommandLineOutput(
      @Nullable File workingDirectory, String... commands) throws IOException {

    ProcessBuilder processBuilder = new ProcessBuilder(commands);
    processBuilder.directory(workingDirectory);
    processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT);
    // The runtime reaps the process once it exits.
    Process process = processBuilder.start();
    return process.getInputStream();
  }

  public static List<String> getCommandLineOutputLines(
      @Nullable File workingDirectory, String... commands) throws IOException {
    try (InputStream commandLineOutput = getCommandLineOutput(workingDirectory, commands)) {
      BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(commandLineOutput));
      return bufferedReader.lines().collect(Collectors.toList());
    }
  }

  static List<String> getCommandLineOutputLines(String... commands) throws IOException {
    return getCommandLineOutputLines(null, commands);
  }

  public static InputStream getCommandLineOutput(List<String> commands) throws IOException {
    return getCommandLineOutput(null, commands.toArray(new String[0]));
  }
}
