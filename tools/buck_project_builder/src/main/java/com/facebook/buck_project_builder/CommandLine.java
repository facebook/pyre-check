// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.stream.Collectors;

public final class CommandLine {

  private CommandLine() {}

  private static InputStream getCommandLineOutput(String... commands) throws IOException {
    return Runtime.getRuntime().exec(commands).getInputStream();
  }

  static List<String> getCommandLineOutputLines(String... commands) throws IOException {
    try (InputStream commandLineOutput = getCommandLineOutput(commands)) {
      BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(commandLineOutput));
      return bufferedReader.lines().collect(Collectors.toList());
    }
  }

  public static InputStream getCommandLineOutput(List<String> commands) throws IOException {
    return getCommandLineOutput(commands.toArray(new String[0]));
  }
}
