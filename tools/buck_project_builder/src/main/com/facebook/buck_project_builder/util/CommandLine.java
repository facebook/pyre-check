// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder.util;

import javax.annotation.Nullable;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.stream.Collectors;

final class CommandLine {

  private CommandLine() {}

  private static List<String> getCommandLineOutputLines(
      String[] commands, @Nullable String workingDirectory) throws IOException {
    File workingDirFile = workingDirectory == null ? null : new File(workingDirectory);
    try (InputStream commandLineOutput =
        Runtime.getRuntime().exec(commands, null, workingDirFile).getInputStream()) {
      BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(commandLineOutput));
      return bufferedReader.lines().collect(Collectors.toList());
    }
  }

  static List<String> getCommandLineOutputLines(String... commands) throws IOException {
    return getCommandLineOutputLines(commands, null);
  }

  static List<String> getCommandLineOutputLines(List<String> commands, String workingDirectory)
      throws IOException {
    return getCommandLineOutputLines(commands.toArray(new String[0]), workingDirectory);
  }
}
