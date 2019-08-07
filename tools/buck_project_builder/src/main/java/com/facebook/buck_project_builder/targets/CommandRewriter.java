package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;

import java.util.List;

public interface CommandRewriter {
  default String rewriteThriftLibraryBuildCommand(
      String command, String baseModulePath, List<String> sources) {
    return command;
  }

  default String rewriteSwigLibraryBuildCommand(
      String command, String outputDirectory, ImmutableList<String> sources) {
    return command;
  }

  default String rewriteAntlr4LibraryBuildCommand(
      String command, String basePath, String outputDirectory, ImmutableList<String> sources) {
    return command;
  }
}
