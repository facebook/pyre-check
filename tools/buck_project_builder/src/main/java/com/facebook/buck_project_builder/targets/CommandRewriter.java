/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;

import javax.annotation.Nullable;
import java.io.IOException;
import java.util.List;

public interface CommandRewriter {
  default String rewriteThriftLibraryBuildCommand(
      String command,
      String baseModulePath,
      List<String> sources,
      String buckRoot,
      @Nullable String projectName)
      throws IOException {
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
