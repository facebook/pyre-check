/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder;

import com.facebook.buck_project_builder.cache.BuilderCache;
import com.facebook.buck_project_builder.cache.CacheLock;
import com.facebook.buck_project_builder.targets.BuildTargetsCollector;
import com.facebook.buck_project_builder.targets.CommandRewriter;
import com.facebook.buck_project_builder.targets.PlatformSelector;
import com.google.common.collect.ImmutableList;
import com.google.gson.Gson;
import org.apache.commons.io.FileUtils;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;

public final class BuckProjectBuilder {

  public static BuilderCommand parseCommandLineArguments(String[] arguments) {
    try {
      return BuilderCommand.fromCommandLineArguments(arguments);
    } catch (BuilderException exception) {
      SimpleLogger.error(exception.getMessage());
      System.exit(1);
      throw new RuntimeException(); // unreachable
    }
  }

  /**
   * Prints nothing if the build is successful. Otherwise, exit by 1 then prints the failure reason
   * in standard error.
   */
  public BuckProjectBuilder(
      BuilderCommand command, PlatformSelector platformSelector, CommandRewriter commandRewriter)
      throws IOException {
    long start = System.currentTimeMillis();
    String buckRoot = command.getBuckRoot();
    String outputDirectory = command.getOutputDirectory();
    ImmutableList<String> targets = command.getTargets();
    String mode = command.getMode();
    @Nullable String projectName = command.getProjectName();

    try {
      CacheLock.synchronize(
          () -> {
            DebugOutput debugOutput =
                new BuildTargetsCollector(
                        buckRoot,
                        outputDirectory,
                        projectName,
                        platformSelector,
                        commandRewriter,
                        mode)
                    .getBuilder(start, targets, command.getIsolationPrefix())
                    .buildTargets(buckRoot, projectName);
            if (command.isDebug()) {
              System.out.println(new Gson().toJson(debugOutput));
            }
          },
          buckRoot,
          projectName);
      BuildTimeLogger.logBuildTime(start, System.currentTimeMillis(), targets);
    } catch (BuilderException exception) {
      SimpleLogger.error(exception.getMessage());
      SimpleLogger.error("Build failure. Invalidating all build cache.");
      FileUtils.deleteQuietly(new File(BuilderCache.getCachePath(targets, buckRoot, projectName)));
      System.exit(1);
    }
  }

  public static void main(String[] arguments) throws IOException {
    System.setProperty("java.net.preferIPv6Addresses", "true");
    new BuckProjectBuilder(
        parseCommandLineArguments(arguments),
        new PlatformSelector() {},
        new CommandRewriter() {}
      );
  }
}
