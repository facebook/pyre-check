// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import com.facebook.buck_project_builder.cache.BuilderCache;
import com.facebook.buck_project_builder.cache.CacheLock;
import com.facebook.buck_project_builder.targets.BuildTargetsCollector;
import com.facebook.buck_project_builder.targets.CommandRewriter;
import com.facebook.buck_project_builder.targets.PlatformSelector;
import com.google.common.collect.ImmutableList;
import com.google.gson.Gson;
import org.apache.commons.io.FileUtils;

import java.io.File;

public final class BuckProjectBuilder {

  /**
   * Prints nothing if the build is successful. Otherwise, exit by 1 then prints the failure reason
   * in standard error.
   */
  public BuckProjectBuilder(
      String[] arguments, PlatformSelector platformSelector, CommandRewriter commandRewriter) {
    long start = System.currentTimeMillis();
    BuilderCommand command;
    try {
      command = BuilderCommand.fromCommandLineArguments(arguments);
    } catch (BuilderException exception) {
      SimpleLogger.error(exception.getMessage());
      System.exit(1);
      return;
    }
    String buckRoot = command.getBuckRoot();
    String outputDirectory = command.getOutputDirectory();
    ImmutableList<String> targets = command.getTargets();

    try {
      CacheLock.synchronize(
          () -> {
            DebugOutput debugOutput =
                new BuildTargetsCollector(
                        buckRoot, outputDirectory, platformSelector, commandRewriter)
                    .getBuilder(start, targets)
                    .buildTargets();
            if (command.isDebug()) {
              System.out.println(new Gson().toJson(debugOutput));
            }
          });
      BuildTimeLogger.logBuildTime(start, System.currentTimeMillis(), targets);
    } catch (BuilderException exception) {
      SimpleLogger.error(exception.getMessage());
      SimpleLogger.error("Build failure. Invalidating all build cache.");
      FileUtils.deleteQuietly(new File(BuilderCache.getCachePath(targets)));
      System.exit(1);
    }
  }

  public static void main(String[] arguments) {
    new BuckProjectBuilder(arguments, new PlatformSelector() {}, new CommandRewriter() {});
  }
}
