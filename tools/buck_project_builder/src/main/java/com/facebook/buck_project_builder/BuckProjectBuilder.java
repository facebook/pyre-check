// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import com.facebook.buck_project_builder.targets.BuildTargetsBuilder;
import com.facebook.buck_project_builder.targets.BuildTargetsCollector;
import com.google.common.collect.ImmutableList;
import com.google.gson.Gson;

public final class BuckProjectBuilder {

  private BuckProjectBuilder() {}

  /**
   * Prints nothing if the build is successful. Otherwise, exit by 1 then prints the failure reason
   * in standard error.
   */
  public static void main(String[] arguments) {
    try {
      long start = System.currentTimeMillis();
      BuilderCommand command = BuilderCommand.fromCommandLineArguments(arguments);
      ImmutableList<String> targets = command.getTargets();
      BuildTargetsBuilder builder =
          new BuildTargetsBuilder(command.getBuckRoot(), command.getOutputDirectory());
      BuildTargetsCollector.collectBuckTargets(targets)
          .forEach(target -> target.addToBuilder(builder));
      DebugOutput debugOutput = builder.buildTargets();
      if (command.isDebug()) {
        System.out.println(new Gson().toJson(debugOutput));
      }
      BuildTimeLogger.logBuildTime(start, System.currentTimeMillis(), targets);
    } catch (BuilderException exception) {
      SimpleLogger.error(exception.getMessage());
      System.exit(1);
    }
  }
}
