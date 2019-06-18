// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import com.facebook.buck_project_builder.targets.BuildTargetsBuilder;
import com.facebook.buck_project_builder.targets.BuildTargetsCollector;

import java.util.logging.Logger;

public final class BuckProjectBuilder {

  private static final Logger LOGGER = Logger.getGlobal();

  private BuckProjectBuilder() {}

  /**
   * Prints nothing if the build is successful. Otherwise, exit by 1 then prints the failure reason
   * in standard error.
   */
  public static void main(String[] arguments) {
    try {
      BuilderCommand command = BuilderCommand.fromCommandLineArguments(arguments);
      BuildTargetsBuilder builder =
          new BuildTargetsBuilder(command.getBuckRoot(), command.getOutputDirectory());
      BuildTargetsCollector.collectBuckTargets(command.getTargets())
          .forEach(target -> target.addToBuilder(builder));
      builder.buildTargets();
    } catch (BuilderException exception) {
      LOGGER.severe(exception.getMessage());
      System.exit(1);
    }
  }
}
