// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import com.facebook.buck_project_builder.targets.BuildTarget;
import com.facebook.buck_project_builder.targets.BuildTargetsCollector;
import com.google.common.collect.ImmutableList;

import java.io.IOException;
import java.nio.file.Files;
import java.util.logging.Logger;

public final class BuckProjectBuilder {

  private static final Logger LOGGER = Logger.getGlobal();

  private BuckProjectBuilder() {}

  /**
   * Prints nothing if the build is successful. Otherwise, exit by
   * 1 then prints the failure reason in standard error.
   */
  public static void main(String[] arguments) {
    try {
      BuilderCommand command = BuilderCommand.fromCommandLineArguments(arguments);
      String outputDirectory;
      if (command.getOutputDirectory() != null) {
        outputDirectory = command.getOutputDirectory();
      } else {
        try {
          outputDirectory = Files.createTempDirectory("pyre_tmp").toString();
        } catch (IOException exception) {
          throw new BuilderException("Unable to create output directory for type checking.");
        }
      }
      String buckRoot = command.getBuckRoot();
      ImmutableList<BuildTarget> targets =
          BuildTargetsCollector.collectBuckTargets(command.getTargets());
      // Use parallel stream to parallelize build.
      targets.parallelStream().forEach(target -> target.build(buckRoot, outputDirectory));
    } catch (BuilderException exception) {
      LOGGER.severe(exception.getMessage());
      System.exit(1);
    }
  }
}
