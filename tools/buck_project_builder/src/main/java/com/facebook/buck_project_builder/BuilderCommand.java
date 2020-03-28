package com.facebook.buck_project_builder;

import com.google.common.collect.ImmutableList;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;

final class BuilderCommand {

  private final boolean debug;
  private final String buckRoot;
  private final String outputDirectory;
  private final ImmutableList<String> targets;
  private final @Nullable String mode;
  private final @Nullable String projectName;

  BuilderCommand(
      boolean debug,
      String buckRoot,
      String outputDirectory,
      ImmutableList<String> targets,
      @Nullable String mode,
      @Nullable String projectName) {
    this.debug = debug;
    this.buckRoot = buckRoot;
    this.outputDirectory = outputDirectory;
    this.targets = targets;
    this.mode = mode;
    this.projectName = projectName;
  }

  static BuilderCommand fromCommandLineArguments(String[] arguments) throws BuilderException {
    CommandLineParser parser = new DefaultParser();
    Options parsingOptions = new Options();
    parsingOptions.addOption(Option.builder().longOpt("debug").build());
    parsingOptions.addOption(Option.builder().hasArg().longOpt("buck_root").build());
    parsingOptions.addOption(Option.builder().hasArg().longOpt("output_directory").build());
    parsingOptions.addOption(Option.builder().hasArg().longOpt("mode").build());
    parsingOptions.addOption(Option.builder().hasArg().longOpt("project_name").build());
    try {
      CommandLine parsedArguments = parser.parse(parsingOptions, arguments);
      String buckRoot = parsedArguments.getOptionValue("buck_root");
      if (buckRoot == null) {
        throw new BuilderException("`buck_root` is a required command line argument.");
      }
      String outputDirectory = parsedArguments.getOptionValue("output_directory");
      if (outputDirectory == null) {
        throw new BuilderException("`output_directory` is a required command line argument.");
      }
      @Nullable String mode = parsedArguments.getOptionValue("mode");
      List<String> targets = parsedArguments.getArgList();
      @Nullable String projectName = parsedArguments.getOptionValue("project_name");
      return new BuilderCommand(
          parsedArguments.hasOption("debug"),
          buckRoot,
          outputDirectory,
          ImmutableList.copyOf(targets),
          mode,
          projectName);
    } catch (ParseException exception) {
      throw new BuilderException(
          "Unexpected command line arguments. Detail: " + exception.getMessage());
    }
  }

  public boolean isDebug() {
    return debug;
  }

  public String getBuckRoot() {
    return buckRoot;
  }

  public String getOutputDirectory() {
    return outputDirectory;
  }

  public ImmutableList<String> getTargets() {
    return targets;
  }

  public String getMode() {
    return mode;
  }

  public @Nullable String getProjectName() {
    return projectName;
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (this == other) {
      return true;
    }
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    BuilderCommand builderCommand = (BuilderCommand) other;
    return debug == builderCommand.debug
        && buckRoot.equals(builderCommand.buckRoot)
        && outputDirectory.equals(builderCommand.outputDirectory)
        && targets.equals(builderCommand.targets)
        && (projectName != null
            ? projectName.equals(builderCommand.projectName)
            : builderCommand.projectName == null);
  }

  @Override
  public int hashCode() {
    return Objects.hash(debug, buckRoot, outputDirectory, targets, projectName);
  }

  @Override
  public String toString() {
    return String.format(
        "{debug=%b, buckRoot=%s, outputDirectory=%s, targets=%s, projectName=%s}",
        debug, buckRoot, outputDirectory, targets, projectName);
  }
}
