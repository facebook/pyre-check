package com.facebook.buck_project_builder.cache;

import com.google.common.collect.ImmutableList;

import java.util.List;
import java.util.Objects;

public final class ThriftBuildCommand {
  private final String command;
  private final String baseModulePath;
  private final List<String> sources;

  public ThriftBuildCommand(String command, String baseModulePath, List<String> sources) {
    this.command = command;
    this.baseModulePath = baseModulePath;
    this.sources = sources;
  }

  private ThriftBuildCommand() {
    this("", "", ImmutableList.of());
  }

  public List<String> getSources() {
    return sources;
  }

  public String getBaseModulePath() {
    return baseModulePath;
  }

  public String getCommand() {
    return command;
  }

  @Override
  public String toString() {
    return String.format(
        "{command=%s, baseModulePath=%s, sources=%s", command, baseModulePath, sources);
  }

  @Override
  public boolean equals(Object other) {
    if (this == other) {
      return true;
    }
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    ThriftBuildCommand thriftBuildCommand = (ThriftBuildCommand) other;
    return command.equals(thriftBuildCommand.command)
        && baseModulePath.equals(thriftBuildCommand.baseModulePath)
        && sources.equals(thriftBuildCommand.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(command, baseModulePath, sources);
  }
}
