package com.facebook.buck_project_builder;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public final class DebugOutput {
  private final Set<String> conflictingFiles;
  private final Set<String> unsupportedFiles;

  /** Used for gson serialization. */
  private DebugOutput() {
    this(new HashSet<>(), new HashSet<>());
  }

  public DebugOutput(Set<String> conflictingFiles, Set<String> unsupportedFiles) {
    this.conflictingFiles = conflictingFiles;
    this.unsupportedFiles = unsupportedFiles;
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (this == other) {
      return true;
    }
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    DebugOutput output = (DebugOutput) other;
    return conflictingFiles.equals(output.conflictingFiles)
        && unsupportedFiles.equals(output.unsupportedFiles);
  }

  @Override
  public int hashCode() {
    return Objects.hash(conflictingFiles, unsupportedFiles);
  }
}
