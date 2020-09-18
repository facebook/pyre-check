/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.List;

public final class DebugOutput {
  private final Set<String> conflictingFiles;
  private final Set<String> unsupportedFiles;
  private final int MAXIMUM_FILES = 0;

  /** Used for gson serialization. */
  private DebugOutput() {
    this(new HashSet<>(), new HashSet<>());
  }

  public DebugOutput(Set<String> conflictingFiles, Set<String> unsupportedFiles) {
    this.conflictingFiles = truncateSet(conflictingFiles);
    this.unsupportedFiles = truncateSet(unsupportedFiles);
  }

  /** We don't want to send too much data to the parent process. */
  private Set<String> truncateSet(Set<String> set) {
    List<String> temporaryList = new ArrayList<String>();
    temporaryList.addAll(set);
    Set<String> temporarySet = new HashSet<String>();
    temporarySet.addAll(temporaryList.subList(0, Math.min(set.size(), MAXIMUM_FILES)));
    return temporarySet;
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
