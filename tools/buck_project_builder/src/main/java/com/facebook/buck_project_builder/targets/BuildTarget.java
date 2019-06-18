package com.facebook.buck_project_builder.targets;

public interface BuildTarget {
  void addToBuilder(BuildTargetsBuilder builder);
}
