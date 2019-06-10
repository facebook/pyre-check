package com.facebook.buck_project_builder.targets;

public interface BuildTarget {
  void build(String buckRoot, String outputDirectory);
}
