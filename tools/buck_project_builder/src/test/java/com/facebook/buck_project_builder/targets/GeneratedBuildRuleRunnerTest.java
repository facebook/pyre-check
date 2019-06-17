package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class GeneratedBuildRuleRunnerTest {

  @Test
  public void getBasePathPrefixedSourcesTest() {
    assertEquals(
        "when cellPath is not present, source paths are relative.",
        "./BASE_PATH/a.py ./BASE_PATH/b.py",
        GeneratedBuildRuleRunner.getBasePathPrefixedSources(
            null, "BASE_PATH", ImmutableList.of("a.py", "b.py")));

    assertEquals(
        "when cellPath is present, source paths are absolute.",
        "/CELL_PATH/BASE_PATH/a.py /CELL_PATH/BASE_PATH/b.py",
        GeneratedBuildRuleRunner.getBasePathPrefixedSources(
            "/CELL_PATH", "BASE_PATH", ImmutableList.of("a.py", "b.py")));
  }
}
