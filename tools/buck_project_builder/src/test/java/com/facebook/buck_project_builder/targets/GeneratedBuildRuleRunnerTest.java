package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonArray;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class GeneratedBuildRuleRunnerTest {

  @Test
  public void buildSourcesTest() {
    JsonArray sourcesField = new JsonArray();
    sourcesField.add("a.py");
    sourcesField.add("b.py");

    assertEquals(
        "when cellPath is not present, source paths are relative.",
        ImmutableList.of("./BASE_PATH/a.py", "./BASE_PATH/b.py"),
        GeneratedBuildRuleRunner.buildSources(null, ".", "BASE_PATH", sourcesField));

    assertEquals(
        "when cellPath is present, source paths are absolute.",
        ImmutableList.of("/CELL_PATH/BASE_PATH/a.py", "/CELL_PATH/BASE_PATH/b.py"),
        GeneratedBuildRuleRunner.buildSources("/CELL_PATH", ".", "BASE_PATH", sourcesField));
  }
}
