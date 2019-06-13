package com.facebook.buck_project_builder;

import com.google.common.collect.ImmutableMap;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class BuckCellsTest {

  @Test
  public void parseCellMappingsTest() {
    assertEquals(
        ImmutableMap.of("cell1", "path1", "cell2", ".", "cell3", "..", "cell4", "../path/to/"),
        BuckCells.parseCellMappings(
            "{\"cell1\": \"path1\", \"cell2\": \".\", \"cell3\": \"..\", \"cell4\": \"../path/to/\"}"));
  }

  @Test
  public void getCellPathTest() {
    assertNull(BuckCells.getCellPath("//path/to/target:", ImmutableMap.of()));
    assertEquals(
        "bar",
        BuckCells.getCellPath(
            "foo//path/to/target:", ImmutableMap.of("foo", "bar", "answer", "42")));
    assertEquals(
        "42",
        BuckCells.getCellPath(
            "answer//path/to/target:", ImmutableMap.of("foo", "bar", "answer", "42")));
  }

  @Test(expected = Error.class)
  public void missingCellPathShouldThrowErrorTest() {
    BuckCells.getCellPath("missing//target/...", ImmutableMap.of());
  }
}
