package com.facebook.buck_project_builder;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class BuckQueryTest {

  @Test
  public void normalizeTargetTest() {
    assertEquals("//pyre/...", BuckQuery.normalizeTarget("//pyre/..."));
    assertEquals("//pyre/...", BuckQuery.normalizeTarget("pyre/..."));
    assertEquals("c//pyre/...", BuckQuery.normalizeTarget("c//pyre/..."));
  }
}
