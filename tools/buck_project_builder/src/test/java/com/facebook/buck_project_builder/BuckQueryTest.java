/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
