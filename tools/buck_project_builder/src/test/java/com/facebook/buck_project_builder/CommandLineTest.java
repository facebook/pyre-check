/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder;

import com.google.common.collect.ImmutableList;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class CommandLineTest {

  @Test
  public void correctCommandLineOutput() throws IOException {
    assertEquals(
        "echo \"hello world\" should get back hello world",
        ImmutableList.of("hello world"),
        CommandLine.getCommandLineOutputLines("echo", "hello world"));
    assertEquals(
        "ls should give the same answer for two different names of current directory",
        CommandLine.getCommandLineOutputLines("ls"),
        CommandLine.getCommandLineOutputLines("ls", "."));
  }

  @Test(expected = IOException.class)
  public void nonExistingCommandTriggersError() throws IOException {
    CommandLine.getCommandLineOutputLines("haha4242");
  }
}
