/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder;

import com.facebook.buck_project_builder.SimpleLogger;
import com.google.common.collect.ImmutableList;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class BuilderCommandTest {

  private static void assertParsedTo(BuilderCommand expected, String arguments)
      throws BuilderException {
    assertEquals(expected, BuilderCommand.fromCommandLineArguments(arguments.split(" ")));
  }

  @Test
  public void goodArgumentsDoParse() throws BuilderException {
    // All optional arguments do appear. Multiple build targets.
    assertParsedTo(
        new BuilderCommand(false, "ROOT", "OUT", ImmutableList.of("foo", "bar"), null, null),
        "--buck_root ROOT --output_directory OUT foo bar");

    // Empty targets are allowed.
    assertParsedTo(
        new BuilderCommand(false, "ROOT", "OUT", ImmutableList.of(), null, null),
        "--buck_root ROOT --output_directory OUT");

    // Debug argument parsing.
    assertParsedTo(
        new BuilderCommand(true, "ROOT", "OUT", ImmutableList.of(), null, null),
        "--debug --buck_root ROOT --output_directory OUT");

    // Buck mode
    assertParsedTo(
        new BuilderCommand(true, "ROOT", "OUT", ImmutableList.of(), "@mode/dev", null),
        "--debug --buck_root ROOT --output_directory OUT --mode @mode/dev");

    // Project name.
    assertParsedTo(
        new BuilderCommand(true, "ROOT", "OUT", ImmutableList.of(), "@mode/dev", "foo-project"),
        "--debug --buck_root ROOT --output_directory OUT --mode @mode/dev --project_name foo-project");
  }

  @Test(expected = BuilderException.class)
  public void randomArgumentsDoNotParse() throws BuilderException {
    BuilderCommand.fromCommandLineArguments(new String[] {"hello"});
  }

  @Test(expected = BuilderException.class)
  public void emptyArgumentsDoNotParse() throws BuilderException {
    BuilderCommand.fromCommandLineArguments(new String[0]);
  }

  @Test(expected = BuilderException.class)
  public void missingBuckRootDoNotParse() throws BuilderException {
    BuilderCommand.fromCommandLineArguments(new String[] {"target_1"});
  }

  @Test(expected = BuilderException.class)
  public void missingOutputDirectoryDoNotParse() throws BuilderException {
    BuilderCommand.fromCommandLineArguments(new String[] {"--buck_root", "ROOT", "target_1"});
  }

  @Test(expected = BuilderException.class)
  public void missingOutputDirectoryArgumentDoNotParse() throws BuilderException {
    BuilderCommand.fromCommandLineArguments(
        new String[] {"--buck_root", "ROOT", "--output_directory"});
  }
}
