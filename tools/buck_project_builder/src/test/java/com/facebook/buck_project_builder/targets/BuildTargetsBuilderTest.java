package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.facebook.buck_project_builder.DebugOutput;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.junit.Test;

import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

public class BuildTargetsBuilderTest {

  @Test
  public void pythonTargetsBuildInformationIsCorrectlyAddedTest() throws BuilderException {
    BuildTargetsBuilder builder =
        new BuildTargetsBuilder(0, "/BUCK/ROOT/", "/OUT/DIR/", ImmutableList.of("//target"));

    new PythonTarget(
            null,
            "BASE/PATH",
            null,
            ImmutableMap.of("source.py", "out.py", "source-conflict.py", "out.py"),
            ImmutableSet.of("generated.py"))
        .addToBuilder(builder);

    assertEquals(
        "Python sources are correctly added.",
        ImmutableMap.of(
            Paths.get("/OUT/DIR/BASE/PATH/out.py"), Paths.get("/BUCK/ROOT/BASE/PATH/source.py")),
        builder.getSources());
    assertEquals(
        "Unsupported generated sources are correctly added.",
        ImmutableSet.of("/OUT/DIR/BASE/PATH/generated.py"),
        builder.getUnsupportedGeneratedSources());
    assertEquals(
        "Conflicting files are detected",
        new DebugOutput(
            ImmutableSet.of("BASE/PATH/out.py"), ImmutableSet.of("BASE/PATH/generated.py")),
        builder.buildTargets());
  }

  @Test
  public void remoteTargetBuildInformationIsCorrectlyAddedTest() {
    BuildTargetsBuilder builder =
        new BuildTargetsBuilder(0, "/BUCK/ROOT/", "/OUT/DIR/", ImmutableList.of("//target"));

    new RemoteFileTarget("REMOTE_URL").addToBuilder(builder);

    assertEquals(
        "Remote wheel urls are correctly added.",
        ImmutableSet.of("REMOTE_URL"),
        builder.getPythonWheelUrls());
  }

  @Test
  public void generatedCodeTargetBuildInformationIsCorrectlyAddedTest() {
    BuildTargetsBuilder builder =
        new BuildTargetsBuilder(0, "/BUCK/ROOT/", "/OUT/DIR/", ImmutableList.of("//target"));

    new ThriftLibraryTarget("CMD_THRIFT", "PATH", ImmutableList.of()).addToBuilder(builder);
    new SwigLibraryTarget("CMD_SWIG", ImmutableList.of()).addToBuilder(builder);
    new Antlr4LibraryTarget("CMD_ANTLR4", "PATH", ImmutableList.of()).addToBuilder(builder);

    assertEquals(
        "Thrift library build commands are correctly added.",
        ImmutableSet.of(new ThriftLibraryTarget("CMD_THRIFT", "PATH", ImmutableList.of())),
        builder.getThriftLibraryTargets());
    assertEquals(
        "Swig library build commands are correctly added.",
        ImmutableSet.of("CMD_SWIG"),
        builder.getSwigLibraryBuildCommands());
    assertEquals(
        "ANTLR4 library build commands are correctly added.",
        ImmutableSet.of("CMD_ANTLR4"),
        builder.getAntlr4LibraryBuildCommands());
  }
}
