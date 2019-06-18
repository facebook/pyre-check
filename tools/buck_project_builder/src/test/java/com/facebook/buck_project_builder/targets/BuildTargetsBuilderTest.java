package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.junit.Test;

import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

public class BuildTargetsBuilderTest {

  @Test
  public void buildInformationIsCorrectlyAddedTest() throws BuilderException {
    BuildTargetsBuilder builder = new BuildTargetsBuilder("/BUCK/ROOT/", "/OUT/DIR/");

    new PythonTarget(
            null,
            "BASE/PATH",
            null,
            ImmutableMap.of("source.py", "out.py"),
            ImmutableSet.of("generated.py"))
        .addToBuilder(builder);
    new RemoteFileTarget("REMOTE_URL").addToBuilder(builder);
    new ThriftLibraryTarget("BASE/PATH", "CMD_THRIFT", ImmutableList.of()).addToBuilder(builder);
    new SwigLibraryTarget(null, "BASE/PATH", "CMD_SWIG", ImmutableList.of()).addToBuilder(builder);

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
        "Remote wheel urls are correctly added.",
        ImmutableSet.of("REMOTE_URL"),
        builder.getPythonWheelUrls());
    assertEquals(
        "Thrift library build commands are correctly added.",
        ImmutableSet.of("CMD_THRIFT"),
        builder.getThriftLibraryBuildCommands());
    assertEquals(
        "Swig library build commands are correctly added.",
        ImmutableSet.of("CMD_SWIG"),
        builder.getSwigLibraryBuildCommands());
  }
}
