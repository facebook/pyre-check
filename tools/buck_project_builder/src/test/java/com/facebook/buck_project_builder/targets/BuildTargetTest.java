package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.facebook.buck_project_builder.FileSystemTest;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.junit.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class BuildTargetTest {

  /**
   * Python targets' build strategy is simply creating symbolic link for all the source files to a
   * path in output directory. We test whether we created all symbolic links in the right place in a
   * temporary folder.
   */
  @Test
  public void pythonTargetsCanCorrectlyBuild() throws IOException, BuilderException {
    String temporaryRoot = Files.createTempDirectory("python_target_build_test").toString();

    File buckRoot = Paths.get(temporaryRoot, "buck_root/").toFile();
    buckRoot.mkdirs();
    File outputDirectory = Paths.get(temporaryRoot, "buck_out/").toFile();
    outputDirectory.mkdirs();

    File sourceFile = Paths.get(buckRoot.getPath(), "a.py").toFile();
    FileWriter writer = new FileWriter(sourceFile);
    writer.write("print('hello world')\n");
    writer.flush();

    PythonTarget pythonTargetWithoutBaseModule =
        new PythonTarget(".", null, ImmutableMap.of("a.py", "b.py"));
    BuildTargetsBuilder builder =
        new BuildTargetsBuilder(
            0, buckRoot.getPath(), outputDirectory.getPath(), ImmutableList.of("//target"));
    pythonTargetWithoutBaseModule.addToBuilder(builder);
    builder.buildTargets();
    FileSystemTest.assertIsSymbolicLinkWithContent(
        Paths.get(outputDirectory.getPath(), "b.py"), "print('hello world')");

    PythonTarget pythonTargetWithBaseModule =
        new PythonTarget(".", "foo.bar", ImmutableMap.of("a.py", "b.py"));
    builder =
        new BuildTargetsBuilder(
            0, buckRoot.getPath(), outputDirectory.getPath(), ImmutableList.of("//target"));
    pythonTargetWithBaseModule.addToBuilder(builder);
    builder.buildTargets();
    FileSystemTest.assertIsSymbolicLinkWithContent(
        Paths.get(outputDirectory.getPath(), "foo", "bar", "b.py"), "print('hello world')");

    new File(temporaryRoot).deleteOnExit();
  }
}
