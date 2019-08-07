package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.facebook.buck_project_builder.FileSystemTest;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.junit.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
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

    ImmutableList<String> targets = ImmutableList.of("//target");
    ImmutableMap<Path, Path> sources =
        ImmutableMap.of(
            Paths.get(outputDirectory.toString(), "b.py"), Paths.get(buckRoot.toString(), "a.py"));
    BuildTargetsBuilder builder =
        new BuildTargetsBuilder(
            0,
            buckRoot.getPath(),
            outputDirectory.getPath(),
            targets,
            sources,
            ImmutableSet.of(),
            ImmutableSet.of(),
            ImmutableSet.of(),
            ImmutableSet.of(),
            ImmutableSet.of());
    builder.buildTargets();
    FileSystemTest.assertIsSymbolicLinkWithContent(
        Paths.get(outputDirectory.getPath(), "b.py"), "print('hello world')");

    sources =
        ImmutableMap.of(
            Paths.get(outputDirectory.toString(), "foo", "bar", "b.py"),
            Paths.get(buckRoot.toString(), "a.py"));
    builder =
        new BuildTargetsBuilder(
            0,
            buckRoot.getPath(),
            outputDirectory.getPath(),
            targets,
            sources,
            ImmutableSet.of(),
            ImmutableSet.of(),
            ImmutableSet.of(),
            ImmutableSet.of(),
            ImmutableSet.of());
    builder.buildTargets();
    FileSystemTest.assertIsSymbolicLinkWithContent(
        Paths.get(outputDirectory.getPath(), "foo", "bar", "b.py"), "print('hello world')");

    new File(temporaryRoot).deleteOnExit();
  }
}
