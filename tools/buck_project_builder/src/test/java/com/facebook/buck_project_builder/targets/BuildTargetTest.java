package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.FileSystemTest;
import com.google.common.collect.ImmutableMap;
import org.apache.commons.io.FileUtils;
import org.junit.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

public class BuildTargetTest {

  /**
   * Python targets' build strategy is simply creating symbolic link for all the source files to a
   * path in output directory. We test whether we created all symbolic links in the right place in a
   * temporary folder.
   */
  @Test
  public void pythonTargetsCanCorrectlyBuild() throws IOException {
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
    pythonTargetWithoutBaseModule.build(buckRoot.getPath(), outputDirectory.getPath());
    FileSystemTest.assertIsSymbolicLinkWithContent(
        Paths.get(outputDirectory.getPath(), "b.py"), "print('hello world')");

    PythonTarget pythonTargetWithBaseModule =
        new PythonTarget(".", "foo.bar", ImmutableMap.of("a.py", "b.py"));
    pythonTargetWithBaseModule.build(buckRoot.getPath(), outputDirectory.getPath());
    FileSystemTest.assertIsSymbolicLinkWithContent(
        Paths.get(outputDirectory.getPath(), "foo", "bar", "b.py"), "print('hello world')");

    new File(temporaryRoot).deleteOnExit();
  }

  @Test
  public void copyGeneratedThriftSourcesTest() throws IOException {
    String root = Files.createTempDirectory("copy_thrift_source_test").toString();
    Path sourceRootPath = Paths.get(root, "src");
    String sourceRoot = sourceRootPath.toString();
    String outputRoot = Paths.get(root, "out").toString();

    File pythonFile = Paths.get(sourceRoot, "foo", "bar", "a.py").toFile();
    FileUtils.write(pythonFile, "print('a')", Charset.defaultCharset());
    File pythonInterfaceFile = Paths.get(sourceRoot, "foo", "bar", "b.pyi").toFile();
    FileUtils.write(pythonInterfaceFile, "def foo(): ...", Charset.defaultCharset());
    File remoteFile = Paths.get(sourceRoot, "remote", "c-remote").toFile();
    FileUtils.write(remoteFile, "haha", Charset.defaultCharset());

    ThriftLibraryTarget.copyGeneratedThriftSources(sourceRootPath, outputRoot);

    assertEquals(
        "print('a')",
        FileUtils.readFileToString(
            Paths.get(outputRoot, "foo", "bar", "a.py").toFile(), Charset.defaultCharset()));
    assertEquals(
        "def foo(): ...",
        FileUtils.readFileToString(
            Paths.get(outputRoot, "foo", "bar", "b.pyi").toFile(), Charset.defaultCharset()));
    assertEquals(
        "haha",
        FileUtils.readFileToString(
            Paths.get(outputRoot, "remote", "c-remote.py").toFile(), Charset.defaultCharset()));

    FileUtils.deleteDirectory(new File(root));
  }
}
