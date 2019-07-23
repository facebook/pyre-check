// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class FileSystemTest {

  private static final String COMMON_SOURCE_DIRECTORY = "/project";
  private static final String COMMON_OUT_DIRECTORY = "/out";

  private static void testResolveSourceMapping(
      Map<String, String> sources, Map<String, String> expectedMapping) {
    Map<String, String> actualMapping =
        FileSystem.resolveSourceMapping(COMMON_SOURCE_DIRECTORY, COMMON_OUT_DIRECTORY, sources);
    assertEquals(expectedMapping, actualMapping);
  }

  private static void writeContent(File file, String content) throws IOException {
    file.getParentFile().mkdirs();
    try (FileWriter writer = new FileWriter(file)) {
      writer.write(content + "\n");
      writer.flush();
    }
  }

  private static void assertContent(File file, String expectedContent) throws IOException {
    try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
      assertEquals(reader.readLine(), expectedContent);
    }
  }

  public static void assertIsSymbolicLinkWithContent(Path symbolicLinkPath, String expectedContent)
      throws IOException {
    assertTrue(
        "after symbolic link creation, symbolicLinkPath should actually be a symbolic link",
        Files.isSymbolicLink(symbolicLinkPath));
    assertContent(symbolicLinkPath.toFile(), expectedContent);
  }

  @Test
  public void resolveSourceMappingTest() {
    // simple identity mapping case
    testResolveSourceMapping(
        ImmutableMap.of(
            "a.py", "a.py",
            "b.py", "b.py"),
        ImmutableMap.of(
            "/project/a.py", "/out/a.py",
            "/project/b.py", "/out/b.py"));

    // non-identity source mappings
    testResolveSourceMapping(
        ImmutableMap.of(
            "a.py", "foo/bar/a.py",
            "b.py", "foo/bar/baz/b.py"),
        ImmutableMap.of(
            "/project/a.py", "/out/foo/bar/a.py",
            "/project/b.py", "/out/foo/bar/baz/b.py"));
  }

  @Test
  public void addSymbolicLinkTest() throws IOException {
    String root = Files.createTempDirectory("symbolic_link_test").toString();
    Path actualPath = Paths.get(root, "a.txt");
    Path symbolicLinkPath = Paths.get(root, "a/b/c/link");

    assertFalse("symbolic link does not exist yet", symbolicLinkPath.toFile().exists());
    writeContent(actualPath.toFile(), "abc");
    FileSystem.addSymbolicLink(symbolicLinkPath, actualPath);
    assertIsSymbolicLinkWithContent(symbolicLinkPath, "abc");

    // the operation above makes the symbolic link appear.
    assertTrue("symbolic link should already be there", symbolicLinkPath.toFile().exists());
    writeContent(actualPath.toFile(), "def");
    FileSystem.addSymbolicLink(symbolicLinkPath, actualPath);
    assertIsSymbolicLinkWithContent(symbolicLinkPath, "def");

    // We do nothing when the source path does not exist, so the symbolic link file should still
    // have the old content.
    FileSystem.addSymbolicLink(
        symbolicLinkPath, Paths.get("this", "path", "can", "never", "exist"));
    assertIsSymbolicLinkWithContent(symbolicLinkPath, "def");

    new File(root).delete();
  }

  @Test
  public void unzipRemoteFileTest() throws IOException {
    String root = Files.createTempDirectory("unzip_remote_test").toString();
    File testFile = new File(root, "test.txt");
    writeContent(testFile, "hello world");
    File outputDirectory = Paths.get(root, "out").toFile();

    File zipFile = Paths.get(root, "test.zip").toFile();
    try (InputStream testFileInputStream = new FileInputStream(testFile)) {
      try (ZipOutputStream zipOutputStream = new ZipOutputStream(new FileOutputStream(zipFile))) {
        ZipEntry entry = new ZipEntry(Paths.get("foo", "bar", "test.txt").toString());
        zipOutputStream.putNextEntry(entry);
        IOUtils.copy(testFileInputStream, zipOutputStream);
      }
    }

    // use file protocol to simulate remote file
    String cacheDirectory = Files.createTempDirectory("cache-path").toString();
    ImmutableSet<String> conflictingFilesOnFirstUnzip =
        FileSystem.unzipRemoteFile("file://" + zipFile.toString(), cacheDirectory, outputDirectory);
    assertEquals(ImmutableSet.of(), conflictingFilesOnFirstUnzip);
    // Unzip twice to test file conflict detection
    ImmutableSet<String> conflictingFilesOnSecondUnzip =
        FileSystem.unzipRemoteFile("file://" + zipFile.toString(), cacheDirectory, outputDirectory);
    assertEquals(ImmutableSet.of("foo/bar/test.txt"), conflictingFilesOnSecondUnzip);
    assertContent(Paths.get(root, "out", "foo", "bar", "test.txt").toFile(), "hello world");

    // Check the zip is cached.
    assertTrue(Paths.get(cacheDirectory, "test.zip").toFile().exists());

    FileUtils.deleteDirectory(new File(cacheDirectory));
    FileUtils.deleteDirectory(new File(root));
  }
}
