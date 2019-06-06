// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import org.junit.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

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
    try (FileWriter writer = new FileWriter(file)) {
      writer.write(content + "\n");
      writer.flush();
    }
  }

  private static void assertIsSymbolicLinkWithContent(Path symbolicLinkPath, String expectedContent)
      throws IOException {
    assertTrue(
        "after symbolic link creation, symbolicLinkPath should actually be a symbolic link",
        Files.isSymbolicLink(symbolicLinkPath));
    try (BufferedReader reader = new BufferedReader(new FileReader(symbolicLinkPath.toFile()))) {
      assertEquals(reader.readLine(), expectedContent);
    }
  }

  @Test
  public void resolveSourceMappingTest() {
    // simple identity mapping case
    testResolveSourceMapping(
        Map.of(
            "a.py", "a.py",
            "b.py", "b.py"),
        Map.of(
            "/project/a.py", "/out/a.py",
            "/project/b.py", "/out/b.py"));

    // non-identity source mappings
    testResolveSourceMapping(
        Map.of(
            "a.py", "foo/bar/a.py",
            "b.py", "foo/bar/baz/b.py"),
        Map.of(
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
  }
}
