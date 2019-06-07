// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class FileSystem {

  private FileSystem() {}

  /**
   * @return a mapping from absolute source path to absolute output path as specified by the sources
   *     object. Files are not guaranteed to exist.
   */
  public static Map<String, String> resolveSourceMapping(
      String sourceDirectory, String outputDirectory, Map<String, String> sources) {
    Map<String, String> result = new HashMap<>();
    for (Map.Entry<String, String> entry : sources.entrySet()) {
      String sourceFile = entry.getKey();
      String outputFile = entry.getValue();
      result.put(
          Paths.get(sourceDirectory, sourceFile).toString(),
          Paths.get(outputDirectory, outputFile).toString());
    }
    return result;
  }

  public static void addSymbolicLink(Path linkPath, Path actualPath) {
    linkPath.getParent().toFile().mkdirs();
    try {
      Files.deleteIfExists(linkPath);
      Files.createSymbolicLink(linkPath, actualPath);
    } catch (IOException exception) {
      Logger.getGlobal().log(Level.SEVERE, exception.getMessage());
    }
  }
}
