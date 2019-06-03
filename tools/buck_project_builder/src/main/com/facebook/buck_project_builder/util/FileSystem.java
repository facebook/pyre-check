// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class FileSystem {

  private FileSystem() {}

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
