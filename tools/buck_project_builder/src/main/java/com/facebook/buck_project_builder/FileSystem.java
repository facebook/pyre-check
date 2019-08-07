// Copyright 2004-present Facebook. All Rights Reserved.

package com.facebook.buck_project_builder;

import com.google.common.collect.ImmutableSet;
import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public final class FileSystem {

  private FileSystem() {}

  public static void addSymbolicLink(Path linkPath, Path actualPath) {
    if (!actualPath.toFile().exists()) {
      // We ignore requests to create symbolic link when actual path does not exist.
      return;
    }
    linkPath.getParent().toFile().mkdirs();
    try {
      Files.deleteIfExists(linkPath);
      Files.createSymbolicLink(linkPath, actualPath);
    } catch (IOException exception) {
      SimpleLogger.error("Cannot create symbolic link: " + exception.getMessage());
    }
  }

  private static File downloadRemoteZip(String remoteUrl, String cacheDirectory)
      throws IOException {
    URL url = new URL(remoteUrl);
    String relativeZipPath = Paths.get(url.getPath()).getFileName().toString();
    File outputFile = Paths.get(cacheDirectory, relativeZipPath).toFile();
    if (outputFile.exists()) {
      return outputFile;
    }
    new File(url.getPath()).getName();
    try (InputStream remoteInputStream = url.openStream();
        FileOutputStream zipFileOutputStream = new FileOutputStream(outputFile)) {
      IOUtils.copy(remoteInputStream, zipFileOutputStream);
    }
    return outputFile;
  }

  /**
   * @return a set of files in the zip that are not unzipped because they can override existing
   *     files.
   */
  public static ImmutableSet<String> unzipRemoteFile(
      String remoteUrl, String cacheDirectory, File outputDirectory) throws IOException {
    File downloadedZipFile = downloadRemoteZip(remoteUrl, cacheDirectory);
    ImmutableSet.Builder<String> conflictingFileSetBuilder = ImmutableSet.builder();
    try (ZipFile zipFile = new ZipFile(downloadedZipFile)) {
      Enumeration<? extends ZipEntry> zipEntries = zipFile.entries();
      while (zipEntries.hasMoreElements()) {
        ZipEntry zipEntry = zipEntries.nextElement();
        if (!zipEntry.isDirectory()) {
          File outputFile = new File(outputDirectory, File.separator + zipEntry.getName());
          if (outputFile.exists()) {
            conflictingFileSetBuilder.add(zipEntry.getName());
            // Avoid overriding existing files, which might be symbolic links.
            continue;
          }
          outputFile.getParentFile().mkdirs();
          IOUtils.copy(zipFile.getInputStream(zipEntry), new FileOutputStream(outputFile));
        }
      }
    }
    return conflictingFileSetBuilder.build();
  }
}
