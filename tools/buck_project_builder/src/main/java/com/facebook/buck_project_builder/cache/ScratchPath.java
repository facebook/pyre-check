/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder.cache;

import com.facebook.buck_project_builder.SimpleLogger;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.concurrent.TimeUnit;

public class ScratchPath {
  public static String getScratchPath(String buckRoot) throws IOException {
    @Nullable File outputFile = null;
    try {
      outputFile = File.createTempFile("output-file", null);
      if (outputFile != null) {
        outputFile.deleteOnExit();
      }
      String command = "mkscratch path --subdir pyre " + buckRoot;
      Process process =
          new ProcessBuilder()
              .command(org.apache.commons.exec.CommandLine.parse(command).toStrings())
              .redirectError(ProcessBuilder.Redirect.INHERIT)
              .redirectOutput(ProcessBuilder.Redirect.to(outputFile))
              .directory(new File(buckRoot))
              .start();
      boolean hasTimedOut = !process.waitFor(60, TimeUnit.SECONDS);
      if (hasTimedOut) {
        throw new IOException("Timed out while getting scratch directory for buck builder cache.");
      }
      String output = new String(Files.readAllBytes(outputFile.toPath())).trim();
      return output;
    } catch (IOException | InterruptedException exception) {
      String output = exception.getMessage();
      if (outputFile != null) {
        output += new String(Files.readAllBytes(outputFile.toPath()));
      }
      throw new IOException(output);
    }
  }
}
