package com.facebook.buck_project_builder.cache;

import com.facebook.buck_project_builder.BuilderException;
import com.facebook.buck_project_builder.SimpleLogger;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;

public final class CacheLock {

  public static void synchronize(Builder builder) throws BuilderException {
    File lockFile = new File(BuilderCache.LOCK_PATH);
    try {
      FileUtils.touch(lockFile);
    } catch (IOException exception) {
      throw new BuilderException("Cannot create builder lock file.");
    }
    try (RandomAccessFile randomAccessLockFile = new RandomAccessFile(lockFile, "rw");
        FileChannel lockFileChannel = randomAccessLockFile.getChannel()) {
      while (lockFileChannel.tryLock() == null) {
        try {
          Thread.sleep(1000);
          SimpleLogger.info("Waiting for builder cache lock...");
        } catch (InterruptedException exception) {
          throw new BuilderException("Interrupted while waiting for builder cache lock.");
        }
      }
      // Acquired the lock.
      builder.build();
    } catch (IOException exception) {
      throw new BuilderException("Cannot synchronize access to builder cache.");
    }
  }

  public interface Builder {
    void build() throws BuilderException;
  }
}
