package com.facebook.buck_project_builder.cache;

import com.facebook.buck_project_builder.SimpleLogger;
import com.facebook.buck_project_builder.targets.ThriftLibraryTarget;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import org.apache.commons.codec.digest.DigestUtils;

import javax.annotation.Nullable;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Set;

public final class BuilderCache {

  public static final String LOCK_PATH = "/tmp/pyre/buck_builder_cache/builder.lock";
  public static final String THRIFT_CACHE_PATH = "/tmp/pyre/buck_builder_cache/thrift-gen";
  public static final String WHEEL_CACHE_PATH = "/tmp/pyre/buck_builder_cache/downloaded-wheels";

  private final long lastBuiltTime;
  private final Set<ThriftLibraryTarget> thriftCaches;

  public BuilderCache(long lastBuiltTime, Set<ThriftLibraryTarget> thriftCaches) {
    this.lastBuiltTime = lastBuiltTime;
    this.thriftCaches = thriftCaches;
  }

  public BuilderCache() {
    this(0, ImmutableSet.of());
  }

  public static String getCachePath(ImmutableList<String> targets) {
    String escapedTargets = DigestUtils.md5Hex(String.join(";", targets));
    if (escapedTargets.length() > 255) {
      // 255 is the Linux filename length limit for EXT4.
      // Most target list is not crazily long, and collision is unlikely to happen.
      escapedTargets = escapedTargets.substring(0, 255);
    }
    return Paths.get("/tmp/pyre/buck_builder_cache", escapedTargets).toString();
  }

  private static File getCacheJsonFile(ImmutableList<String> targets) {
    return Paths.get(getCachePath(targets), "cache.json").toFile();
  }

  @VisibleForTesting
  static BuilderCache readFromCache(Reader reader) {
    try {
      return new Gson().fromJson(reader, BuilderCache.class);
    } catch (JsonSyntaxException exception) {
      SimpleLogger.warning("Buck builder cache is corrupted. Rebuilding everything...");
      // Return a cache that will invalidate everything.
      return new BuilderCache();
    }
  }

  public static BuilderCache readFromCache(ImmutableList<String> targets) {
    File cacheJson = getCacheJsonFile(targets);
    try (FileReader reader = new FileReader(cacheJson)) {
      return readFromCache(reader);
    } catch (IOException exception) {
      SimpleLogger.warning("Buck builder cache is not found. Rebuilding everything...");
      // Return a cache that will invalidate everything.
      return new BuilderCache();
    }
  }

  @VisibleForTesting
  void writeToCache(Writer writer) {
    new Gson().toJson(this, writer);
  }

  public void writeToCache(ImmutableList<String> targets) {
    File cacheJsonFile = getCacheJsonFile(targets);
    if (cacheJsonFile.exists()) {
      cacheJsonFile.delete();
    }
    cacheJsonFile.getParentFile().mkdirs();
    try (FileWriter writer = new FileWriter(cacheJsonFile)) {
      writeToCache(writer);
    } catch (IOException exception) {
      SimpleLogger.warning("Failed to update builder cache.");
    }
  }

  public long getLastBuiltTime() {
    return lastBuiltTime;
  }

  public Set<ThriftLibraryTarget> getThriftCaches() {
    return thriftCaches;
  }

  @Override
  public String toString() {
    return String.format("{lastBuiltTime=%d, getThriftCaches=%s}", lastBuiltTime, thriftCaches);
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (this == other) {
      return true;
    }
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    BuilderCache builderCache = (BuilderCache) other;
    return lastBuiltTime == builderCache.lastBuiltTime
        && thriftCaches.equals(builderCache.thriftCaches);
  }

  @Override
  public int hashCode() {
    return Objects.hash(lastBuiltTime, thriftCaches);
  }
}
