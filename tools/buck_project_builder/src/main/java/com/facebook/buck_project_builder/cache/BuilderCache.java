/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
import java.util.concurrent.TimeUnit;

public class BuilderCache {

  private final long lastBuiltTime;
  private final Set<ThriftLibraryTarget> thriftCaches;

  public BuilderCache(long lastBuiltTime, Set<ThriftLibraryTarget> thriftCaches) {
    this.lastBuiltTime = lastBuiltTime;
    this.thriftCaches = thriftCaches;
  }

  public BuilderCache() {
    this(0, ImmutableSet.of());
  }

  public static String getCachePath(
      ImmutableList<String> targets, String buckRoot, @Nullable String projectName)
      throws IOException {
    String escapedTargets = DigestUtils.md5Hex(String.join(";", targets));
    if (escapedTargets.length() > 255) {
      // 255 is the Linux filename length limit for EXT4.
      // Most target list is not crazily long, and collision is unlikely to happen.
      escapedTargets = escapedTargets.substring(0, 255);
    }
    return Paths.get(BuilderCache.getBuckBuilderCachePath(buckRoot, projectName), escapedTargets)
        .toString();
  }

  private static File getCacheJsonFile(
      ImmutableList<String> targets, String buckRoot, @Nullable String projectName)
      throws IOException {
    return Paths.get(getCachePath(targets, buckRoot, projectName), "cache.json").toFile();
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

  public static BuilderCache readFromCache(
      ImmutableList<String> targets, String buckRoot, @Nullable String projectName)
      throws IOException {
    File cacheJson = getCacheJsonFile(targets, buckRoot, projectName);
    try (FileReader reader = new FileReader(cacheJson)) {
      return readFromCache(reader);
    } catch (IOException exception) {
      SimpleLogger.warning("Buck builder cache not found. Rebuilding everything...");
      // Return a cache that will invalidate everything.
      return new BuilderCache();
    }
  }

  @VisibleForTesting
  void writeToCache(Writer writer) {
    new Gson().toJson(this, writer);
  }

  public void writeToCache(
      ImmutableList<String> targets, String buckRoot, @Nullable String projectName)
      throws IOException {
    File cacheJsonFile = getCacheJsonFile(targets, buckRoot, projectName);
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

  public static String getBuckBuilderCachePath(String buckRoot, @Nullable String projectName)
      throws IOException {
    String suffix = projectName != null ? String.format("_%s", projectName) : "";
    String cacheDirectoryName = String.format(".buck_builder_cache%s", suffix);
    return Paths.get(ScratchPath.getScratchPath(buckRoot), cacheDirectoryName).toString();
  }

  public static String getLockPath(String buckRoot, @Nullable String projectName)
      throws IOException {
    return Paths.get(getBuckBuilderCachePath(buckRoot, projectName), "builder.lock").toString();
  }

  public static String getThriftCachePath(String buckRoot, @Nullable String projectName)
      throws IOException {
    return Paths.get(getBuckBuilderCachePath(buckRoot, projectName), "thrift-gen").toString();
  }

  public static String getWheelCachePath(String buckRoot, @Nullable String projectName)
      throws IOException {
    return Paths.get(getBuckBuilderCachePath(buckRoot, projectName), "downloaded-wheels")
        .toString();
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
