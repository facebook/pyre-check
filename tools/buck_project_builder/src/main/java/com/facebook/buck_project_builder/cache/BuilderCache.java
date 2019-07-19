package com.facebook.buck_project_builder.cache;

import com.facebook.buck_project_builder.SimpleLogger;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import javax.annotation.Nullable;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

public final class BuilderCache {

  private final long lastBuiltTime;
  private final Set<ThriftBuildCommand> thriftCaches;

  public BuilderCache(long lastBuiltTime, Set<ThriftBuildCommand> thriftCaches) {
    this.lastBuiltTime = lastBuiltTime;
    this.thriftCaches = thriftCaches;
  }

  public BuilderCache() {
    this(0, ImmutableSet.of());
  }

  public static String getCachePath(ImmutableList<String> targets) {
    String escapedTargets =
        targets.stream()
            .map(target -> target.replaceAll("/", "_"))
            .collect(Collectors.joining("-"));
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

  public static BuilderCache readFromCache(ImmutableList<String> targets) {
    File cacheJson = getCacheJsonFile(targets);
    try (FileReader reader = new FileReader(cacheJson)) {
      return new Gson().fromJson(reader, BuilderCache.class);
    } catch (IOException | JsonSyntaxException exception) {
      SimpleLogger.warning(
          "Buck builder cache is not found or corrupted. Rebuilding everything...");
      // Return a cache that will invalidate everything.
      return new BuilderCache();
    }
  }

  public void writeToCache(ImmutableList<String> targets) {
    File cacheJsonFile = getCacheJsonFile(targets);
    if (cacheJsonFile.exists()) {
      cacheJsonFile.delete();
    }
    cacheJsonFile.getParentFile().mkdirs();
    try (FileWriter writer = new FileWriter(cacheJsonFile)) {
      new Gson().toJson(this, writer);
    } catch (IOException exception) {
      SimpleLogger.warning("Failed to update builder cache.");
    }
  }

  public long getLastBuiltTime() {
    return lastBuiltTime;
  }

  public Set<ThriftBuildCommand> getThriftCaches() {
    return thriftCaches;
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
