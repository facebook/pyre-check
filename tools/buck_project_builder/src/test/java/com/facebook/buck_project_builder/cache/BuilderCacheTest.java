package com.facebook.buck_project_builder.cache;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.io.FileUtils;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class BuilderCacheTest {

  @Test
  public void getCachePathTest() {
    assertEquals(
        "/tmp/pyre/buck_builder_cache/__abc", BuilderCache.getCachePath(ImmutableList.of("//abc")));
    assertEquals(
        "/tmp/pyre/buck_builder_cache/__abc-__def:efg",
        BuilderCache.getCachePath(ImmutableList.of("//abc", "//def:efg")));
    assertEquals(
        "/tmp/pyre/buck_builder_cache/" + Strings.repeat("a", 255),
        BuilderCache.getCachePath(ImmutableList.of(Strings.repeat("a", 1000))));
  }

  @Test
  public void cacheReadWriteTest() throws IOException {
    FileUtils.deleteDirectory(new File("/tmp/pyre/buck_builder_cache/__target"));
    assertEquals(
        "Expect empty cache if cache is not found.",
        new BuilderCache(),
        BuilderCache.readFromCache(ImmutableList.of("//target")));

    ThriftBuildCommand thriftBuildCommand =
        new ThriftBuildCommand("CMD", "PATH", ImmutableList.of("a.thrift"));
    new BuilderCache(42, ImmutableSet.of(thriftBuildCommand))
        .writeToCache(ImmutableList.of("//target"));
    assertEquals(
        "Expect to read the same cache content that has just been written",
        new BuilderCache(42, ImmutableSet.of(thriftBuildCommand)),
        BuilderCache.readFromCache(ImmutableList.of("//target")));

    new BuilderCache(65536, ImmutableSet.of(thriftBuildCommand))
        .writeToCache(ImmutableList.of("//target"));
    assertEquals(
        "Expect to read a different cache content that has just been overridden",
        new BuilderCache(65536, ImmutableSet.of(thriftBuildCommand)),
        BuilderCache.readFromCache(ImmutableList.of("//target")));

    FileUtils.deleteDirectory(new File("/tmp/pyre/buck_builder_cache/__target"));
  }
}
