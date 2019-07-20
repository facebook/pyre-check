package com.facebook.buck_project_builder.cache;

import com.facebook.buck_project_builder.targets.ThriftLibraryTarget;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FileUtils;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

public class BuilderCacheTest {

  @Test
  public void getCachePathTest() {
    assertEquals(
        "/tmp/pyre/buck_builder_cache/6ff3f6302227d54ed823ac603a57d864",
        BuilderCache.getCachePath(ImmutableList.of("//abc")));
    assertEquals(
        "/tmp/pyre/buck_builder_cache/b8565123512e859863e40feb6d46143e",
        BuilderCache.getCachePath(ImmutableList.of("//abc", "//def:efg")));
    assertEquals(
        "/tmp/pyre/buck_builder_cache/cabe45dcc9ae5b66ba86600cca6b8ba8",
        BuilderCache.getCachePath(ImmutableList.of(Strings.repeat("a", 1000))));
  }

  @Test
  public void cacheReadWriteTest() throws IOException {
    FileUtils.deleteDirectory(new File("/tmp/pyre/buck_builder_cache/__target"));
    assertEquals(
        "Expect empty cache if cache is not found.",
        new BuilderCache(),
        BuilderCache.readFromCache(ImmutableList.of("//target")));

    ThriftLibraryTarget thriftLibraryTarget =
        new ThriftLibraryTarget("CMD", "PATH", ImmutableList.of("a.thrift"));
    new BuilderCache(42, ImmutableSet.of(thriftLibraryTarget))
        .writeToCache(ImmutableList.of("//target"));
    assertEquals(
        "Expect to read the same cache content that has just been written",
        new BuilderCache(42, ImmutableSet.of(thriftLibraryTarget)),
        BuilderCache.readFromCache(ImmutableList.of("//target")));

    new BuilderCache(65536, ImmutableSet.of(thriftLibraryTarget))
        .writeToCache(ImmutableList.of("//target"));
    assertEquals(
        "Expect to read a different cache content that has just been overridden",
        new BuilderCache(65536, ImmutableSet.of(thriftLibraryTarget)),
        BuilderCache.readFromCache(ImmutableList.of("//target")));

    FileUtils.deleteDirectory(new File("/tmp/pyre/buck_builder_cache/__target"));
  }

  @Test
  public void brokenCacheJsonInvalidatesCacheTest() throws IOException {
    String targetHash = DigestUtils.md5Hex("//target2");
    File temporaryBadJsonFile =
        Paths.get("/tmp/pyre/buck_builder_cache", targetHash, "cache.json").toFile();
    File temporaryTestCacheDirectory = temporaryBadJsonFile.getParentFile();
    temporaryTestCacheDirectory.mkdirs();

    FileUtils.writeStringToFile(temporaryBadJsonFile, "broken", Charset.defaultCharset());
    assertEquals(new BuilderCache(), BuilderCache.readFromCache(ImmutableList.of("//target2")));

    FileUtils.deleteDirectory(temporaryTestCacheDirectory);
  }
}
