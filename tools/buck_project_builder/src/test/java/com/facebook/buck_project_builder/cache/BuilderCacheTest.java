package com.facebook.buck_project_builder.cache;

import com.facebook.buck_project_builder.BuilderException;
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
import static org.junit.Assert.fail;

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
  public void cacheReadWriteTest() throws BuilderException {
    CacheLock.synchronize(
        () -> {
          FileUtils.deleteQuietly(
              new File("/tmp/pyre/buck_builder_cache/6ff3f6302227d54ed823ac603a57d864"));
          assertEquals(
              "Expect empty cache if cache is not found.",
              new BuilderCache(),
              BuilderCache.readFromCache(ImmutableList.of("//abc")));

          ThriftLibraryTarget thriftLibraryTarget =
              new ThriftLibraryTarget("CMD", "PATH", ImmutableList.of("a.thrift"));
          new BuilderCache(42, ImmutableSet.of(thriftLibraryTarget))
              .writeToCache(ImmutableList.of("//abc"));
          assertEquals(
              "Expect to read the same cache content that has just been written",
              new BuilderCache(42, ImmutableSet.of(thriftLibraryTarget)),
              BuilderCache.readFromCache(ImmutableList.of("//abc")));

          new BuilderCache(65536, ImmutableSet.of(thriftLibraryTarget))
              .writeToCache(ImmutableList.of("//abc"));
          assertEquals(
              "Expect to read a different cache content that has just been overridden",
              new BuilderCache(65536, ImmutableSet.of(thriftLibraryTarget)),
              BuilderCache.readFromCache(ImmutableList.of("//abc")));

          FileUtils.deleteQuietly(
              new File("/tmp/pyre/buck_builder_cache/6ff3f6302227d54ed823ac603a57d864"));
        });
  }

  @Test
  public void brokenCacheJsonInvalidatesCacheTest() throws BuilderException {
    CacheLock.synchronize(
        () -> {
          String targetHash = DigestUtils.md5Hex("//target2");
          File temporaryBadJsonFile =
              Paths.get("/tmp/pyre/buck_builder_cache", targetHash, "cache.json").toFile();
          File temporaryTestCacheDirectory = temporaryBadJsonFile.getParentFile();
          temporaryTestCacheDirectory.mkdirs();

          try {
            FileUtils.writeStringToFile(temporaryBadJsonFile, "broken", Charset.defaultCharset());
          } catch (IOException exception) {
            fail(exception.getMessage());
          }
          assertEquals(
              new BuilderCache(), BuilderCache.readFromCache(ImmutableList.of("//target2")));

          FileUtils.deleteQuietly(temporaryTestCacheDirectory);
        });
  }
}
