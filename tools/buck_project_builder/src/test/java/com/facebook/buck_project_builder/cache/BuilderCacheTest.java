package com.facebook.buck_project_builder.cache;

import com.facebook.buck_project_builder.SimpleLogger;
import com.facebook.buck_project_builder.targets.ThriftLibraryTarget;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.junit.runner.RunWith;
import org.junit.Test;

import org.powermock.api.mockito.PowerMockito.*;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

@RunWith(PowerMockRunner.class)
@PrepareForTest(ScratchPath.class)
public class BuilderCacheTest {
  public static final String DUMMY_BUCK_ROOT = "/dummy-buck-root";

  @Test
  public void getCachePathTest() throws IOException {
    String scratchPath =
        Paths.get(BuilderCacheTest.DUMMY_BUCK_ROOT, "scratch/fooZjohnsmith/pyre").toString();
    String builderCachePath = Paths.get(scratchPath, ".buck_builder_cache").toString();

    PowerMockito.mockStatic(ScratchPath.class);
    PowerMockito.when(ScratchPath.getScratchPath(BuilderCacheTest.DUMMY_BUCK_ROOT))
        .thenReturn(scratchPath);
    assertEquals(
        Paths.get(builderCachePath, "6ff3f6302227d54ed823ac603a57d864").toString(),
        BuilderCache.getCachePath(ImmutableList.of("//abc"), BuilderCacheTest.DUMMY_BUCK_ROOT));
    assertEquals(
        Paths.get(builderCachePath, "b8565123512e859863e40feb6d46143e").toString(),
        BuilderCache.getCachePath(
            ImmutableList.of("//abc", "//def:efg"), BuilderCacheTest.DUMMY_BUCK_ROOT));
    assertEquals(
        Paths.get(builderCachePath, "cabe45dcc9ae5b66ba86600cca6b8ba8").toString(),
        BuilderCache.getCachePath(
            ImmutableList.of(Strings.repeat("a", 1000)), BuilderCacheTest.DUMMY_BUCK_ROOT));
  }

  @Test
  public void cacheReadWriteTest() {
    // Use a broken reader to simulate bad cache.
    assertEquals(
        "Expect empty cache if cache not found.",
        new BuilderCache(),
        BuilderCache.readFromCache(
            new Reader() {
              @Override
              public int read(char[] cbuf, int off, int len) throws IOException {
                throw new IOException();
              }

              @Override
              public void close() throws IOException {
                throw new IOException();
              }
            }));

    ThriftLibraryTarget thriftLibraryTarget =
        new ThriftLibraryTarget("CMD", "PATH", ImmutableList.of("a.thrift"));
    StringWriter stringWriter = new StringWriter();
    new BuilderCache(42, ImmutableSet.of(thriftLibraryTarget)).writeToCache(stringWriter);
    StringReader stringReader = new StringReader(stringWriter.toString());
    assertEquals(
        "Expect to read the same cache content that has just been written",
        new BuilderCache(42, ImmutableSet.of(thriftLibraryTarget)),
        BuilderCache.readFromCache(stringReader));
  }

  @Test
  public void brokenCacheJsonInvalidatesCacheTest() {
    assertEquals(
        "Broken cache content invalidates everything",
        new BuilderCache(),
        BuilderCache.readFromCache(new StringReader("broken")));
  }
}
