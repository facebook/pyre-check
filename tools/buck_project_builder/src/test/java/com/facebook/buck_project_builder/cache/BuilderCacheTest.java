package com.facebook.buck_project_builder.cache;

import com.facebook.buck_project_builder.targets.ThriftLibraryTarget;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.junit.Test;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

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
  public void cacheReadWriteTest() {
    // Use a broken reader to simulate bad cache.
    assertEquals(
        "Expect empty cache if cache is not found.",
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
