package com.facebook.buck_project_builder.targets;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class ThriftLibraryTargetTest {

  @Test
  public void getBaseModulePathTest() {
    assertEquals(
        "PATH",
        ThriftLibraryTarget.extractBaseModulePath(
            "CMD && if [ ! -f $OUT/gen-py/PATH/ttypes.pyi ]; CMD CMD"));
    assertEquals(
        "PATH",
        ThriftLibraryTarget.extractBaseModulePath(
            "CMD && if [ ! -f $OUT/gen-py/PATH/ttypes.py ]; CMD CMD"));
    assertEquals(
        "PATH",
        ThriftLibraryTarget.extractBaseModulePath(
            "CMD && if [ ! -f $OUT/gen-py/PATH/ttypes.py ]; echo /gen-py/PATH/ttypes.py"));
    assertEquals(
        "PATH",
        ThriftLibraryTarget.extractBaseModulePath(
            "CMD && if [ ! -f $OUT/gen-py3/PATH/types.pyx ]; echo /gen-py3/PATH/types.pyx"));
    assertEquals(
        "some/base/path",
        ThriftLibraryTarget.extractBaseModulePath(
            "CMD && if [ ! -f $OUT/gen-py/some/base/path/ttypes.pyi ]; CMD CMD"));
    assertNull(ThriftLibraryTarget.extractBaseModulePath("CMD"));
    assertNull(
        ThriftLibraryTarget.extractBaseModulePath(
            "CMD && if [ ! -f $OUT/gen-py/ttypes.pyi ]; CMD CMD"));
  }
}
