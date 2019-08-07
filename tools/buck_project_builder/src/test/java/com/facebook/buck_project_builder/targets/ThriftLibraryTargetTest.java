package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableList;
import org.junit.Test;

import javax.annotation.Nullable;

import static com.facebook.buck_project_builder.targets.BuildTargetTestCommon.BUCK_ROOT;
import static com.facebook.buck_project_builder.targets.BuildTargetTestCommon.JSON_PARSER;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class ThriftLibraryTargetTest {

  private static void assertExpectedParsedBuildTarget(
      String targetJsonString, @Nullable ThriftLibraryTarget expectedTarget) {
    assertEquals(
        expectedTarget,
        ThriftLibraryTarget.parse(
            null, BUCK_ROOT, JSON_PARSER.parse(targetJsonString).getAsJsonObject()));
  }

  @Test
  public void parseTest() {
    String targetJson;
    // Thrift library parsing should be supported, including those with extensions
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"genrule\",\n"
            + "  \"cmd\": \"CMD && if [ ! -f $OUT/gen-py/PATH/ttypes.pyi ]; CMD CMD\",\n"
            + "  \"labels\": [\"generated\", \"thrift_library\", \"thrift_library=py/compile\"],\n"
            + "  \"srcs\": [ \"a.py\", \"b.py\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new ThriftLibraryTarget("CMD", "PATH", ImmutableList.of("./PATH/a.py", "./PATH/b.py")));
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"genrule\",\n"
            + "  \"cmd\": \"CMD\",\n"
            + "  \"labels\": [\"generated\", \"thrift_library\", \"thrift_library=py/compile\"],\n"
            + "  \"srcs\": [ \"a.py\", \"b.py\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(targetJson, null);

    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"genrule\",\n"
            + "  \"cmd\": \"CMD && if [ ! -f $OUT/gen-py/PATH/ttypes.pyi ]; CMD CMD\",\n"
            + "  \"labels\": [\"generated\", \"thrift_library\", \"thrift_library=pyi/compile\"],\n"
            + "  \"srcs\": [ \"a.pyi\", \"b.pyi\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new ThriftLibraryTarget("CMD", "PATH", ImmutableList.of("./PATH/a.pyi", "./PATH/b.pyi")));

    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"genrule\",\n"
            + "  \"cmd\": \"CMD && if [ ! -f $OUT/gen-py/PATH/ttypes.pyi ]; CMD CMD\",\n"
            + "  \"labels\": [\"generated\", \"thrift_library\", \"thrift_library=py3/compile\"],\n"
            + "  \"srcs\": [ \"a.pyi\", \"b.pyi\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new ThriftLibraryTarget("CMD", "PATH", ImmutableList.of("./PATH/a.pyi", "./PATH/b.pyi")));

    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"genrule\",\n"
            + "  \"cmd\": \"CMD && if [ ! -f $OUT/gen-py/PATH/ttypes.pyi ]; CMD CMD\",\n"
            + "  \"labels\": [\"generated\", \"thrift_library\", \"thrift_library=py-extension/compile\"],\n"
            + "  \"srcs\": [ \"a.py\", \"b.py\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new ThriftLibraryTarget("CMD", "PATH", ImmutableList.of("./PATH/a.py", "./PATH/b.py")));

    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"genrule\",\n"
            + "  \"cmd\": \"CMD\",\n"
            + "  \"labels\": [\"generated\", \"thrift_library\", \"thrift_library=py3/compile\"],\n"
            + "  \"srcs\": [ \"//target/a.py\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(targetJson, null);
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"genrule\",\n"
            + "  \"cmd\": \"CMD\",\n"
            + "  \"labels\": [\"generated\", \"thrift_library\", \"thrift_library=py3/compile\"],\n"
            + "  \"srcs\": [ \":a.py\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(targetJson, null);
  }

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
