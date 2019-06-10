package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.gson.JsonParser;
import org.junit.Test;

import javax.annotation.Nullable;

import static org.junit.Assert.assertEquals;

public class BuildTargetsCollectorTest {

  private static final JsonParser JSON_PARSER = new JsonParser();

  private static void assertExpectedParsedBuildTarget(
      String targetJsonString, @Nullable PythonTarget expectedTarget) {
    PythonTarget actualBuiltTarget =
        BuildTargetsCollector.parseBuildTarget(
            JSON_PARSER.parse(targetJsonString).getAsJsonObject());
    assertEquals(expectedTarget, actualBuiltTarget);
  }

  private static void assertExpectedParsedBuildTargetList(
      String targetsJsonString, ImmutableList<PythonTarget> expectedTargets) {
    ImmutableList<PythonTarget> actualBuiltTarget =
        BuildTargetsCollector.parseBuildTargetList(
            JSON_PARSER.parse(targetsJsonString).getAsJsonObject());
    assertEquals(expectedTargets, actualBuiltTarget);
  }

  @Test(expected = BuilderException.class)
  public void emptyTargetListNotAllowed() throws BuilderException {
    BuildTargetsCollector.collectBuckTargets(ImmutableList.of());
  }

  @Test
  public void parseToSingleExpectedBuildTarget() {
    String targetJson;
    // Basic python_binary, python_library, python_test example.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_binary\",\n"
            + "  \"labels\": [\"is_fully_translated\"],\n"
            + "  \"srcs\": { \"a.py\": \"a.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("python_binary", "PATH", null, ImmutableMap.of("a.py", "a.py")));
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"srcs\": { \"a.py\": \"a.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("python_library", "PATH", null, ImmutableMap.of("a.py", "a.py")));
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_test\",\n"
            + "  \"srcs\": { \"a.py\": \"a.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson, new PythonTarget("python_test", "PATH", null, ImmutableMap.of("a.py", "a.py")));

    // We also need to support the case when srcs is an array.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_binary\",\n"
            + "  \"srcs\": [ \"a.py\", \"b.py\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget(
            "python_binary", "PATH", null, ImmutableMap.of("a.py", "a.py", "b.py", "b.py")));

    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"base_module\": \"BASE_MODULE\",\n"
            + "  \"buck.type\": \"python_binary\",\n"
            + "  \"srcs\": { \"a.py\": \"b.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("python_binary", "PATH", "BASE_MODULE", ImmutableMap.of("a.py", "b.py")));

    // Python library with label=["generated"] should NOT be ignored.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"labels\": [\"is_fully_translated\", \"generated\"],\n"
            + "  \"srcs\": { \"generated_1.py\": \"generated_2.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget(
            "python_library", "PATH", null, ImmutableMap.of("generated_1.py", "generated_2.py")));

    // Unsupported targets should be ignored
    assertExpectedParsedBuildTarget("{\"buck.type\": \"random_stuff\"}", null);
    assertExpectedParsedBuildTarget("{\"buck.type\": \"java_library\"}", null);
    assertExpectedParsedBuildTarget("{\"buck.type\": \"cxx_library\"}", null);
    assertExpectedParsedBuildTarget("{\"buck.type\": \"ocaml_library\"}", null);
    assertExpectedParsedBuildTarget("{\"buck.type\": \"go_library\"}", null);
  }

  @Test
  public void parseToListOfExpectedBuildTargets() {
    // The first 4 targets should be parsed to python rules, and the last one should be ignored.
    String targetsJson =
        "{\n"
            + "  \"target1\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_binary\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"srcs\": {\"a.py\": \"a.py\"}\n"
            + "  },\n"
            + "  \"target2\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_library\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"srcs\": [\"a.py\", \"b.py\"]\n"
            + "  },\n"
            + "  \"target3\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_test\",\n"
            + "    \"srcs\": {\"a.py\": \"a.py\"}\n"
            + "  },\n"
            + "  \"target4\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_binary\",\n"
            + "    \"labels\": [\"is_fully_translated\", \"generated\"],\n"
            + "    \"srcs\": {\"a.py\": \"a.py\"}\n"
            + "  },\n"
            + "  \"target2_to_be_ignored\": {\n"
            + "    \"buck.type\": \"go_binary\"\n"
            + "  }\n"
            + "}\n";
    assertExpectedParsedBuildTargetList(
        targetsJson,
        ImmutableList.of(
            new PythonTarget("python_binary", "PATH", null, ImmutableMap.of("a.py", "a.py")),
            new PythonTarget(
                "python_library", "PATH", null, ImmutableMap.of("a.py", "a.py", "b.py", "b.py")),
            new PythonTarget("python_test", "PATH", null, ImmutableMap.of("a.py", "a.py")),
            new PythonTarget("python_binary", "PATH", null, ImmutableMap.of("a.py", "a.py"))));
  }
}
