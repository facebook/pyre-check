package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.gson.JsonParser;
import org.junit.Test;

import javax.annotation.Nullable;

import static org.junit.Assert.assertEquals;

public class BuildTargetsCollectorTest {

  private static final JsonParser JSON_PARSER = new JsonParser();

  private static void assertExpectedParsedBuildTarget(
      String targetJsonString,
      @Nullable BuildTarget expectedTarget,
      @Nullable String cellPath,
      String buckRoot,
      String buildTargetName,
      ImmutableSet<String> requiredRemoteFiles) {
    BuildTarget actualBuiltTarget =
        BuildTargetsCollector.parseBuildTarget(
            JSON_PARSER.parse(targetJsonString).getAsJsonObject(),
            cellPath,
            buckRoot,
            buildTargetName,
            requiredRemoteFiles);
    assertEquals(expectedTarget, actualBuiltTarget);
  }

  private static void assertExpectedParsedBuildTarget(
      String targetJsonString, @Nullable BuildTarget expectedTarget) {
    assertExpectedParsedBuildTarget(
        targetJsonString, expectedTarget, null, ".", "", ImmutableSet.of());
  }

  private static void assertExpectedParsedBuildTargetList(
      String targetsJsonString, ImmutableList<BuildTarget> expectedTargets) {
    ImmutableList<BuildTarget> actualBuiltTarget =
        BuildTargetsCollector.parseBuildTargetList(
            ImmutableMap.of(), ".", JSON_PARSER.parse(targetsJsonString).getAsJsonObject());
    assertEquals(expectedTargets, actualBuiltTarget);
  }

  @Test
  public void normalizeTargetTest() {
    assertEquals("//pyre/...", BuildTargetsCollector.normalizeTarget("//pyre/..."));
    assertEquals("//pyre/...", BuildTargetsCollector.normalizeTarget("pyre/..."));
    assertEquals("c//pyre/...", BuildTargetsCollector.normalizeTarget("c//pyre/..."));
  }

  @Test(expected = BuilderException.class)
  public void emptyTargetListNotAllowed() throws BuilderException {
    BuildTargetsCollector.collectBuckTargets(".", ImmutableList.of());
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
        targetJson, new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py")));
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"srcs\": { \"a.py\": \"a.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson, new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py")));
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_test\",\n"
            + "  \"srcs\": { \"a.py\": \"a.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson, new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py")));

    // We also need to support the case when srcs is an array.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_binary\",\n"
            + "  \"srcs\": [ \"a.py\", \"b.py\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py", "b.py", "b.py")));

    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"base_module\": \"BASE_MODULE\",\n"
            + "  \"buck.type\": \"python_binary\",\n"
            + "  \"srcs\": { \"b.py\": \"a.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson, new PythonTarget("PATH", "BASE_MODULE", ImmutableMap.of("a.py", "b.py")));

    // Python library with versioned sources
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"versioned_srcs\": [[{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3.6\",\n"
            + "      \"//third-party-buck/gcc-5-glibc-2.23/build/python:__project__\": \"3.7\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3.6/a.py\",\n"
            + "      \"b.py\": \"3.6/b.py\"\n"
            + "  }]]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("PATH", null, ImmutableMap.of("3.6/a.py", "a.py", "3.6/b.py", "b.py")));
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"versioned_srcs\": [[{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3.6\",\n"
            + "      \"//third-party-buck/gcc-5-glibc-2.23/build/python:__project__\": \"3.7\"\n"
            + "  }, [\"a.py\", \"b.py\"]]]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py", "b.py", "b.py")));

    // Python library with sources AND versioned sources
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"srcs\": [\"non_version_specific.py\"],\n"
            + "  \"versioned_srcs\": [[{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3.6\",\n"
            + "      \"//third-party-buck/gcc-5-glibc-2.23/build/python:__project__\": \"3.7\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3.6/a.py\",\n"
            + "      \"b.py\": \"3.6/b.py\"\n"
            + "  }]]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget(
            "PATH",
            null,
            ImmutableMap.of(
                "non_version_specific.py",
                "non_version_specific.py",
                "3.6/a.py",
                "a.py",
                "3.6/b.py",
                "b.py")));

    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"versioned_srcs\": [[{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3.6\",\n"
            + "      \"//third-party-buck/gcc-5-glibc-2.23/build/python:__project__\": \"3.7\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3.6/a.py\",\n"
            + "      \"b.py\": \"3.6/b.py\"\n"
            + "  }], [{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3/a.py\",\n"
            + "      \"b.py\": \"3/b.py\"\n"
            + "  }], [{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3.\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3.0/a.py\",\n"
            + "      \"b.py\": \"3.0/b.py\"\n"
            + "  }], [{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3.7\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3.7/a.py\",\n"
            + "      \"b.py\": \"3.7/b.py\"\n"
            + "  }]]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("PATH", null, ImmutableMap.of("3.6/a.py", "a.py", "3.6/b.py", "b.py")));
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"versioned_srcs\": [[{\n"
            + "      \"//third-party-buck/platform007/build/python:python\": \"3.6\",\n"
            + "      \"//third-party-buck/gcc-5-glibc-2.23/build/python:__project__\": \"3.7\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3.6/a.py\",\n"
            + "      \"b.py\": \"3.6/b.py\"\n"
            + "  }], [{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3/a.py\",\n"
            + "      \"b.py\": \"3/b.py\"\n"
            + "  }], [{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3.\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3.0/a.py\",\n"
            + "      \"b.py\": \"3.0/b.py\"\n"
            + "  }], [{\n"
            + "      \"//third-party-buck/platform007/build/python:__project__\": \"3.7\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"3.7/a.py\",\n"
            + "      \"b.py\": \"3.7/b.py\"\n"
            + "  }]]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("PATH", null, ImmutableMap.of("3.7/a.py", "a.py", "3.7/b.py", "b.py")));

    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"versioned_srcs\": [[{\n"
            + "      \"//third-party-buck/gcc-5-glibc-2.23/build/openssl:__project__\": \"1.1\"\n"
            + "  }, {\n"
            + "      \"a.py\": \"1.1/a.py\",\n"
            + "      \"b.py\": \"1.1/b.py\"\n"
            + "  }]]\n"
            + "}";
    assertExpectedParsedBuildTarget(targetJson, null);

    // Python library with platform_srcs
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"platform_srcs\": [[\"py2\", {\n"
            + "      \"a.py\": \"a.py\"\n"
            + "  }]]\n"
            + "}";
    assertExpectedParsedBuildTarget(targetJson, null);
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"platform_srcs\": [[\"py3\", {\n"
            + "      \"a.py\": \"a.py\"\n"
            + "  }]]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson, new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py")));

    // Python library with label=["generated"] should NOT be ignored.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_library\",\n"
            + "  \"labels\": [\"is_fully_translated\", \"generated\"],\n"
            + "  \"srcs\": { \"generated_2.py\": \"generated_1.py\" }\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget("PATH", null, ImmutableMap.of("generated_1.py", "generated_2.py")));

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
        new ThriftLibraryTarget(
            "CMD",
            "PATH",
            ImmutableList.of("./PATH/a.py", "./PATH/b.py")));
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
        new ThriftLibraryTarget(
            "CMD",
            "PATH",
            ImmutableList.of("./PATH/a.pyi", "./PATH/b.pyi")));

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
        new ThriftLibraryTarget(
            "CMD",
            "PATH",
            ImmutableList.of("./PATH/a.pyi", "./PATH/b.pyi")));

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
        new ThriftLibraryTarget(
            "CMD",
            "PATH",
            ImmutableList.of("./PATH/a.py", "./PATH/b.py")));

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

    // Swig library parsing should be supported
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"name\": \"TEST-py-gen\",\n"
            + "  \"buck.type\": \"cxx_genrule\",\n"
            + "  \"cmd\": \"CMD\",\n"
            + "  \"srcs\": [ \"a.i\" ]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson, new SwigLibraryTarget("CMD", ImmutableList.of("./PATH/a.i")));

    // Remote file parsing should be parsed only if base_path + ":" + name is in the set.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"remote_file\",\n"
            + "  \"name\": \"NAME\",\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"url\": \"URL\"\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new RemoteFileTarget("URL"),
        "../path/to",
        ".",
        "PATH:NAME",
        ImmutableSet.of("PATH:NAME"));
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"remote_file\",\n"
            + "  \"name\": \"NAME\",\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"url\": \"URL\"\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson, null, ".", ".", "build-target-name", ImmutableSet.of("PATH:BAD_NAME"));

    // Cell path has impact on build target.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_binary\",\n"
            + "  \"srcs\": [\"a.py\"]\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget(
            "../path/to/", "PATH", null, ImmutableMap.of("a.py", "a.py"), ImmutableSet.of()),
        "../path/to/",
        ".",
        "",
        ImmutableSet.of());

    // Generated source should be detected.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_binary\",\n"
            + "  \"srcs\": {\"a.py\": \"//foo:bar\", \"b.py\": \":foo\"}\n"
            + "}";
    assertExpectedParsedBuildTarget(
        targetJson,
        new PythonTarget(null, "PATH", null, ImmutableMap.of(), ImmutableSet.of("a.py", "b.py")));

    // Unsupported targets should be ignored
    assertExpectedParsedBuildTarget(
        "{\"buck.base_path\": \"PATH\", \"buck.type\": \"random_stuff\"}", null);
    assertExpectedParsedBuildTarget(
        "{\"buck.base_path\": \"PATH\", \"buck.type\": \"java_library\"}", null);
    assertExpectedParsedBuildTarget(
        "{\"buck.base_path\": \"PATH\", \"buck.type\": \"cxx_library\"}", null);
    assertExpectedParsedBuildTarget(
        "{\"buck.base_path\": \"PATH\", \"buck.type\": \"ocaml_library\"}", null);
    assertExpectedParsedBuildTarget(
        "{\"buck.base_path\": \"PATH\", \"buck.type\": \"go_library\"}", null);
  }

  @Test
  public void parseToListOfExpectedBuildTargets() {
    // The first 4 targets should be parsed to python rules.
    // 5th, 6th and 7th target are for remote_file parsing integration test. The remote file should
    // be
    // included because the version matches.
    // The last target should be ignored.
    String targetsJson =
        "{\n"
            + "  \"//target1\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_binary\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"srcs\": {\"a.py\": \"a.py\"}\n"
            + "  },\n"
            + "  \"//target2\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_library\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"srcs\": [\"a.py\", \"b.py\"]\n"
            + "  },\n"
            + "  \"//target3\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_test\",\n"
            + "    \"srcs\": {\"a.py\": \"a.py\"}\n"
            + "  },\n"
            + "  \"//target4\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_binary\",\n"
            + "    \"labels\": [\"is_fully_translated\", \"generated\"],\n"
            + "    \"srcs\": {\"a.py\": \"a.py\"}\n"
            + "  },\n"
            + "  \"//target_for_remote_file:NAME\": {\n"
            + "    \"buck.base_path\": \"target_for_remote_file\",\n"
            + "    \"buck.type\": \"python_library\",\n"
            + "    \"name\": \"NAME\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"platform_deps\": [[\"py3-platform007$\", [\":42.21\"]], [\"BAD\", [\":42.21\"]]]\n"
            + "  },\n"
            + "  \"//target_for_remote_file:42.21\": {\n"
            + "    \"buck.base_path\": \"target_for_remote_file\",\n"
            + "    \"buck.type\": \"python_library\",\n"
            + "    \"name\": \"42.21\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"platform_deps\": [[\"py3-platform007$\", [\":a.whl\"]]]\n"
            + "  },\n"
            + "  \"//target_for_remote_file:a.whl-remote\": {\n"
            + "    \"buck.base_path\": \"target_for_remote_file\",\n"
            + "    \"buck.type\": \"remote_file\",\n"
            + "    \"name\": \"a.whl-remote\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"url\": \"URL1\"\n"
            + "  },\n"
            + "  \"//target_for_remote_file:42.21-BAD.whl-remote\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"remote_file\",\n"
            + "    \"name\": \"NAME-42.21-BAD.whl-remote\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"url\": \"URL2\"\n"
            + "  },\n"
            + "  \"//target2_to_be_ignored\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"go_binary\"\n"
            + "  }\n"
            + "}\n";
    assertExpectedParsedBuildTargetList(
        targetsJson,
        ImmutableList.of(
            new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py")),
            new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py", "b.py", "b.py")),
            new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py")),
            new PythonTarget("PATH", null, ImmutableMap.of("a.py", "a.py")),
            new RemoteFileTarget("URL1")));
  }
}
