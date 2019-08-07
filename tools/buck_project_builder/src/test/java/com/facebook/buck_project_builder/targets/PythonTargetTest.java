package com.facebook.buck_project_builder.targets;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.junit.Test;

import javax.annotation.Nullable;

import static com.facebook.buck_project_builder.targets.BuildTargetTestCommon.JSON_PARSER;
import static org.junit.Assert.assertEquals;

public class PythonTargetTest {

  private static void assertExpectedParsedBuildTarget(
      String targetJsonString, @Nullable String cellPath, @Nullable PythonTarget expectedTarget) {
    assertEquals(
        expectedTarget,
        PythonTarget.parse(cellPath, JSON_PARSER.parse(targetJsonString).getAsJsonObject()));
  }

  private static void assertExpectedParsedBuildTarget(
      String targetJsonString, @Nullable PythonTarget expectedTarget) {
    assertExpectedParsedBuildTarget(targetJsonString, null, expectedTarget);
  }

  @Test
  public void parseTest() {
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

    // Cell path has impact on build target.
    targetJson =
        "{\n"
            + "  \"buck.base_path\": \"PATH\",\n"
            + "  \"buck.type\": \"python_binary\",\n"
            + "  \"srcs\": [\"a.py\"]\n"
            + "}";

    assertExpectedParsedBuildTarget(
        targetJson,
        "../path/to/",
        new PythonTarget(
            "../path/to/", "PATH", null, ImmutableMap.of("a.py", "a.py"), ImmutableSet.of()));

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
  }
}
