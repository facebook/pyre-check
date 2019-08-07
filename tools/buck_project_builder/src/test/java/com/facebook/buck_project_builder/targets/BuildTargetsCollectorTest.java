package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.junit.Test;

import java.nio.file.Paths;

import static com.facebook.buck_project_builder.targets.BuildTargetTestCommon.BUCK_ROOT;
import static com.facebook.buck_project_builder.targets.BuildTargetTestCommon.JSON_PARSER;
import static com.facebook.buck_project_builder.targets.BuildTargetTestCommon.OUTPUT_DIRECTORY;
import static org.junit.Assert.assertEquals;

public class BuildTargetsCollectorTest {

  private static BuildTargetsCollector collectTargetsFromJson(String targetsJsonString) {
    BuildTargetsCollector collector = new BuildTargetsCollector(BUCK_ROOT, OUTPUT_DIRECTORY);
    collector.collectBuildTargets(
        ImmutableMap.of(), JSON_PARSER.parse(targetsJsonString).getAsJsonObject());
    return collector;
  }

  @Test(expected = BuilderException.class)
  public void emptyTargetListNotAllowed() throws BuilderException {
    new BuildTargetsCollector(BUCK_ROOT, OUTPUT_DIRECTORY).getBuilder(0, ImmutableList.of());
  }

  @Test
  public void unsupportedTargetsShouldBeIgnored() {
    // Unsupported targets should be ignored
    String targetsJson =
        "{\n"
            + "  \"//target1\": {\"buck.base_path\": \"PATH\", \"buck.type\": \"random_stuff\"},\n"
            + "  \"//target2\": {\"buck.base_path\": \"PATH\", \"buck.type\": \"java_library\"},\n"
            + "  \"//target3\": {\"buck.base_path\": \"PATH\", \"buck.type\": \"cxx_library\"},\n"
            + "  \"//target4\": {\"buck.base_path\": \"PATH\", \"buck.type\": \"ocaml_library\"},\n"
            + "  \"//target5\": {\"buck.base_path\": \"PATH\", \"buck.type\": \"go_library\"}\n"
            + "}\n";
    BuildTargetsCollector collector = collectTargetsFromJson(targetsJson);
    assertEquals(ImmutableMap.of(), collector.getSources());
    assertEquals(ImmutableSet.of(), collector.getUnsupportedGeneratedSources());
    assertEquals(ImmutableSet.of(), collector.getPythonWheelUrls());
    assertEquals(ImmutableSet.of(), collector.getThriftLibraryTargets());
    assertEquals(ImmutableSet.of(), collector.getSwigLibraryBuildCommands());
    assertEquals(ImmutableSet.of(), collector.getAntlr4LibraryBuildCommands());
    assertEquals(ImmutableSet.of(), collector.getConflictingFiles());
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
            + "    \"srcs\": {\"a.py\": \"b.py\"}\n"
            + "  },\n"
            + "  \"//target2\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_library\",\n"
            + "    \"labels\": [\"is_fully_translated\"],\n"
            + "    \"srcs\": [\"b.py\", \"c.py\"]\n"
            + "  },\n"
            + "  \"//target3\": {\n"
            + "    \"buck.base_path\": \"PATH\",\n"
            + "    \"buck.type\": \"python_test\",\n"
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
            + "    \"url\": \"URL\"\n"
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
    BuildTargetsCollector collector = collectTargetsFromJson(targetsJson);
    assertEquals(
        ImmutableMap.of(
            Paths.get("./PATH/a.py"),
            Paths.get("./PATH/b.py"),
            Paths.get("./PATH/b.py"),
            Paths.get("./PATH/b.py"),
            Paths.get("./PATH/c.py"),
            Paths.get("./PATH/c.py")),
        collector.getSources());
    assertEquals(ImmutableSet.of("URL"), collector.getPythonWheelUrls());
    assertEquals(ImmutableSet.of("PATH/a.py"), collector.getConflictingFiles());
  }
}
