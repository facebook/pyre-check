package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuckCells;
import com.facebook.buck_project_builder.BuilderException;
import com.facebook.buck_project_builder.CommandLine;
import com.facebook.buck_project_builder.SimpleLogger;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public final class BuildTargetsCollector {

  /**
   * @return an array that contains all of the buck targets (including the dependencies), given a
   *     list of targets we need to type check
   */
  public static ImmutableList<BuildTarget> collectBuckTargets(
      String buckRoot, ImmutableList<String> targets) throws BuilderException {
    return parseBuildTargetList(BuckCells.getCellMappings(), buckRoot, getBuildTargetJson(targets));
  }

  /**
   * Exposed for testing. Do not call it directly.
   *
   * @return a list of parsed json targets from target json.
   */
  static ImmutableList<BuildTarget> parseBuildTargetList(
      ImmutableMap<String, String> cellMappings, String buckRoot, JsonObject targetJsonMap) {
    Set<String> requiredRemoteFiles = new HashSet<>();
    // The first pass collects python_library that refers to a remote python_file
    for (Map.Entry<String, JsonElement> entry : targetJsonMap.entrySet()) {
      JsonObject targetJsonObject = entry.getValue().getAsJsonObject();
      addRemotePythonFiles(targetJsonObject, requiredRemoteFiles, targetJsonMap);
    }
    ImmutableSet<String> immutableRequiredRemoteFiles = ImmutableSet.copyOf(requiredRemoteFiles);
    // The second pass parses all normal targets and remote_file target with version information
    // collected above.
    ImmutableList.Builder<BuildTarget> buildTargetListBuilder = ImmutableList.builder();
    for (Map.Entry<String, JsonElement> entry : targetJsonMap.entrySet()) {
      String buildTargetName = entry.getKey();
      String cellPath = BuckCells.getCellPath(buildTargetName, cellMappings);
      JsonObject targetJsonObject = entry.getValue().getAsJsonObject();
      BuildTarget parsedTarget =
          parseBuildTarget(
              targetJsonObject, cellPath, buckRoot, buildTargetName, immutableRequiredRemoteFiles);
      if (parsedTarget != null) {
        buildTargetListBuilder.add(parsedTarget);
      }
    }
    return buildTargetListBuilder.build();
  }

  private static @Nullable String getSupportedPlatformDependency(
      JsonElement platformDependenciesField) {
    for (JsonElement platformDependencyTuple : platformDependenciesField.getAsJsonArray()) {
      JsonArray pair = platformDependencyTuple.getAsJsonArray();
      if (!pair.get(0).getAsString().equals("py3-platform007$")) {
        continue;
      }
      return pair.get(1).getAsJsonArray().get(0).getAsString();
    }
    return null;
  }

  private static void addRemotePythonFiles(
      JsonObject targetJsonObject, Set<String> requiredRemoteFiles, JsonObject targetJsonMap) {
    if (!targetJsonObject.get("buck.type").getAsString().equals("python_library")
        || targetJsonObject.get("deps") != null) {
      // remote_file's corresponding top level python_library must not have deps field.
      return;
    }
    JsonArray platformDependenciesField = targetJsonObject.getAsJsonArray("platform_deps");
    if (platformDependenciesField == null) {
      return;
    }
    String versionedSuffix = getSupportedPlatformDependency(platformDependenciesField);
    if (versionedSuffix == null) {
      return;
    }
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    JsonObject versionedTarget = targetJsonMap.getAsJsonObject("//" + basePath + versionedSuffix);
    if (versionedTarget == null) {
      return;
    }
    String wheelSuffix = getSupportedPlatformDependency(versionedTarget.get("platform_deps"));
    if (wheelSuffix == null) {
      return;
    }
    requiredRemoteFiles.add("//" + basePath + wheelSuffix + "-remote");
  }

  /**
   * Exposed for testing. Do not call it directly.
   *
   * @return the parsed build target, or null if it is a non-python related target.
   */
  static @Nullable BuildTarget parseBuildTarget(
      JsonObject targetJsonObject,
      @Nullable String cellPath,
      String buckRoot,
      String buildTargetName,
      ImmutableSet<String> requiredRemoteFiles) {
    String type = targetJsonObject.get("buck.type").getAsString();
    switch (type) {
      case "python_binary":
      case "python_library":
      case "python_test":
        return PythonTarget.parse(cellPath, targetJsonObject);
      case "genrule":
        // Thrift library targets have genrule rule type.
        BuildTarget parsedTarget = ThriftLibraryTarget.parse(cellPath, buckRoot, targetJsonObject);
        if (parsedTarget != null) {
          return parsedTarget;
        }
        return Antlr4LibraryTarget.parse(cellPath, buckRoot, targetJsonObject);
      case "cxx_genrule":
        // Swig library targets have cxx_genrule rule type.
        return SwigLibraryTarget.parse(cellPath, buckRoot, targetJsonObject);
      case "remote_file":
        return requiredRemoteFiles.contains(buildTargetName)
            ? RemoteFileTarget.parse(targetJsonObject)
            : null;
      default:
        return null;
    }
  }

  private static JsonObject getBuildTargetJson(ImmutableList<String> targets)
      throws BuilderException {
    if (targets.isEmpty()) {
      throw new BuilderException("Targets should not be empty.");
    }
    SimpleLogger.info("Querying targets' information...");
    long start = System.currentTimeMillis();
    ImmutableList<String> buildCommand = getBuildCommand(targets);
    try (InputStream commandLineOutput = CommandLine.getCommandLineOutput(buildCommand)) {
      JsonElement parsedJson = new JsonParser().parse(new InputStreamReader(commandLineOutput));
      long buckQueryTime = System.currentTimeMillis() - start;
      SimpleLogger.info("Found targets' information in " + buckQueryTime + "ms.");
      if (!parsedJson.isJsonObject()) {
        throw new BuilderException(
            String.format(
                "Unexpected `buck query` output. It should always be a json object.\nBad json: %s. Query: %s.",
                parsedJson, String.join(" ", buildCommand)));
      }
      return parsedJson.getAsJsonObject();
    } catch (IOException exception) {
      throw new BuilderException(
          "Cannot compute all targets to build due to IO Exception. Reason: "
              + exception.getMessage());
    } catch (JsonSyntaxException exception) {
      throw new BuilderException(
          "Unexpected JSON syntax error in produced buck targets. Reason: "
              + exception.getMessage());
    }
  }

  static String normalizeTarget(String target) {
    if (target.contains("//")) {
      return target;
    }
    return "//" + target;
  }

  private static ImmutableList<String> getBuildCommand(ImmutableList<String> targets) {
    /*
     * The command that we will run has the form:
     *
     * buck query \
     *   "kind([all build rule types we have to support], deps(%s))" [TARGETS]
     *   --output-attributes [all attributes in the json that we care about]
     *
     * We use
     * - `kind` to filter by the types of build rules
     * - `deps(%s)` to get all transitive dependencies
     * - `--output-attributes` to get only wanted fields in json
     *
     * See: https://buck.build/command/query.html for more detail.
     */
    return ImmutableList.<String>builder()
        .add("buck")
        .add("query")
        .add(
            "kind('python_binary|python_library|python_test|genrule|cxx_genrule|remote_file', deps(%s))")
        .addAll(
            targets.stream()
                .map(BuildTargetsCollector::normalizeTarget)
                .collect(Collectors.toList()))
        .add("--output-attributes")
        .add("buck.type")
        .add("buck.base_path")
        .add("base_module")
        .add("labels")
        .add("srcs")
        .add("versioned_srcs")
        .add("platform_srcs")
        .add("cmd")
        .add("url")
        .add("binary_src")
        .add("name")
        .add("deps")
        .add("platform_deps")
        .build();
  }
}
