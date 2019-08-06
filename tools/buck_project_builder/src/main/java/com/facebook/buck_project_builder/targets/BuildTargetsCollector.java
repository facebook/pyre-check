package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuckCells;
import com.facebook.buck_project_builder.BuckQuery;
import com.facebook.buck_project_builder.BuilderException;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public final class BuildTargetsCollector {

  private final String buckRoot;
  private final Set<String> requiredRemoteFiles;

  public BuildTargetsCollector(String buckRoot) {
    this(buckRoot, new HashSet<>());
  }

  BuildTargetsCollector(String buckRoot, Set<String> requiredRemoteFiles) {
    this.buckRoot = buckRoot;
    this.requiredRemoteFiles = requiredRemoteFiles;
  }

  /**
   * Exposed for testing. Do not call it directly.
   *
   * @return a list of parsed json targets from target json.
   */
  ImmutableList<BuildTarget> parseBuildTargetList(
      ImmutableMap<String, String> cellMappings, JsonObject targetJsonMap) {
    // The first pass collects python_library that refers to a remote python_file
    for (Map.Entry<String, JsonElement> entry : targetJsonMap.entrySet()) {
      JsonObject targetJsonObject = entry.getValue().getAsJsonObject();
      addRemotePythonFiles(targetJsonObject, targetJsonMap);
    }
    // The second pass parses all normal targets and remote_file target with version information
    // collected above.
    ImmutableList.Builder<BuildTarget> buildTargetListBuilder = ImmutableList.builder();
    for (Map.Entry<String, JsonElement> entry : targetJsonMap.entrySet()) {
      String buildTargetName = entry.getKey();
      String cellPath = BuckCells.getCellPath(buildTargetName, cellMappings);
      JsonObject targetJsonObject = entry.getValue().getAsJsonObject();
      BuildTarget parsedTarget = parseBuildTarget(targetJsonObject, cellPath, buildTargetName);
      if (parsedTarget != null) {
        buildTargetListBuilder.add(parsedTarget);
      }
    }
    return buildTargetListBuilder.build();
  }

  /**
   * Exposed for testing. Do not call it directly.
   *
   * @return the parsed build target, or null if it is a non-python related target.
   */
  @Nullable
  BuildTarget parseBuildTarget(
      JsonObject targetJsonObject, @Nullable String cellPath, String buildTargetName) {
    String type = targetJsonObject.get("buck.type").getAsString();
    switch (type) {
      case "python_binary":
      case "python_library":
      case "python_test":
        return PythonTarget.parse(cellPath, targetJsonObject);
      case "genrule":
        // Thrift library targets have genrule rule type.
        BuildTarget parsedTarget =
            ThriftLibraryTarget.parse(cellPath, this.buckRoot, targetJsonObject);
        if (parsedTarget != null) {
          return parsedTarget;
        }
        return Antlr4LibraryTarget.parse(cellPath, this.buckRoot, targetJsonObject);
      case "cxx_genrule":
        // Swig library targets have cxx_genrule rule type.
        return SwigLibraryTarget.parse(cellPath, this.buckRoot, targetJsonObject);
      case "remote_file":
        return this.requiredRemoteFiles.contains(buildTargetName)
            ? RemoteFileTarget.parse(targetJsonObject)
            : null;
      default:
        return null;
    }
  }

  /**
   * @return an array that contains all of the buck targets (including the dependencies), given a
   *     list of targets we need to type check
   */
  public ImmutableList<BuildTarget> collectBuckTargets(ImmutableList<String> targets)
      throws BuilderException {
    return parseBuildTargetList(BuckCells.getCellMappings(), BuckQuery.getBuildTargetJson(targets));
  }

  private @Nullable String getSupportedPlatformDependency(JsonElement platformDependenciesField) {
    for (JsonElement platformDependencyTuple : platformDependenciesField.getAsJsonArray()) {
      JsonArray pair = platformDependencyTuple.getAsJsonArray();
      if (!pair.get(0).getAsString().equals("py3-platform007$")) {
        continue;
      }
      return pair.get(1).getAsJsonArray().get(0).getAsString();
    }
    return null;
  }

  private void addRemotePythonFiles(JsonObject targetJsonObject, JsonObject targetJsonMap) {
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
    this.requiredRemoteFiles.add("//" + basePath + wheelSuffix + "-remote");
  }
}
