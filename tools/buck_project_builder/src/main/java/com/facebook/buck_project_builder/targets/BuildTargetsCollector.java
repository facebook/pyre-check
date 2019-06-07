package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.BuilderException;
import com.facebook.buck_project_builder.CommandLine;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;

public final class BuildTargetsCollector {

  /**
   * @return an array that contains all of the buck targets (including the dependencies), given a
   *     list of targets we need to type check
   */
  public static ImmutableList<PythonTarget> collectBuckTargets(ImmutableList<String> targets)
      throws BuilderException {
    return parseBuildTargetList(getBuildTargetJson(targets));
  }

  /**
   * Exposed for testing. Do not call it directly.
   *
   * @return a list of parsed json targets from target json.
   */
  static ImmutableList<PythonTarget> parseBuildTargetList(JsonObject targetJsonMap) {
    ImmutableList.Builder<PythonTarget> buildTargetListBuilder = ImmutableList.builder();
    for (Map.Entry<String, JsonElement> entry : targetJsonMap.entrySet()) {
      JsonObject targetJsonObject = entry.getValue().getAsJsonObject();
      PythonTarget parsedTarget = parseBuildTarget(targetJsonObject);
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
  static @Nullable PythonTarget parseBuildTarget(JsonObject targetJsonObject) {
    String type = targetJsonObject.get("buck.type").getAsString();
    switch (type) {
      case "python_binary":
      case "python_library":
      case "python_unittest":
        return PythonTarget.parse(type, targetJsonObject);
      default:
        return null;
    }
  }

  private static JsonObject getBuildTargetJson(ImmutableList<String> targets)
      throws BuilderException {
    if (targets.isEmpty()) {
      throw new BuilderException("Targets should not be empty.");
    }
    try (InputStream commandLineOutput = getBuildTargetJsonStream(targets)) {
      JsonElement parsedJson = new JsonParser().parse(new InputStreamReader(commandLineOutput));
      if (!parsedJson.isJsonObject()) {
        throw new Error(
            "Unexpected buck query output. It should always be a json object. Bad json: "
                + parsedJson);
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

  private static InputStream getBuildTargetJsonStream(ImmutableList<String> targets)
      throws IOException {
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
    ImmutableList<String> command =
        ImmutableList.<String>builder()
            .add("buck")
            .add("query")
            .add("kind('python_binary|python_library|python_test', deps(%s))")
            .addAll(targets)
            .add("--output-attributes")
            .add("buck.type")
            .add("buck.base_path")
            .add("labels")
            .add("srcs")
            .build();
    return CommandLine.getCommandLineOutput(command);
  }
}
