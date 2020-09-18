/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

public final class BuckQuery {

  private BuckQuery() {}

  public static JsonObject getBuildTargetJson(ImmutableList<String> targets, @Nullable String mode)
      throws BuilderException {
    if (targets.isEmpty()) {
      throw new BuilderException("Targets should not be empty.");
    }
    SimpleLogger.info("Querying buck for target information (`buck query`)...");
    long start = System.currentTimeMillis();
    ImmutableList<String> buildCommand = getBuildCommand(targets, mode);
    try (InputStream commandLineOutput = CommandLine.getCommandLineOutput(buildCommand)) {
      JsonElement parsedJson = new JsonParser().parse(new InputStreamReader(commandLineOutput));
      double buckQueryTime = (System.currentTimeMillis() - start) / 1000.0;
      SimpleLogger.info(String.format("Got target information in %.2fs", buckQueryTime));
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

  private static ImmutableList<String> getBuildCommand(
      ImmutableList<String> targets, @Nullable String mode) {
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
    ImmutableList.Builder<String> builder =
        ImmutableList.<String>builder().add("buck").add("query");
    if (mode != null) {
      builder.add(mode);
    }
    return builder
        .add(
            "kind('python_binary|python_library|python_test|genrule|cxx_genrule|remote_file', deps(%s))")
        .addAll(targets.stream().map(BuckQuery::normalizeTarget).collect(Collectors.toList()))
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

  static String normalizeTarget(String target) {
    if (!target.contains("//")) {
      target = String.format("//%s", target);
    }
    if (!target.contains("'")) {
      return String.format("'%s'", target);
    }
    return target;
  }
}
