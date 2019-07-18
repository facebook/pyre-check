package com.facebook.buck_project_builder;

import com.google.common.collect.ImmutableList;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

final class BuildTimeLogger {

  private BuildTimeLogger() {}

  static void logBuildTime(long start, long end, ImmutableList<String> targets) {
    JsonObject intObject = new JsonObject();
    intObject.add("start_time", new JsonPrimitive(start / 1000));
    intObject.add("time", new JsonPrimitive(end / 1000));
    intObject.add("elapsed_time", new JsonPrimitive(end - start));

    JsonObject normalObject = new JsonObject();
    normalObject.add("name", new JsonPrimitive("FastBuckBuilderBuildTime"));
    normalObject.add("target", new JsonPrimitive(targets.toString()));
    String user = System.getenv("USER");
    normalObject.add("username", new JsonPrimitive(user == null ? "" : user));
    String host = System.getenv("HOSTNAME");
    normalObject.add("host", new JsonPrimitive(host == null ? "" : host));

    JsonObject statistics = new JsonObject();
    statistics.add("int", intObject);
    statistics.add("normal", normalObject);

    try (OutputStream messageWriterStream =
        Runtime.getRuntime()
            .exec(new String[] {"scribe_cat", "perfpipe_pyre_performance"})
            .getOutputStream()) {
      OutputStreamWriter writer = new OutputStreamWriter(messageWriterStream);
      new Gson().toJson(statistics, writer);
      writer.flush();
    } catch (IOException exception) {
      SimpleLogger.warning("Failed to log build time: " + exception.getMessage());
    }
  }
}
