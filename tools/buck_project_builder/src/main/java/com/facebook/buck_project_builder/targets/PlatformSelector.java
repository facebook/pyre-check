package com.facebook.buck_project_builder.targets;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;

import javax.annotation.Nullable;

public class PlatformSelector {
  public @Nullable String getSupportedPlatformDependency(JsonElement platformDependenciesField) {
    for (JsonElement platformDependencyTuple : platformDependenciesField.getAsJsonArray()) {
      JsonArray pair = platformDependencyTuple.getAsJsonArray();
      if (!pair.get(0).getAsString().equals("py3-platform007$")) {
        continue;
      }
      return pair.get(1).getAsJsonArray().get(0).getAsString();
    }
    return null;
  }
}
