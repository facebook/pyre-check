package com.facebook.buck_project_builder.targets;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;

import javax.annotation.Nullable;

public interface PlatformSelector {
  default @Nullable String getSupportedPlatformDependency(JsonElement platformDependenciesField) {
    return null;
  }

  default @Nullable JsonElement getSupportedVersionedSources(JsonArray versionedSourcesArray) {
    return null;
  }
}
