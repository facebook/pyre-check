package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.FileSystem;
import com.google.common.collect.ImmutableSet;
import com.google.gson.JsonObject;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

public final class RemoteFileTarget implements BuildTarget {

  private static final Logger LOGGER = Logger.getGlobal();

  private final String url;

  RemoteFileTarget(String url) {
    this.url = url;
  }

  static RemoteFileTarget parse(
      JsonObject targetJsonObject, ImmutableSet<String> requiredRemoteFiles) {
    String basePath = targetJsonObject.get("buck.base_path").getAsString();
    String name = targetJsonObject.get("name").getAsString();
    if (!requiredRemoteFiles.contains(basePath + ":" + name)) {
      return null;
    }
    String url = targetJsonObject.get("url").getAsString();
    return new RemoteFileTarget(url);
  }

  @Override
  public void build(String buckRoot, String outputDirectory) {
    try {
      FileSystem.unzipRemoteFile(url, new File(outputDirectory));
    } catch (IOException exception) {
      LOGGER.warning("Cannot fetch and unzip remote python dependencies.");
    }
  }

  @Override
  public String toString() {
    return String.format("{url=%s}", url);
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (this == other) {
      return true;
    }
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    RemoteFileTarget remoteFileTarget = (RemoteFileTarget) other;
    return url.equals(remoteFileTarget.url);
  }

  @Override
  public int hashCode() {
    return url.hashCode();
  }
}
