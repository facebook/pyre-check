package com.facebook.buck_project_builder.targets;

import com.google.gson.JsonObject;

import javax.annotation.Nullable;

public final class RemoteFileTarget implements BuildTarget {

  private final String url;

  RemoteFileTarget(String url) {
    this.url = url;
  }

  static RemoteFileTarget parse(JsonObject targetJsonObject) {
    return new RemoteFileTarget(targetJsonObject.get("url").getAsString());
  }

  @Override
  public void addToBuilder(BuildTargetsBuilder builder) {
    builder.addPythonWheelUrl(url);
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
