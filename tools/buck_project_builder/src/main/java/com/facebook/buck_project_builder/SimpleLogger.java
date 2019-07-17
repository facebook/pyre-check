package com.facebook.buck_project_builder;

public final class SimpleLogger {

  private SimpleLogger() {}

  public static void info(String message) {
    System.err.println("INFO: " + message);
  }

  public static void warning(String message) {
    System.err.println("WARNING: " + message);
  }

  public static void error(String message) {
    System.err.println("ERROR: " + message);
  }
}
