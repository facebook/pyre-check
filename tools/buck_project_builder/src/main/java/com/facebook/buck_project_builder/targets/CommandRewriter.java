package com.facebook.buck_project_builder.targets;

import com.facebook.buck_project_builder.cache.BuilderCache;
import com.google.common.collect.ImmutableList;
import org.apache.commons.codec.digest.DigestUtils;

import java.nio.file.Paths;
import java.util.List;

public class CommandRewriter {
  public String rewriteThriftLibraryBuildCommand(
      String command, String baseModulePath, List<String> sources) {
    return command
        .replace("$(exe //thrift/compiler:thrift)", "thrift")
        .replace(
            "$(location //thrift/compiler/generate/templates:templates)",
            "thrift/compiler/generate/templates")
        .replaceFirst("-I \\$\\(location .*\\)", "-I .")
        .replace(
            "-o \"$OUT\"",
            String.format(
                "-out \"%s\"",
                Paths.get(BuilderCache.THRIFT_CACHE_PATH, DigestUtils.md5Hex(baseModulePath))))
        .replace("\"$SRCS\"", String.join(" ", sources))
        .replaceFirst(" &&.*", "");
  }

  public String rewriteSwigLibraryBuildCommand(
      String command, String outputDirectory, ImmutableList<String> sources) {
    return command
        .replaceFirst(
            "mkdir .+\\$\\(exe //third-party-buck/platform007/tools/swig:bin/swig\\)",
            "./third-party-buck/platform007/tools/swig/bin/swig")
        .replaceFirst(
            " -I- -I.+$",
            String.format(
                " -I- -I. -outdir %s -o %s -oh %s %s",
                outputDirectory,
                outputDirectory + "/temp.cc",
                outputDirectory + "/temp.h",
                String.join(" ", sources)))
        .replaceAll("'", "");
  }

  public String rewriteAntlr4LibraryBuildCommand(
      String command, String basePath, String outputDirectory, ImmutableList<String> sources) {
    return command
        .replaceFirst("mkdir .+\\$\\(exe //tools/antlr4:antlr4_wrapper\\)", "")
        .replace(
            "--install_dir=\"$OUT\"",
            String.format("--install_dir=\"%s\"", Paths.get(outputDirectory, basePath)))
        .replace("--antlr4_command=$(location //tools/antlr4:antlr4)", "")
        .replaceFirst("--grammars .+$", "--grammars " + String.join(" ", sources));
  }
}
