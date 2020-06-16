package com.facebook.buck_project_builder;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.apache.commons.io.IOUtils;

import javax.annotation.Nullable;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Map;

public final class BuckCells {

  private BuckCells() {}

  static ImmutableMap<String, String> parseCellMappings(String cellMappingsJsonString) {
    Map<String, String> parsedMap =
        new Gson()
            .fromJson(cellMappingsJsonString, new TypeToken<Map<String, String>>() {}.getType());
    return ImmutableMap.copyOf(parsedMap);
  }

  public static ImmutableMap<String, String> getCellMappings() throws BuilderException {
    try {
      return parseCellMappings(
          IOUtils.toString(
              CommandLine.getCommandLineOutput(ImmutableList.of("buck", "audit", "cell", "--json")),
              Charset.defaultCharset()));
    } catch (IOException exception) {
      throw new BuilderException(
          "'buck audit cell' failed to run. There must be errors in your dev environment.");
    }
  }

  /** @return cell path of the cell name in build target, or null if there is no explicit target. */
  public static @Nullable String getCellPath(
      String buildTargetName, ImmutableMap<String, String> cellMappings) {
    if (buildTargetName.startsWith("//")) {
      return null;
    }
    String buckCell = buildTargetName.substring(0, buildTargetName.indexOf("//"));
    String cellPath = cellMappings.get(buckCell);
    if (cellPath == null) {
      throw new Error(
          String.format(
              "Buck cell %s is not specified in .buckconfig. The config file might be broken.",
              buckCell));
    }
    return cellPath;
  }
}
