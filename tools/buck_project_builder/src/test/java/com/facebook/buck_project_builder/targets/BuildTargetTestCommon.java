/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder.targets;

import com.google.gson.JsonParser;

final class BuildTargetTestCommon {
  static final JsonParser JSON_PARSER = new JsonParser();
  static final String BUCK_ROOT = ".";
  static final String OUTPUT_DIRECTORY = ".";
}
