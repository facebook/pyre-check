/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.buck_project_builder;

public class BuilderException extends Exception {
  public BuilderException(String message) {
    super(message);
  }
}
