/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

function FBInternalSidebarSections(path) {
   if (process.env.FB_INTERNAL) {
     return require(path);
   } else {
     return {};
   }
}

function FBInternal(elements) {
  return process.env.FB_INTERNAL ? elements : [];
}

function FBInternalWithOssFallback(elements, fallback) {
   return process.env.FB_INTERNAL ? elements : fallback;
}

module.exports = {
  documentation: {
    "Pyre": [
      ...FBInternal(["fb/getting-started"]),
      "getting-started",
      "installation",
      "configuration",
      "types-in-python",
      ...FBInternal(["fb/increasing-type-coverage"]),
      "errors",
      "querying-pyre"
    ],
    "Pysa": [
      ...FBInternal(["fb/pysa-basics-internal"]),
      "pysa-basics",
      "pysa-running",
      "pysa-features",
      "pysa-advanced",
      ...FBInternal(["fb/pysa-model-generators-internal"]),
      "pysa-model-generators",
      "pysa-model-dsl",
      "pysa-tips",
      "pysa-coverage",
      "static-analysis-post-processor",
      ...FBInternal(["fb/pysa-additional-resources-internal"]),
      "pysa-additional-resources"
    ]
  },
  ...FBInternalSidebarSections('./fb/sidebars.js')
}
