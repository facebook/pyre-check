/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {fbContent, fbInternalOnly} = require('internaldocs-fb-helpers');

module.exports = {
  pyre: [
    ...fbContent({
      internal: ['fb/getting-started'],
      external: ['getting-started'],
    }),
    ...fbContent({
      external: ['installation'],
    }),
    'configuration',
    'types-in-python',
    ...fbInternalOnly(['fb/increasing-type-coverage']),
    'errors',
    'querying-pyre',
  ],
  pysa: [
    'pysa-basics',
    ...fbContent({
      internal: [
        'fb/pysa-running-internal',
        'fb/pysa-on-fbcode-internal',
        'fb/pysa-running-opensource-internal',
        'fb/pysa-running-common-internal',
      ],
      external: ['pysa-running'],
    }),
    ...fbInternalOnly(['fb/pysa-shipping-rules-models-internal']),
    'pysa-features',
    'pysa-advanced',
    'pysa-implementation-details',
    'pysa-model-generators',
    'pysa-model-dsl',
    'pysa-tips',
    'pysa-false-negatives',
    'pysa-coverage',
    'static-analysis-post-processor',
    'pysa-additional-resources',
  ],
  ...fbInternalOnly(() => require('./fb/sidebars.js')),
};
