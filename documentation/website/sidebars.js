/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {fbContent, fbInternalOnly} = require('internaldocs-fb-helpers');

module.exports = {
  pyre: [
    'getting-started',
    ...fbContent({
      external: ['installation'],
    }),
    ...fbContent({
      external: ['configuration'],
      internal: ['fb/configuration'],
    }),
    'types-in-python',
    ...fbInternalOnly(['fb/increasing-type-coverage']),
    'errors',
    ...fbInternalOnly(['fb/open-source-setup']),
    'querying-pyre',
    'features',
  ],
  pysa: [
    ...fbContent({
      external: ['pysa-quickstart'],
    }),
    {
      'How Pysa Works': [
        'pysa-basics',
        'pysa-features',
        'pysa-advanced',
        'pysa-implementation-details',
      ],
    },

    ...fbContent({
      internal: [
        {
          'Running Pysa': [
            'fb/pysa-running-internal',
            'fb/pysa-on-fbcode-internal',
            'fb/pysa-running-opensource-internal',
            'fb/pysa-running-common-internal',
            'fb/pysa-running-sandcastle',
          ],
        },
      ],
      external: ['pysa-running'],
    }),
    ...fbContent({
      internal: ['fb/pysa-shipping-rules-models-internal'],
      external: ['pysa-shipping-rules-models'],
    }),
    {
      'Scaling Beyond Individual Models': [
        'pysa-model-generators',
        'pysa-model-dsl',
      ],
    },
    {
      'Development Tips': [
        'pysa-false-negatives',
        'pysa-coverage',
        'pysa-tips',
      ],
    },
    'static-analysis-post-processor',
    ...fbContent({
      internal: [
        {
          'Warning Codes': [
            'warning_codes/overview-public',
            'fb/warning_codes/code-5001',
            'fb/warning_codes/code-5003',
            'fb/warning_codes/code-5004',
            'fb/warning_codes/code-5005',
            'fb/warning_codes/code-5007',
            'fb/warning_codes/code-5008',
            'fb/warning_codes/code-5010',
            'fb/warning_codes/code-5011',
            'fb/warning_codes/code-5012',
            'fb/warning_codes/code-5013',
            'fb/warning_codes/code-5014',
            'fb/warning_codes/code-5015',
            'fb/warning_codes/code-5016',
            'fb/warning_codes/code-5017',
            'fb/warning_codes/code-5018',
            'fb/warning_codes/code-5023',
            'fb/warning_codes/code-5024',
            'fb/warning_codes/code-5027',
            'fb/warning_codes/code-5028',
            'fb/warning_codes/code-5029',
            'fb/warning_codes/code-5031',
            'fb/warning_codes/code-5032',
            'fb/warning_codes/code-5034',
            'fb/warning_codes/code-5035',
            'fb/warning_codes/code-5036',
            'fb/warning_codes/code-5037',
            'fb/warning_codes/code-5100',
            'fb/warning_codes/code-5101',
            'fb/warning_codes/code-5102',
            'fb/warning_codes/code-5103',
            'fb/warning_codes/code-5105',
            'fb/warning_codes/code-5106',
            'fb/warning_codes/code-5107',
            'fb/warning_codes/code-5116',
            'fb/warning_codes/code-5117',
            'fb/warning_codes/code-5120',
            'fb/warning_codes/code-5132',
            'fb/warning_codes/code-6029',
            'fb/warning_codes/code-6064',
            'fb/warning_codes/code-6065',
            'fb/warning_codes/code-6066',
            'fb/warning_codes/code-6073',
            'fb/warning_codes/code-6074',
            'fb/warning_codes/code-6107',
            'fb/warning_codes/code-6306',
            'fb/warning_codes/code-6308',
            'fb/warning_codes/code-6310',
            'fb/warning_codes/code-6312',
            'fb/warning_codes/code-6432',
            'fb/warning_codes/code-6445',
            'fb/warning_codes/code-6446',
            'fb/warning_codes/code-6449',
            'fb/warning_codes/code-6458',
            'fb/warning_codes/code-6459',
            'fb/warning_codes/code-6460',
            'fb/warning_codes/code-6461',
            'fb/warning_codes/code-7330',
          ],
        },
      ],
      external: [
        {
          'Warning Codes': [
            'warning_codes/overview-public',
            'warning_codes/code-5001-public',
            'warning_codes/code-6065-public',
          ],
        },
      ],
    }),
    'pysa-additional-resources',
    ...fbInternalOnly(['fb/pysa-cross-repo-taint-exchange']),
    'pysa-explore',
    ...fbInternalOnly(['fb/pysa-ig-integration-test']),
    ...fbInternalOnly([
      {
        'Pysa Developer Docs': [
          'fb/pysa-deployment',
          'fb/pysa-sandcastle-internals',
          'fb/pysa-oncall-runbook',
          'fb/review-tma-system-coverage',
          'fb/pysa-tasks-backlog'
        ],
      },
    ]),
  ],
  ...fbInternalOnly(() => require('./fb/sidebars.js')),
};
