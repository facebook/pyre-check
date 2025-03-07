/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 */

/*
 * We should explicitly list all the labels below, so that src/js/docs-categories.js that generates
 * the /en/docs page will automatically work.
 *
 * For categories, the first item must be the index page.
 */

// $FlowIgnore[cannot-resolve-module] docusaurus type defaults to any
const {fbInternalOnly} = require('docusaurus-plugin-internaldocs-fb/internal');

module.exports = {
  // $FlowIgnore[signature-verification-failure] docusaurus type defaults to any
  docsSidebar: [
    // TODO (T217317240): Go through internal only docs and release the ones that should be public to public
    ...fbInternalOnly([
      {
        type: 'category',
        label: 'Introduction',
        items: ['getting-started', 'install'],
      },
      {
        type: 'category',
        label: 'Internal Docs',
        items: ['fb/error-kinds'],
      },
    ]),
  ],
};
