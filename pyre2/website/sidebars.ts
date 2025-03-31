/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

/*
 * We should explicitly list all the labels below, so that src/js/docs-categories.js that generates
 * the /en/docs page will automatically work.
 *
 * For categories, the first item must be the index page.
 */

import { fbInternalOnly } from 'docusaurus-plugin-internaldocs-fb/internal';
import type { SidebarsConfig } from '@docusaurus/plugin-content-docs';

const docsSidebar = [
    // TODO (T217317240): Go through internal only docs and release the ones that should be public to public
    ...fbInternalOnly([
        {
            type: 'category' as const,
            label: 'Internal Docs',
            items: ['fb/getting-started', 'fb/install', 'fb/error-kinds'],
        },
    ]),
];

const sidebars: SidebarsConfig = {
    docsSidebar,
};

export { docsSidebar };
export default sidebars;
