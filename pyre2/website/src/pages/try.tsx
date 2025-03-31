/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import BrowserOnly from '@docusaurus/BrowserOnly';
import Layout from '@theme/Layout';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

const TryPyre2 = React.lazy(() => import('../try-pyre2/TryPyre2'));

export default function TryPyre2Page(): JSX.Element {
    const { siteConfig = {} } = useDocusaurusContext();
    return (
        <Layout
            title="Try Pyrefly: the Pyrefly Playground"
            description={siteConfig.description}
            noFooter>
            <BrowserOnly>
                {() => (
                    <React.Suspense fallback={<div>Loading...</div>}>
                        <TryPyre2 sampleFilename="playground.py" />
                    </React.Suspense>
                )}
            </BrowserOnly>
        </Layout>
    );
}
