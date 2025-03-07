/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import BrowserOnly from '@docusaurus/BrowserOnly';
import * as React from 'react';

const TryPyre2 = React.lazy(() => import('./TryPyre2'));

export default component TryPyre2BrowserOnly(
    editorHeight: number | "auto",
    codeSample: string,
    showErrorPanel: boolean,
) {
    return <BrowserOnly>
    {() => (
    <React.Suspense fallback={<div>Loading...</div>}>
        <TryPyre2 editorHeight={editorHeight} codeSample={codeSample} showErrorPanel={showErrorPanel}/>
    </React.Suspense>
    )}
    </BrowserOnly>
}
