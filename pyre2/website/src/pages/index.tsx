/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import Layout from '@theme/Layout';
import NewLandingPage from './newLandingPage';
import LandingPageHeader from '../components/landing-page/landingPageHeader';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

// This import serves no runtime purposes, but we import it to force webpack to run babel on it,
// so we can test whether babel can handle newer syntax.
import '../js/parser-playground';

export default function Home(): React.ReactElement {
    const { siteConfig } = useDocusaurusContext();

    // TODO (T218370640): replace current landing page with new landing page
    return process.env.INTERNAL_STATIC_DOCS === '1' ? (
        <NewLandingPage />
    ) : (
        <Layout
            title="Pyrefly: A Static Type Checker for Python"
            description={siteConfig.description}>
            <LandingPageHeader />
        </Layout>
    );
}
