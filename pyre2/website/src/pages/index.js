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
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import * as stylex from '@stylexjs/stylex';
import NewLandingPage from './newLandingPage';
import Firefly from '../components/landing-page/firefly';
import LandingPageHeader from '../components/landing-page/landingPageHeader';

// This import serves no runtime purposes, but we import it to force webpack to run babel on it,
// so we can test whether babel can handle newer syntax.
import '../js/parser-playground';

export default component Home() {
  const context = useDocusaurusContext();
  const {siteConfig} = context;

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
