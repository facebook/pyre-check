/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import * as stylex from '@stylexjs/stylex';
import {landingPageStyles} from './index';
import Firefly from '../components/firefly';
import QuoteCard from '../components/quoteCard';

// This import serves no runtime purposes, but we import it to force webpack to run babel on it,
// so we can test whether babel can handle newer syntax.
import '../js/parser-playground';
import PerformanceComparisonChart from '../components/PerformanceComparisonChart';
import QuotesGrid from '../components/quotesGrid';
import WhyPyrefly from '../components/whyPyrefly';
import LandingPageSection from '../components/landingPageSection';
import LandingPageHeader from '../components/landingPageHeader';

const shouldShowNewLandingPage = process.env.INTERNAL_STATIC_DOCS;
export default component NewLandingPage() {
  const context = useDocusaurusContext();
  const {siteConfig} = context;

  // TODO: update content and improve css
  return (
    <Layout
      title="Pyrefly: A Static Type Checker for Python"
      description={siteConfig.description}>
      <LandingPageHeader />
      <LandingPageSection title="Why Pyrefly" child={<WhyPyrefly />} />
      <LandingPageSection
        title="Performance Comparison"
        child={<PerformanceComparisonChart />}
      />
      <LandingPageSection
        title="What People Say About Pyrefly"
        child={<QuotesGrid />}
        isLastSection={true}
      />
    </Layout>
  );
}
