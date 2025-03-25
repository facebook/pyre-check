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

const shouldShowNewLandingPage = process.env.INTERNAL_STATIC_DOCS;
export default component NewLandingPage() {
  const context = useDocusaurusContext();
  const {siteConfig} = context;

  // TODO: update content and improve css
  return (
    <Layout
      title="Pyrefly: A Static Type Checker for Python"
      description={siteConfig.description}>
      <header {...stylex.props(landingPageStyles.featureHero)}>
        <div className="container">
          <h1 {...stylex.props(landingPageStyles.title)}>
            pyrefly<span>.</span>
          </h1>
          <p {...stylex.props(landingPageStyles.subtitle)}>
            <span>
              {' '}
              <a
                href="https://github.com/facebook/pyrefly/milestone/1"
                {...stylex.props(landingPageStyles.yellowLink)}>
                Coming soon
              </a>
              : A faster Python type checker written in Rust
            </span>
          </p>
          <section>
            <Firefly />
            <Firefly />
            <Firefly />
            <Firefly />
          </section>
        </div>
      </header>
      <section {...stylex.props(styles.section)}>
        <div className="container">
          <h2 {...stylex.props(styles.sectionTitle)}>Why Pyrefly</h2>
          <WhyPyrefly />
        </div>
      </section>
      <section {...stylex.props(styles.section)}>
        <div className="container">
          <h2 {...stylex.props(styles.sectionTitle)}>Performance Comparison</h2>
          <PerformanceComparisonChart />
        </div>
      </section>
      <section {...stylex.props(styles.section, styles.lastSection)}>
        <div className="container">
          <h2 {...stylex.props(styles.sectionTitle)}>
            What People Say About Pyrefly
          </h2>
          <QuotesGrid />
        </div>
      </section>
    </Layout>
  );
}

const styles = stylex.create({
  section: {
    flex: 1,
    marginTop: 20,
  },
  lastSection: {
    marginBottom: 30,
  },
  sectionTitle: {
    fontSize: '3rem',
  },
});
