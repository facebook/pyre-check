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
          <div {...stylex.props(styles.whyPyreflyGrid)}>
            {/* Short-term benefits */}
            <div {...stylex.props(styles.gridItem)}>
              <h3>Scale with Confident</h3>
              <p>Lightning fast, even on millions of lines of code.</p>
            </div>
            <div {...stylex.props(styles.gridItem)}>
              <h3>Developer Delight</h3>
              <p>
                Write better code, faster, and with confidence using your
                favorite editor supported by the Pyrefly IDE.
              </p>
            </div>
            <div {...stylex.props(styles.gridItem)}>
              <h3>Easy Onboarding</h3>
              <p>
                Seamless transition from existing typechecker configurations to
                Pyrefly to get started!
              </p>
            </div>

            {/* Long-term benefits */}
            <div {...stylex.props(styles.gridItem)}>
              <h3>Codebase clarity</h3>
              <p>
                Improve code readability and maintainability with precise type
                annotations.
              </p>
            </div>
            <div {...stylex.props(styles.gridItem)}>
              <h3>Tooling for Improving Types</h3>
              <p>
                Use the tools packaged with Pyrefly to elevate your typing
                coverage from 0 to 100.
              </p>
            </div>
            <div {...stylex.props(styles.gridItem)}>
              <h3>Fully featured</h3>
              <p>
                Follows the typing standards introduced in PEPs 484, 526, 612,
                along with being a typechecker that is being actively developed
                on and constantly improving.
              </p>
            </div>
          </div>
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
  whyPyreflyGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(3, 1fr)',
    gap: '2rem',
  },
});
