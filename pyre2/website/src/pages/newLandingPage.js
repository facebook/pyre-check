/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import {useState} from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import * as stylex from '@stylexjs/stylex';
import {landingPageStyles} from './index';
import Firefly from '../components/firefly';

// This import serves no runtime purposes, but we import it to force webpack to run babel on it,
// so we can test whether babel can handle newer syntax.
import '../js/parser-playground';

const shouldShowNewLandingPage = process.env.INTERNAL_STATIC_DOCS;
export default component NewLandingPage() {
  const context = useDocusaurusContext();
  const {siteConfig} = context;
  const [selectedSource, setSelectedSource] = useState('instagram');

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
              <h3>Faster Performance</h3>
              <p>Built in Rust for superior type checking speed</p>
            </div>
            <div {...stylex.props(styles.gridItem)}>
              <h3>Better Error Messages</h3>
              <p>Clear and actionable type error reporting</p>
            </div>
            <div {...stylex.props(styles.gridItem)}>
              <h3>Easy Migration</h3>
              <p>Seamless transition from existing type checkers</p>
            </div>

            {/* Long-term benefits */}
            <div {...stylex.props(styles.gridItem)}>
              <h3>Enhanced Type System</h3>
              <p>Advanced type checking capabilities</p>
            </div>
            <div {...stylex.props(styles.gridItem)}>
              <h3>Extensibility</h3>
              <p>Plugin system for custom type checking rules</p>
            </div>
            <div {...stylex.props(styles.gridItem)}>
              <h3>Community Driven</h3>
              <p>Open source with active development</p>
            </div>
          </div>
        </div>
      </section>
      <section {...stylex.props(styles.section)}>
        <div className="container">
          <h2 {...stylex.props(styles.sectionTitle)}>Performance Comparison</h2>
          <div {...stylex.props(styles.buttonRow)}>
            <button
              {...stylex.props(
                styles.button,
                selectedSource === 'instagram' && styles.buttonActive,
              )}
              onClick={() => setSelectedSource('instagram')}>
              Instagram
            </button>
            <button
              {...stylex.props(
                styles.button,
                selectedSource === 'pytorch' && styles.buttonActive,
              )}
              onClick={() => setSelectedSource('pytorch')}>
              PyTorch
            </button>
            <button
              {...stylex.props(
                styles.button,
                selectedSource === 'example' && styles.buttonActive,
              )}
              onClick={() => setSelectedSource('example')}>
              Example
            </button>
          </div>
          <div {...stylex.props(styles.chartContainer)}>
            {/* Chart placeholder */}
            <div {...stylex.props(styles.chartPlaceholder)}>
              Performance chart for {selectedSource} will be displayed here
            </div>
          </div>
        </div>
      </section>
      <section {...stylex.props(styles.section, styles.lastSection)}>
        <div className="container">
          <h2 {...stylex.props(styles.sectionTitle)}>
            What People Say About Pyrefly
          </h2>
          <div {...stylex.props(styles.quotesGrid)}>
            <div {...stylex.props(styles.quoteCard)}>
              <p {...stylex.props(styles.quoteText)}>
                "Pyrefly's performance improvements have significantly reduced
                our CI times. The migration was seamless and the results are
                impressive."
              </p>
              <div {...stylex.props(styles.quoteAuthor)}>
                <strong>Sarah Chen</strong>
                <span>Engineering Lead, Instagram</span>
              </div>
            </div>
            <div {...stylex.props(styles.quoteCard)}>
              <p {...stylex.props(styles.quoteText)}>
                "The enhanced type system and clear error messages have made our
                codebase more maintainable and easier to debug."
              </p>
              <div {...stylex.props(styles.quoteAuthor)}>
                <strong>Michael Rodriguez</strong>
                <span>Core Contributor, PyTorch</span>
              </div>
            </div>
            <div {...stylex.props(styles.quoteCard)}>
              <p {...stylex.props(styles.quoteText)}>
                "As an early adopter, I'm impressed by Pyrefly's extensibility
                and the active community support. It's the future of Python type
                checking."
              </p>
              <div {...stylex.props(styles.quoteAuthor)}>
                <strong>Emma Thompson</strong>
                <span>Tech Lead, Open Source Projects</span>
              </div>
            </div>
          </div>
        </div>
      </section>
    </Layout>
  );
}

const styles = stylex.create({
  buttonRow: {
    display: 'flex',
    justifyContent: 'center',
    gap: '1rem',
    marginBottom: '2rem',
  },
  button: {
    padding: '0.75rem 1.5rem',
    borderRadius: '4px',
    border: '1px solid var(--ifm-color-primary)',
    backgroundColor: 'transparent',
    color: 'var(--ifm-color-primary)',
    cursor: 'pointer',
    transition: 'all 0.2s',
  },
  buttonActive: {
    backgroundColor: 'var(--ifm-color-primary)',
    color: 'white',
  },
  chartContainer: {
    minHeight: '400px',
    border: '1px solid var(--ifm-color-emphasis-300)',
    borderRadius: '8px',
    padding: '2rem',
  },
  chartPlaceholder: {
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
    height: '100%',
    color: 'var(--ifm-color-emphasis-600)',
  },
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
  quotesGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(3, 1fr)',
    gap: '2rem',
  },
  quoteCard: {
    padding: '2rem',
    borderRadius: '8px',
    backgroundColor: 'var(--ifm-background-surface-color)',
    boxShadow: '0 2px 8px rgba(0, 0, 0, 0.1)',
  },
  quoteText: {
    fontSize: '1.1rem',
    lineHeight: '1.6',
    marginBottom: '1.5rem',
    fontStyle: 'italic',
  },
  quoteAuthor: {
    display: 'flex',
    flexDirection: 'column',
    gap: '0.25rem',
  },
});
