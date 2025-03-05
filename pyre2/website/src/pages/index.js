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
import styles from './styles.module.css';

// This import serves no runtime purposes, but we import it to force webpack to run babel on it,
// so we can test whether babel can handle newer syntax.
import '../js/parser-playground';

export default component Home() {
  const context = useDocusaurusContext();
  const {siteConfig} = context;
  return (
    <Layout
      title="Pyrefly: A Static Type Checker for Python"
      description={siteConfig.description}>
      <header className={clsx(styles.feature, styles.featureHero)}>
        <div className="container">
          <p className={styles.featureHeading}>
            <span>Pyrefly is</span> <br className={styles.hiddenLargerUp} /> a
            static type <br className={styles.hiddenLargerUp} /> checker for{' '}
            <br className={styles.hiddenLargerUp} /> <span>Python.</span>
          </p>
          <section>
            <Link
              className={styles.featureButton}
              to={useBaseUrl('en/docs/getting-started')}>
              Get Started
            </Link>
            <Link
              className={styles.featureButton}
              to={useBaseUrl('en/docs/install')}>
              Install Pyrefly
            </Link>
            {/* Add font awesome github logo */}
            <Link
              className={styles.featureButton}
              to="https://ghbtns.com/github-btn.html?user=facebook&repo=pyrefly&type=star&count=false&size=large">
              Github
            </Link>
          </section>
        </div>
      </header>
      <main></main>
    </Layout>
  );
}
