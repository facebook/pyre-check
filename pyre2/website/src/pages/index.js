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
          <h1 className={styles.title}>
            pyrefly<span>.</span>
          </h1>
          <p className={styles.subtitle}>A static type checker for Python.</p>
          <section className={styles.buttonContainer}>
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
          <section>
            <li className={styles.firefly}></li>
            <li className={styles.firefly}></li>
            <li className={styles.firefly}></li>
            <li className={styles.firefly}></li>
          </section>
        </div>
      </header>
      <main>
        <section className={styles.info}>
          <div className={styles.quote}>
            <h3>Ship Faster</h3>
            <p>
              Built from the ground up to support gradual typing and deliver
              responsive incremental checks. Performant on large codebases with
              millions of lines of Python.
            </p>
          </div>
          <div className={styles.photo}>
            <img
              className={clsx(styles.imgFluid, 'rounded')}
              src={useBaseUrl('img/featurette-faster.gif')}
            />
          </div>
        </section>
        <section className={styles.info}>
          <div className={styles.photo}>
            <img
              className={clsx(styles.imgFluid, 'rounded')}
              src={useBaseUrl('img/featurette-smarter.gif')}
            />
          </div>
          <div className={styles.quote}>
            <h3>Integrated</h3>
            <p>
              Designed to help improve code quality and development speed by
              flagging type errors interactively in your terminal or live in
              your favorite editor.
            </p>
          </div>
        </section>
        <section className={styles.info}>
          <div className={styles.quote}>
            <h3>Fully Featured</h3>
            <p>
              Follows the typing standards introduced in PEPs 484, 526, 612, and
              is being actively developed and constantly improved.
            </p>
          </div>
          <div className={styles.photo}></div>
        </section>
      </main>
    </Layout>
  );
}
