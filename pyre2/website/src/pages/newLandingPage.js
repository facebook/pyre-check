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
import cssStyles from './styles.module.css';

// This import serves no runtime purposes, but we import it to force webpack to run babel on it,
// so we can test whether babel can handle newer syntax.
import '../js/parser-playground';

const shouldShowNewLandingPage = process.env.INTERNAL_STATIC_DOCS;
console.log('shouldShowNewLandingPage', shouldShowNewLandingPage);
export default component NewLandingPage() {
  const context = useDocusaurusContext();
  const {siteConfig} = context;

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
            <li className={cssStyles.firefly}></li>
            <li className={cssStyles.firefly}></li>
            <li className={cssStyles.firefly}></li>
            <li className={cssStyles.firefly}></li>
          </section>
        </div>
      </header>
    </Layout>
  );
}
