/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import BrowserOnly from '@docusaurus/BrowserOnly';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Navbar from '@theme/Navbar';
import * as stylex from '@stylexjs/stylex';

const TryPyre2 = React.lazy(() => import('../try-pyre2/TryPyre2'));

export default component TryPyre2Page() {
  const context = useDocusaurusContext();
  const {siteConfig = ({}: any)} = context;
  return (
    <Layout
      // $FlowFixMe[prop-missing]
      title="Try Pyrefly: the Pyrefly Playground"
      // $FlowFixMe[prop-missing]
      description={siteConfig.description}
      noFooter>
      <BrowserOnly>
        {() => (
          <React.Suspense fallback={<div>Loading...</div>}>
            {process.env.INTERNAL_STATIC_DOCS ? (
              <header {...stylex.props(styles.title)}>
                Sandbox isn't available Internally, please use the{' '}
                <a
                  href="https://pyrefly.org/try"
                  {...stylex.props(styles.hyperlink)}>
                  {' '}
                  public sandbox
                </a>
              </header>
            ) : (
              <TryPyre2 sampleFilename="playground.py" />
            )}
          </React.Suspense>
        )}
      </BrowserOnly>
    </Layout>
  );
}

const styles = stylex.create({
  title: {
    marginTop: '10px',
    marginLeft: '10px',
    fontSize: 32,
  },
  hyperlink: {
    textDecoration: 'underline',
    color: '#337ab7',
  },
});
