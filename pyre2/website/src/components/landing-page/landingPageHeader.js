/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import * as stylex from '@stylexjs/stylex';
import {landingPageStyles} from '../../pages';
import Firefly from './firefly';

export default component LandingPageHeader() {
  return (
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
  );
}
