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
import WhyPyreflyGridItem from './whyPyreflyGridItem';

export default component WhyPyrefly() {
  return (
    <div {...stylex.props(styles.whyPyreflyGrid)}>
      {/* Short-term benefits */}
      <WhyPyreflyGridItem
        title="Scale with Confidence"
        content="Lightning fast, Pyrefly can process 1.8 million lines of code in a second!"
      />
      <WhyPyreflyGridItem
        title="Developer Delight"
        content=" Write better code, faster, and with confidence using your favorite
          editor supported by the Pyrefly IDE."
      />
      <WhyPyreflyGridItem
        title="Easy Onboarding"
        content=" Seamless transition from existing typechecker configurations to
          Pyrefly to get started!"
      />

      {/* Long-term benefits */}
      <WhyPyreflyGridItem
        title="Codebase clarity"
        content="Improve code readability and maintainability with precise type
          annotations."
      />
      <WhyPyreflyGridItem
        title="Tooling for Improving Types"
        content="Use the tools packaged with Pyrefly to elevate your typing coverage
          from 0 to 100."
      />
      <WhyPyreflyGridItem
        title="Fully featured"
        content="Follows the typing standards introduced in PEPs 484, 526, 612, along
          with being a typechecker that is being actively developed on and
          constantly improving."
      />
    </div>
  );
}

const styles = stylex.create({
  whyPyreflyGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(3, 1fr)',
    gap: '2rem',
  },
});
