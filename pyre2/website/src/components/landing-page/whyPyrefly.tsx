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

export default function WhyPyrefly(): React.ReactElement {
  return (
    <div {...stylex.props(styles.whyPyreflyGrid)}>
      {/* Short-term benefits */}
      <WhyPyreflyGridItem
        title="Scale with Confidence"
        content=" Typecheck 1.8 million lines of code per second"
      />
      <WhyPyreflyGridItem
        title="Developer Delight"
        content=" Catch errors as you write code, and get instant feedback right in your favorite editor"
      />
      <WhyPyreflyGridItem
        title="Easy Onboarding"
        content=" Start type checking your code in minutes"
      />

      {/* Long-term benefits */}
      <WhyPyreflyGridItem
        title="Codebase clarity"
        content="Improve code readability and maintainability with precise type
          annotations"
      />
      <WhyPyreflyGridItem
        title="Tooling for Improving Types"
        content="Comes packaged with tools to elevate your type coverage
          from 0 to 100"
      />
      <WhyPyreflyGridItem
        title="Fully featured"
        content=" Actively under development. Follows the typing standards introduced in PEPs 484, 526, 612."
      />
    </div>
  );
}

const styles = stylex.create({
  whyPyreflyGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(3, 1fr)',
    gap: '2rem',
    '@media (max-width: 800px)': {
      gridTemplateColumns: 'repeat(2, 1fr)',
    },
    '@media (max-width: 550px)': {
      gridTemplateColumns: 'repeat(1, 1fr)',
    },
  },
});
