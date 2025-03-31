/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useState, useEffect} from 'react';
import * as stylex from '@stylexjs/stylex';

export default component ProgressBar(durationInSeconds: number) {
  return (
    <div
      {...stylex.props(styles.progressBar)}
      style={{
        height: '100%',
        width: `${durationInSeconds}%`,
      }}>
      <div
        {...stylex.props(styles.fill)}
        style={{animationDuration: `${durationInSeconds}s`}}
      />
    </div>
  );
}

const fillUpKeyframes = stylex.keyframes({
  '0%': {
    width: '0%',
  },
  '100%': {
    width: '100%',
  },
});

const styles = stylex.create({
  progressBar: {
    backgroundColor: 'var(--flow-yellow)',
    borderRadius: 30,
    width: '100%',
    overflow: 'hidden',
  },
  fill: {
    height: '100%',
    backgroundColor: 'var(--flow-gray-darker)',
    borderRadius: 'inherit',
    animationName: fillUpKeyframes,
    animationFillMode: 'forwards',
    animationTimingFunction: 'linear',
  },
});
