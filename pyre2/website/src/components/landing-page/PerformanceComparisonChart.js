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
import {useState} from 'react';

export default component PerformanceComparisonChart() {
  const [selectedSource, setSelectedSource] = useState('instagram');

  return (
    <div>
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
});
