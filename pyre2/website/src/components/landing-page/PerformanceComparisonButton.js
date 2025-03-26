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
import type {ProjectValue} from './PerformanceComparisonChart';

export default component PerformanceComparisonButton(
  project: ProjectValue,
  selectedProject: ProjectValue,
  setSelectedProject: any,
) {
  return (
    <button
      {...stylex.props(
        styles.button,
        selectedProject === project && styles.buttonActive,
      )}
      onClick={() => setSelectedProject(project)}>
      {project}
    </button>
  );
}

const styles = stylex.create({
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
});
