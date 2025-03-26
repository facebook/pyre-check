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
import PerformanceComparisonButton from './PerformanceComparisonButton';

// TODO: convert this to enum when we migrate to typescript
export type ProjectValue = 'Instagram' | 'PyTorch' | 'Example';
const Project = Object.freeze({
  INSTAGRAM: 'Instagram',
  PYTORCH: 'PyTorch',
  EXAMPLE: 'Example',
});

export default component PerformanceComparisonChart() {
  const [selectedProject, setSelectedProject] = useState<ProjectValue>(
    Project.INSTAGRAM,
  );

  return (
    <div>
      <div {...stylex.props(styles.buttonRow)}>
        <PerformanceComparisonButton
          project={Project.INSTAGRAM}
          selectedProject={selectedProject}
          setSelectedProject={setSelectedProject}
        />
        <PerformanceComparisonButton
          project={Project.PYTORCH}
          selectedProject={selectedProject}
          setSelectedProject={setSelectedProject}
        />
        <PerformanceComparisonButton
          project={Project.EXAMPLE}
          selectedProject={selectedProject}
          setSelectedProject={setSelectedProject}
        />
      </div>
      <div {...stylex.props(styles.chartContainer)}>
        {/* Chart placeholder */}
        <div {...stylex.props(styles.chartPlaceholder)}>
          Performance chart for {selectedProject} will be displayed here
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
