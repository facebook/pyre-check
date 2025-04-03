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
import { useState } from 'react';
import PerformanceComparisonButton from './PerformanceComparisonButton';
import PerformanceComparisonChart from './PerformanceComparisonChart';
import { Project, type ProjectValue } from './PerformanceComparisonTypes';

export default function PerformanceComparisonChartSection(): React.ReactElement {
    const [selectedProject, setSelectedProject] = useState<ProjectValue>(
        Project.PYTORCH,
    );

    return (
        <div {...stylex.props(styles.body)}>
            <div {...stylex.props(styles.buttonRow)}>
                <PerformanceComparisonButton
                    project={Project.PYTORCH}
                    selectedProject={selectedProject}
                    setSelectedProject={setSelectedProject}
                />
                <PerformanceComparisonButton
                    project={Project.INSTAGRAM}
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
                <PerformanceComparisonChart project={selectedProject} />
            </div>
        </div>
    );
}

const styles = stylex.create({
    body: {
        border: '1px solid',
        borderRadius: '4px',
        backgroundColor: 'var(--color-text)',
        marginVertical: '2rem',
        paddingVertical: '2rem',
    },
    buttonRow: {
        display: 'flex',
        justifyContent: 'center',
        gap: '1rem',
        marginBottom: '2rem',
    },
    chartContainer: { paddingHorizontal: '1rem' },
});
