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
import type { ProjectValue } from './PerformanceComparisonTypes';
import typography from './typography';
interface PerformanceComparisonButtonProps {
    project: ProjectValue;
    selectedProject: ProjectValue;
    setSelectedProject: React.Dispatch<React.SetStateAction<ProjectValue>>;
}

export default function PerformanceComparisonButton({
    project,
    selectedProject,
    setSelectedProject,
}: PerformanceComparisonButtonProps): React.ReactElement {
    return (
        <button
            {...stylex.props(
                styles.button,
                typography.p,
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
        border: '1px solid var(--color-secondary)',
        backgroundColor: 'transparent',
        color: 'var(--color-secondary)',
        cursor: 'pointer',
        transition: 'all 0.2s',
    },
    buttonActive: {
        backgroundColor: 'var(--color-secondary)',
        color: 'white',
    },
});
