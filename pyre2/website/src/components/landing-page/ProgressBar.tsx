/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import * as stylex from '@stylexjs/stylex';

interface ProgressBarProps {
    durationInSeconds: number;
    maxDurationInSeconds: number;
}

export default function ProgressBar({
    durationInSeconds,
    maxDurationInSeconds,
}: ProgressBarProps): React.ReactElement {
    // Calculate the relative percentage width based on the maximum duration
    const relativeWidth = (durationInSeconds / maxDurationInSeconds) * 100;

    return (
        <div
            {...stylex.props(styles.progressBar)}
            style={{
                height: '100%',
                width: `${relativeWidth}%`,
            }}>
            <div
                {...stylex.props(styles.fill)}
                style={{ animationDuration: `${durationInSeconds}s` }}
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
        backgroundColor: 'var(--color-background)',
        borderRadius: 30,
        width: '100%',
        overflow: 'hidden',
    },
    fill: {
        height: '100%',
        backgroundColor: 'var(--color-secondary)',
        borderRadius: 'inherit',
        animationName: fillUpKeyframes,
        animationFillMode: 'forwards',
        animationTimingFunction: 'linear',
    },
});
