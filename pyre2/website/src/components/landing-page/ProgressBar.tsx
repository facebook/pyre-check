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
    highlight: boolean;
}

export default function ProgressBar({
    durationInSeconds,
    maxDurationInSeconds,
    highlight,
}: ProgressBarProps): React.ReactElement {
    // Calculate the relative percentage width based on the maximum duration
    const relativeWidth = (durationInSeconds / maxDurationInSeconds) * 100;

    return (
        <div
            {...stylex.props(styles.progressBar)}
            style={{
                height: '80%',
                width: `${relativeWidth}%`,
            }}>
            <div
                {...stylex.props(styles.fill, highlight && styles.highlight)}
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
        backgroundColor: 'var(--light-grey)',
        borderRadius: 30,
        width: '100%',
        overflow: 'hidden',
    },
    fill: {
        height: '99%',
        backgroundColor: 'var(--color-background)',
        borderRadius: 'inherit',
        animationName: fillUpKeyframes,
        animationFillMode: 'forwards',
        animationTimingFunction: 'linear',
    },
    highlight: {
        backgroundColor: 'var(--dark-yellow)',
    }
});
