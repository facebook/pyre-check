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
import ProgressBar from './ProgressBar';
import {
    Project,
    TypeChecker,
    type ProjectValue,
    type TypeCheckerValue,
} from './PerformanceComparisonTypes';

import PerformanceComparisonChartTimer from './PerformanceComparisonChartTimer';

interface TypeCheckerData {
    typechecker: TypeCheckerValue;
    durationInSeconds: number;
}

interface ProjectData {
    project: ProjectValue;
    data: TypeCheckerData[];
}

interface PerformanceComparisonChartProps {
    project: ProjectValue;
}

export default function PerformanceComparisonChart({
    project,
}: PerformanceComparisonChartProps): React.ReactElement {
    const data = getData(project);

    // Calculate the maximum duration for scaling
    const maxDuration = Math.max(...data.map(item => item.durationInSeconds));

    return (
        <div key={project}>
            {data.map((typechecker, index) => (
                <div
                    {...stylex.props(
                        styles.barContainer,
                        index !== data.length - 1 ? { marginBottom: 20 } : null,
                    )}
                    key={index}>
                    <span {...stylex.props(styles.typecheckerName)}>
                        <strong>{typechecker.typechecker}</strong>
                    </span>
                    <div {...stylex.props(styles.progressBarContainer)}>
                        <ProgressBar
                            durationInSeconds={typechecker.durationInSeconds}
                            maxDurationInSeconds={maxDuration}
                            highlight={typechecker.typechecker === TypeChecker.PYREFLY}
                        />
                    </div>
                    <span {...stylex.props(styles.duration)}>
                        <PerformanceComparisonChartTimer targetSeconds={typechecker.durationInSeconds} />
                    </span>
                </div>
            ))}
        </div>
    );
}

const styles = stylex.create({
    barContainer: {
        flex: 1,
        display: 'flex',
        flexDirection: 'row',
    },
    typecheckerName: {
        display: 'inline-block',
        fontSize: 20,
        width: 150,
    },
    progressBarContainer: {
        flexGrow: 1,
        marginRight: 20,
        paddingTop: '8px'
    },
    duration: {
        marginLeft: 'auto',
    },
});

function getData(project: ProjectValue): TypeCheckerData[] {
    const filteredData = performanceComparsionChartData
        .filter(data => data.project === project)
        .map(data => data.data);

    if (filteredData.length === 0) {
        throw new Error(`No data found for project ${project}`);
    }

    return filteredData[0];
}

const performanceComparsionChartData: ProjectData[] = [
    {
        project: Project.PYTORCH,
        data: [
            { typechecker: TypeChecker.PYREFLY, durationInSeconds: 0.5 },
            { typechecker: TypeChecker.MYPY, durationInSeconds: 8 },
            { typechecker: TypeChecker.PYRIGHT, durationInSeconds: 5 },
            { typechecker: TypeChecker.PYTYPE, durationInSeconds: 12 },
            { typechecker: TypeChecker.PYRE1, durationInSeconds: 15 },
        ],
    },
    {
        project: Project.INSTAGRAM,
        data: [
            { typechecker: TypeChecker.PYREFLY, durationInSeconds: 2 },
            { typechecker: TypeChecker.MYPY, durationInSeconds: 50 },
            { typechecker: TypeChecker.PYRIGHT, durationInSeconds: 20 },
            { typechecker: TypeChecker.PYTYPE, durationInSeconds: 40 },
            { typechecker: TypeChecker.PYRE1, durationInSeconds: 40 },
        ],
    },
    {
        project: Project.EXAMPLE,
        data: [
            { typechecker: TypeChecker.PYREFLY, durationInSeconds: 2 },
            { typechecker: TypeChecker.MYPY, durationInSeconds: 10 },
            { typechecker: TypeChecker.PYRIGHT, durationInSeconds: 5 },
            { typechecker: TypeChecker.PYTYPE, durationInSeconds: 12 },
            { typechecker: TypeChecker.PYRE1, durationInSeconds: 12 },
        ],
    },
];
