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
import type {ProjectValue} from './PerformanceComparisonChartSection';
import ProgressBar from './ProgressBar';

// TODO: convert this to enum when we migrate to typescript
export type TypeCheckerValue = 'Pyrefly' | 'Pyright' | 'MyPy' | 'Pytype';
const TypeChecker = Object.freeze({
  PYREFLY: 'Pyrefly',
  PYRIGHT: 'Pyright',
  MYPY: 'MyPy',
  PYTYPE: 'Pytype',
  PYRE1: 'Pyre1',
});

export default component PerformanceComparisonChart(project: ProjectValue) {
  const data = getData(project);
  return (
    <div key={project}>
      {data.map((typechecker, index) => (
        <div {...stylex.props(styles.barContainer)} key={index}>
          <span {...stylex.props(styles.typecheckerName)}>
            <strong>{typechecker.typechecker}</strong>
          </span>
          <div {...stylex.props(styles.progressBarContainer)}>
            <ProgressBar durationInSeconds={typechecker.durationInSeconds} />
          </div>
          <span {...stylex.props(styles.duration)}>
            <strong>{`${typechecker.durationInSeconds}s`}</strong>
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
    marginBottom: 20,
  },
  typecheckerName: {
    display: 'inline-block',
    fontSize: 20,
    width: 150,
  },
  progressBarContainer: {
    flexGrow: 1,
  },
  duration: {
    marginLeft: 'auto',
  },
});

function getData(project: ProjectValue) {
  const filteredData = performanceComparsionChartData
    .filter(data => data.project === project)
    .map(data => data.data);

  if (filteredData.length === 0) {
    throw new Error(`No data found for project ${project}`);
  }

  return filteredData[0];
}

const performanceComparsionChartData = [
  {
    project: 'Instagram',
    data: [
      {typechecker: TypeChecker.PYREFLY, durationInSeconds: 2},
      {typechecker: TypeChecker.MYPY, durationInSeconds: 50},
      {typechecker: TypeChecker.PYRIGHT, durationInSeconds: 20},
      {typechecker: TypeChecker.PYTYPE, durationInSeconds: 40},
      {typechecker: TypeChecker.PYRE1, durationInSeconds: 40},
    ],
  },
  {
    project: 'PyTorch',
    data: [
      {typechecker: TypeChecker.PYREFLY, durationInSeconds: 2},
      {typechecker: TypeChecker.MYPY, durationInSeconds: 10},
      {typechecker: TypeChecker.PYRIGHT, durationInSeconds: 5},
      {typechecker: TypeChecker.PYTYPE, durationInSeconds: 12},
      {typechecker: TypeChecker.PYRE1, durationInSeconds: 12},
    ],
  },
  {
    project: 'Example',
    data: [
      {typechecker: TypeChecker.PYREFLY, durationInSeconds: 2},
      {typechecker: TypeChecker.MYPY, durationInSeconds: 10},
      {typechecker: TypeChecker.PYRIGHT, durationInSeconds: 5},
      {typechecker: TypeChecker.PYTYPE, durationInSeconds: 12},
      {typechecker: TypeChecker.PYRE1, durationInSeconds: 12},
    ],
  },
];
