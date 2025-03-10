/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import type {CollapsedFieldProps} from 'react-json-view';

import * as React from 'react';
import {useState, type MixedElement} from 'react';
import ReactJson from 'react-json-view';
import clsx from 'clsx';
import styles from './TryPyre2.module.css';

export type PyreflyErrorMessage = {
  startLineNumber: number,
  startColumn: number,
  endLineNumber: number,
  endColumn: number,
  message: string,
  severity: number,
};

component ErrorMessage(error: PyreflyErrorMessage) {
  // This logic is meant to be an exact match of how we output errors in the cli defined here:
  // - https://fburl.com/code/e9lqk0h2
  // - https://fburl.com/code/hwhe60zt
  // TODO (T217247871): expose full error message from Pyre2 binary and use it directly here instead of duplicating the logic
  const {startLineNumber, startColumn, endLineNumber, endColumn} = error;

  let rangeStr;
  if (startLineNumber === endLineNumber) {
    if (startColumn === endColumn) {
      rangeStr = `${startLineNumber}:${startColumn}`;
    } else {
      rangeStr = `${startLineNumber}:${startColumn}-${endColumn}`;
    }
  } else {
    rangeStr = `${startLineNumber}:${startColumn}-${endLineNumber}:${endColumn}`;
  }

  const message = `${rangeStr}: ${error.message}`;
  return <span className={styles.msgType}>{message}</span>;
}

export default component TryPyre2Results(
  loading: boolean,
  errors: ?$ReadOnlyArray<PyreflyErrorMessage>,
  internalError: string,
) {
  const [activeToolbarTab, setActiveToolbarTab] = useState('errors');

  return (
    <div>
      <div className={styles.resultsToolbar}>
        <ul className={styles.tabs}>
          <li
            className={clsx(
              styles.tab,
              activeToolbarTab === 'errors' && styles.selectedTab,
            )}>
            Errors
          </li>
        </ul>
        {/* TODO (T217536145): Add JSON tab to sandbox */}
      </div>
      <div className={styles.results}>
        {loading && (
          <div>
            <div className={styles.loader}>
              <div className={styles.bounce1}></div>
              <div className={styles.bounce2}></div>
              <div></div>
            </div>
          </div>
        )}
        {!loading && activeToolbarTab === 'errors' && (
          <pre className={clsx(styles.resultBody, styles.errors)}>
            <ul>
              {internalError ? (
                <li>Pyrefly encountered an internal error: {internalError}.</li>
              ) : errors === undefined || errors === null ? (
                <li>Pyrefly failed to fetch errors.</li>
              ) : errors?.length === 0 ? (
                <li>No errors!</li>
              ) : (
                errors.map((error, i) => (
                  <li key={i}>
                    <ErrorMessage key={i} error={error} />
                  </li>
                ))
              )}
            </ul>
          </pre>
        )}
        {/* TODO (T217536145): Add JSON tab to sandbox */}
      </div>
    </div>
  );
}
