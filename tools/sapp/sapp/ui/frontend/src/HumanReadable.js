/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import React from 'react';
import {Tooltip, Typography} from 'antd';
const {Text} = Typography;

export function HumanReadablePort(props: $ReadOnly<{port: string}>) {
  // TODO(T71492980): hard-coding heuristics for Pysa right now.
  var port = props.port;

  var access = '';
  var accesses = port.match(/(\[.*\])+/);
  if (accesses) {
    port = port.slice(0, port.length - accesses[1].length);
    accesses = accesses[1]
      .split(']')
      .filter(access => access !== '')
      .map(access => access.slice(1))
      .map(access => <Text code>{access}</Text>);
    access = <> accessing {accesses}</>;
  }

  if (port === 'result') {
    port = 'returns';
  }
  const formal_match = port.match(/formal\((.*)\)/);
  if (formal_match) {
    port = (
      <>
        into parameter <Text code>{formal_match[1]}</Text>
      </>
    );
  }

  return (
    <Text type="secondary">
      {port}
      {access}
    </Text>
  );
}

export function HumanReadable(
  props: $ReadOnly<{
    input: string,
    separator?: string,
    threshold?: number,
    code?: boolean,
  }>,
): React$Node {
  const separator = props.separator || '.';
  const threshold = props.threshold || 50;

  var readable = props.input;
  if (readable.length > threshold) {
    // Attempt to construct `module...Class.method`.
    const split = readable.split(separator);
    if (split.length > 3) {
      readable = `${split[0]}${separator}[...]${separator}${
        split[split.length - 2]
      }.${split[split.length - 1]}`;
    }
  }
  return (
    <Tooltip title={props.input}>
      <Text code={props.code || false} style={{wordBreak: 'break-all'}}>
        {readable}
      </Text>
    </Tooltip>
  );
}
