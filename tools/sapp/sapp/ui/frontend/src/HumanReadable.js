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
