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

export const Documentation = {
  issues: {
    kind: 'Explanation for kind',
    distance: 'Explanation for distance',
    name: 'Explanation for name',
    feature: 'Explanation for feature',
    likelyNew: 'Explanation for new',
  },
  filter: {
    codes: 'Only show issues with codes',
    paths: 'Only show issues in files at a given path',
    callables: 'Only show issues that originate in a specific callable',
    features:
      'Only show issues with traces whose features match these conditions',
    traceLengths: 'Only show issues with traces of lenght',
  },
  trace: {
    fromSource: 'Trace from the issue root to the source where data originates',
    root: 'Where the trace from the source meets the trace to the sink',
    toSink: 'Trace from the issue root to the data sink',
    frameSelectionPrecondition:
      'Explore alternative traces leading to the same sink',
    frameSelectionPostcondition:
      'Explore alternative traces from the same source',
  },
  source: {
    toNextFrame: 'Flow to next frame',
    tito: 'Flow through function',
  },
};

export function DocumentationTooltip(
  props: $ReadOnly<{path: string}>,
): React$Node {
  var title = Documentation;
  for (const element of props.path.split('.')) {
    title = title[element];
  }
  return (
    <div style={{display: 'inline', marginLeft: 4}}>
      <Tooltip title={title}>
        <Text type="secondary">[?]</Text>
      </Tooltip>
    </div>
  );
}
