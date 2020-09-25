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
import {withRouter} from 'react-router';
import {Breadcrumb, Card} from 'antd';

require('codemirror/lib/codemirror.css');
require('codemirror/mode/python/python.js');

function SourceTraces(props: $ReadOnly<{|issue_id: number|}>): React$Node {
  return (
    <>
      <Card title="Source Traces"></Card>
      <br />
    </>
  );
}

function TraceRoot(props: $ReadOnly<{|issue_id: number|}>): React$Node {
  return (
    <>
      <Card></Card>
      <br />
    </>
  );
}

function SinkTraces(props: $ReadOnly<{|issue_id: number|}>): React$Node {
  return (
    <>
      <Card title="Sink Traces"></Card>
      <br />
    </>
  );
}

function Trace(props: $ReadOnly<{|match: any|}>): React$Node {
  const issue_id = props.match.params.issue_id;
  return (
    <>
      <Breadcrumb style={{margin: '16px 0'}}>
        <Breadcrumb.Item href="/">Issues</Breadcrumb.Item>
        <Breadcrumb.Item>Trace for Issue {issue_id}</Breadcrumb.Item>
      </Breadcrumb>
      <SourceTraces issue_id={issue_id} />
      <TraceRoot issue_id={issue_id} />
      <SinkTraces issue_id={issue_id} />
    </>
  );
}

export default withRouter(Trace);
