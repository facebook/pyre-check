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
import {
  Alert,
  Breadcrumb,
  Card,
  Modal,
  Skeleton,
  Typography,
  Select,
} from 'antd';
import {useQuery, gql} from '@apollo/client';
import Source from './Source.js';

const {Text} = Typography;
const {Option} = Select;

function TraceRoot(
  props: $ReadOnly<{|data: any, loading: boolean|}>,
): React$Node {
  if (props.loading) {
    return (
      <>
        <Card>
          <Skeleton active />
        </Card>
        <br />
      </>
    );
  }

  const issue = props.data.issues.edges[0].node;

  return (
    <>
      <Card>
        <Text strong>{issue.code}</Text>: {issue.message}
        <br />
        <Source path={issue.filename} location={issue.location} />
      </Card>
      <br />
    </>
  );
}

const InitialTraceFramesQuery = gql`
  query InitialTraceFrame($issue_id: Int!, $kind: String!) {
    initial_trace_frames(issue_id: $issue_id, kind: $kind) {
      edges {
        node {
          callee
          callee_id
        }
      }
    }
  }
`;

type Kind = 'precondition' | 'postcondition';

function Expansion(props: $ReadOnly<{|issue_id: number, kind: Kind|}>): React$Node {
  const {loading, error, data} = useQuery(InitialTraceFramesQuery, {
    variables: {issue_id: props.issue_id, kind: props.kind},
  });

  var content = <div />;
  if (loading) {
    content = <Skeleton active />;
  } else if (error) {
    content = <Alert type="error">{error.toString()}</Alert>;
  } else {
    const frames = data.initial_trace_frames.edges;
    var defaultValue = null;
    if (frames.length > 0) {
      defaultValue = frames[0].node.callee_id;
    }
    content = (
      <>
        <Select
          defaultValue={defaultValue}
          style={{width: '100%'}}
          onChange={value => console.log(value)}>
          {frames.map(frame => {
            return (
              <Option value={frame.node.callee_id}>{frame.node.callee}</Option>
            );
          })}
        </Select>
      </>
    );
  }

  const title = props.kind === 'postcondition' ? 'Source Trace' : 'Sink Trace';

  return (
    <>
      <Card title={title}>{content}</Card>
      <br />
    </>
  );
}

const IssueQuery = gql`
  query Issue($issue_id: Int) {
    issues(issue_id: $issue_id) {
      edges {
        node {
          issue_id
          filename
          location
          code
          callable
          message
        }
      }
    }
  }
`;

function Trace(props: $ReadOnly<{|match: any|}>): React$Node {
  const issue_id = props.match.params.issue_id;
  const {loading, error, data} = useQuery(IssueQuery, {variables: {issue_id}});

  var content = (
    <>
      <Expansion issue_id={issue_id} kind="postcondition" />
      <TraceRoot data={data} loading={loading} />
      <Expansion issue_id={issue_id} kind="precondition" />
    </>
  );

  if (error) {
    content = (
      <Modal title="Error" visible={true} footer={null}>
        <p>{error.toString()}</p>
      </Modal>
    );
  }

  return (
    <>
      <Breadcrumb style={{margin: '16px 0'}}>
        <Breadcrumb.Item href="/">Issues</Breadcrumb.Item>
        <Breadcrumb.Item>Trace for Issue {issue_id}</Breadcrumb.Item>
      </Breadcrumb>
      {content}
    </>
  );
}

export default withRouter(Trace);
