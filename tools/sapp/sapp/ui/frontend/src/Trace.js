/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import React, {useState} from 'react';
import {withRouter} from 'react-router';
import {
  Alert,
  Breadcrumb,
  Card,
  Modal,
  Skeleton,
  Select,
  Tooltip,
  Typography,
} from 'antd';
import {BranchesOutlined} from '@ant-design/icons';
import {useQuery, gql} from '@apollo/client';
import Source from './Source.js';
import {Documentation, DocumentationTooltip} from './Documentation.js';

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
      <Card
        size="small"
        title={<>Root: {issue.message}</>}
        extra={<DocumentationTooltip path="trace.root" />}>
        <Source path={issue.filename} location={issue.location} />
      </Card>
      <br />
    </>
  );
}

type Kind = 'precondition' | 'postcondition';

type Frame = $ReadOnly<{
  frame_id: number,
  callee: string,
  callee_id: number,
  filename: string,
  callee_location: string,
}>;

function SelectFrame(
  props: $ReadOnly<{|
    issue_id: number,
    frames: $ReadOnlyArray<Frame>,
    kind: Kind,
    displaySource: boolean,
  |}>,
): React$Node {
  const [selectedFrameId, setSelectedFrameId] = useState(null);

  if (props.frames.length === 0) {
    return (
      <div style={{width: '100%', textAlign: 'center', padding: '2em'}}>
        <Text type="secondary">End of trace.</Text>
      </div>
    );
  }

  const source = (
    <Source
      path={props.frames[0].filename}
      location={props.frames[0].callee_location}
    />
  );

  const defaultSelectedFrameId = props.frames[0].frame_id;
  const select = (
    <Tooltip title={Documentation.trace.frameSelection}>
      <Select
        defaultValue={defaultSelectedFrameId}
        style={{width: '100%'}}
        onChange={setSelectedFrameId}
        suffixIcon={<BranchesOutlined />}>
        {props.frames.map(frame => {
          return <Option value={frame.frame_id}>{frame.callee}</Option>;
        })}
      </Select>
    </Tooltip>
  );
  const step = (
    <Step
      issue_id={props.issue_id}
      frame_id={selectedFrameId || defaultSelectedFrameId}
      kind={props.kind}
    />
  );

  const isPostcondition = props.kind === 'postcondition';
  return (
    <>
      {isPostcondition ? (
        <>
          {step}
          {select}
        </>
      ) : null}
      {props.displaySource ? source : null}
      {!isPostcondition ? (
        <>
          {select}
          {step}
        </>
      ) : null}
    </>
  );
}

function Step(
  props: $ReadOnly<{|
    issue_id: number,
    frame_id: number,
    kind: Kind,
  |}>,
): React$Node {
  const NextTraceFramesQuery = gql`
    query NextTraceFrames($issue_id: Int!, $frame_id: Int!, $kind: String!) {
      next_trace_frames(issue_id: $issue_id, frame_id: $frame_id, kind: $kind) {
        edges {
          node {
            frame_id
            callee
            callee_id
            filename
            callee_location
          }
        }
      }
    }
  `;
  const {loading, error, data} = useQuery(NextTraceFramesQuery, {
    variables: {
      issue_id: props.issue_id,
      frame_id: props.frame_id,
      kind: props.kind,
    },
  });
  const frames = (data?.next_trace_frames?.edges || []).map(edge => edge.node);

  if (loading) {
    return <Skeleton active />;
  }

  if (error) {
    return <Alert type="error">{error.toString()}</Alert>;
  }

  return (
    <SelectFrame
      issue_id={props.issue_id}
      frames={frames}
      kind={props.kind}
      displaySource={true}
    />
  );
}

function Expansion(
  props: $ReadOnly<{|issue_id: number, kind: Kind|}>,
): React$Node {
  const InitialTraceFramesQuery = gql`
    query InitialTraceFrame($issue_id: Int!, $kind: String!) {
      initial_trace_frames(issue_id: $issue_id, kind: $kind) {
        edges {
          node {
            frame_id
            callee
            callee_id
            filename
            callee_location
          }
        }
      }
    }
  `;
  const {loading, error, data} = useQuery(InitialTraceFramesQuery, {
    variables: {issue_id: props.issue_id, kind: props.kind},
  });
  const frames = (data?.initial_trace_frames?.edges || []).map(
    edge => edge.node,
  );

  const isPostcondition = props.kind === 'postcondition';

  var content = <div />;
  if (loading) {
    content = <Skeleton active />;
  } else if (error) {
    content = <Alert type="error">{error.toString()}</Alert>;
  } else {
    content = (
      <SelectFrame
        issue_id={props.issue_id}
        frames={frames}
        kind={props.kind}
        displaySource={false}
      />
    );
  }

  return (
    <>
      <Card
        size="small"
        title={isPostcondition ? 'Trace from Source' : 'Trace to Sink'}
        extra={
          <DocumentationTooltip
            path={isPostcondition ? 'trace.fromSource' : 'trace.toSink'}
          />
        }>
        {content}
      </Card>
      <br />
    </>
  );
}

function Trace(props: $ReadOnly<{|match: any|}>): React$Node {
  const issue_id = props.match.params.issue_id;

  const IssueQuery = gql`
    query Issue($issue_id: Int!) {
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
