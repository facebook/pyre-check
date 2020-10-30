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
import {
  BranchesOutlined,
  ColumnHeightOutlined,
  VerticalAlignMiddleOutlined,
  VerticalAlignBottomOutlined,
  VerticalAlignTopOutlined,
} from '@ant-design/icons';
import {useQuery, gql} from '@apollo/client';
import Source from './Source.js';
import {Documentation, DocumentationTooltip} from './Documentation.js';
import {Issue, IssueSkeleton} from './Issue.js';

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
        title={
          <>
            <VerticalAlignMiddleOutlined style={{marginRight: '.5em'}} />
            Trace Root
          </>
        }
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
  trace_length: number,
  is_leaf: boolean,
  titos: string,
}>;

function SelectFrame(
  props: $ReadOnly<{|
    issue_id: number,
    frames: $ReadOnlyArray<Frame>,
    kind: Kind,
  |}>,
): React$Node {
  const [selectedFrameIndex, setSelectedFrameIndex] = useState(
    props.frames.length === 0 ? null : 0,
  );

  if (props.frames.length === 0) {
    return (
      <div style={{width: '100%', textAlign: 'center', padding: '2em'}}>
        <Text type="warning">Missing Trace Frame</Text>
      </div>
    );
  }

  const source = (
    <Source
      path={props.frames[0].filename}
      location={props.frames[0].callee_location}
      titos={props.frames[0].titos}
    />
  );

  const isLeaf =
    selectedFrameIndex !== null && props.frames[selectedFrameIndex].is_leaf;

  var select = null;
  if (!isLeaf) {
    select = (
      <Select
        defaultValue={selectedFrameIndex}
        style={{width: '100%'}}
        onChange={setSelectedFrameIndex}
        suffixIcon={
          <Tooltip title={Documentation.trace.frameSelection}>
            <BranchesOutlined style={{fontSize: '0.9em'}} />
          </Tooltip>
        }
        disabled={props.frames.length < 2}>
        {props.frames.map((frame, index) => {
          return (
            <Option value={index}>
              <Tooltip title="Distance to sink">
                {frame.trace_length}
                <ColumnHeightOutlined style={{fontSize: '.9em'}} />
              </Tooltip>{' '}
              {frame.callee}
            </Option>
          );
        })}
      </Select>
    );
  }

  var next = null;
  if (selectedFrameIndex !== null && !isLeaf) {
    next = (
      <LoadFrame
        issue_id={props.issue_id}
        frame={props.frames[selectedFrameIndex]}
        kind={props.kind}
      />
    );
  }

  const isPostcondition = props.kind === 'postcondition';
  return (
    <>
      {isPostcondition ? (
        <>
          {next}
          {select}
        </>
      ) : null}
      {source}
      {!isPostcondition ? (
        <>
          {select}
          {next}
        </>
      ) : null}
    </>
  );
}

function LoadFrame(
  props: $ReadOnly<{|
    issue_id: number,
    frame: Frame,
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
            trace_length
            is_leaf
            titos
          }
        }
      }
    }
  `;
  const {loading, error, data} = useQuery(NextTraceFramesQuery, {
    variables: {
      issue_id: props.issue_id,
      frame_id: props.frame.frame_id,
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
    <SelectFrame issue_id={props.issue_id} frames={frames} kind={props.kind} />
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
            trace_length
            is_leaf
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
      />
    );
  }

  return (
    <>
      <Card
        size="small"
        title={
          isPostcondition ? (
            <>
              <VerticalAlignBottomOutlined style={{marginRight: '.5em'}} />
              Traces from Source
            </>
          ) : (
            <>
              <VerticalAlignTopOutlined style={{marginRight: '.5em'}} />
              Traces to Sink
            </>
          )
        }
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
            code
            message
            callable
            filename
            location
            sources
            source_names
            sinks
            sink_names
            features
            min_trace_length_to_sources
            min_trace_length_to_sinks
          }
        }
      }
    }
  `;
  const {loading, error, data} = useQuery(IssueQuery, {variables: {issue_id}});

  var content = null;
  if (error) {
    content = (
      <Modal title="Error" visible={true} footer={null}>
        <p>{error.toString()}</p>
      </Modal>
    );
  } else {
    content = (
      <>
        {loading ? (
          <IssueSkeleton />
        ) : (
          <Issue issue={data.issues.edges[0].node} hideTitle={true} />
        )}
        <br />
        <Expansion issue_id={issue_id} kind="postcondition" />
        <TraceRoot data={data} loading={loading} />
        <Expansion issue_id={issue_id} kind="precondition" />
      </>
    );
  }

  return (
    <>
      <Breadcrumb style={{margin: '16px 0'}}>
        <Breadcrumb.Item href="/">Issues</Breadcrumb.Item>
        <Breadcrumb.Item>Traces for Issue {issue_id}</Breadcrumb.Item>
      </Breadcrumb>
      {content}
    </>
  );
}

export default withRouter(Trace);
