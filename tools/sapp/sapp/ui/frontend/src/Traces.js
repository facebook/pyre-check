/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {IssueDescription} from './Issue.js';
import React, {useState} from 'react';
import {withRouter} from 'react-router';
import {
  Alert,
  Breadcrumb,
  Card,
  Divider,
  Modal,
  Skeleton,
  Select,
  Tag,
  Tooltip,
  Typography,
} from 'antd';
import {
  ExpandAltOutlined,
  LoginOutlined,
  LogoutOutlined,
  VerticalAlignMiddleOutlined,
  VerticalAlignBottomOutlined,
  VerticalAlignTopOutlined,
} from '@ant-design/icons';
import {useQuery, gql} from '@apollo/client';
import Source from './Source.js';
import {Documentation} from './Documentation.js';
import {Issue, IssueSkeleton} from './Issue.js';
import {HumanReadable, HumanReadablePort} from './HumanReadable';

require('./Traces.css');

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
        <Text>
          <VerticalAlignMiddleOutlined style={{marginRight: '.5em'}} />
          Source and sink traces meet at{' '}
          <HumanReadable input={issue.callable} code />.
        </Text>
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
  callee_port: string,
  callee_id: number,
  filename: string,
  callee_location: string,
  trace_length: number,
  is_leaf: boolean,
  titos: string,
}>;

function SelectFrame(
  props: $ReadOnly<{|
    issue_instance_id: number,
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

  const isPostcondition = props.kind === 'postcondition';
  const isLeaf =
    selectedFrameIndex !== null && props.frames[selectedFrameIndex].is_leaf;

  var select = null;
  if (!isLeaf) {
    const icon = (frame): React$Node => {
      const leaf = isPostcondition ? 'source' : 'sink';

      var icon = <ExpandAltOutlined />;
      if (frame.trace_length === 1) {
        icon = isPostcondition ? <LogoutOutlined /> : <LoginOutlined />;
      }

      return (
        <Tooltip title={`Distance to ${leaf}`}>
          <Text type="secondary">
            {icon} {frame.trace_length}{' '}
          </Text>
        </Tooltip>
      );
    };

    select = (
      <div class="Trace-select">
        <Select
          defaultValue={selectedFrameIndex}
          style={{width: '100%'}}
          onChange={setSelectedFrameIndex}
          suffixIcon={null}
          disabled={props.frames.length < 2}>
          {props.frames.map((frame, index) => {
            return (
              <Option value={index}>
                {icon(frame)}
                <HumanReadable input={frame.callee} code threshold={30} />{' '}
                <HumanReadablePort port={frame.callee_port || ''} />
              </Option>
            );
          })}
        </Select>
      </div>
    );
  }

  var next = null;
  if (selectedFrameIndex !== null && !isLeaf) {
    next = (
      <LoadFrame
        issue_instance_id={props.issue_instance_id}
        frame={props.frames[selectedFrameIndex]}
        kind={props.kind}
      />
    );
  }

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
    issue_instance_id: number,
    frame: Frame,
    kind: Kind,
  |}>,
): React$Node {
  const NextTraceFramesQuery = gql`
    query NextTraceFrames(
      $issue_instance_id: Int!
      $frame_id: Int!
      $kind: String!
    ) {
      next_trace_frames(
        issue_instance_id: $issue_instance_id
        frame_id: $frame_id
        kind: $kind
      ) {
        edges {
          node {
            frame_id
            callee
            callee_port
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
      issue_instance_id: props.issue_instance_id,
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
    <SelectFrame
      issue_instance_id={props.issue_instance_id}
      frames={frames}
      kind={props.kind}
    />
  );
}

function Expansion(
  props: $ReadOnly<{|
    issue_instance_id: number,
    issue: ?IssueDescription,
    kind: Kind,
  |}>,
): React$Node {
  const InitialTraceFramesQuery = gql`
    query InitialTraceFrame($issue_instance_id: Int!, $kind: String!) {
      initial_trace_frames(issue_instance_id: $issue_instance_id, kind: $kind) {
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
    variables: {issue_instance_id: props.issue_instance_id, kind: props.kind},
  });
  const frames = (data?.initial_trace_frames?.edges || []).map(
    edge => edge.node,
  );

  const isPostcondition = props.kind === 'postcondition';

  var content = null;
  if (loading) {
    content = <Skeleton active />;
  } else if (error) {
    content = <Alert type="error">{error.toString()}</Alert>;
  } else {
    content = (
      <SelectFrame
        issue_instance_id={props.issue_instance_id}
        frames={frames}
        kind={props.kind}
      />
    );
  }

  var description = null;
  const issue = props.issue;
  if (issue != null) {
    if (isPostcondition) {
      description = (
        <>
          <Text>
            <VerticalAlignTopOutlined style={{marginRight: '.5em'}} />
            Data of {issue.sources.length === 1 ? 'type ' : 'types '}
            {issue.sources.map(leave => (
              <Tag color="green">{leave}</Tag>
            ))}
            flowing <i>up to</i> <HumanReadable input={issue.callable} code />.
          </Text>
          <Divider plain>Traces</Divider>
        </>
      );
    } else {
      description = (
        <>
          <Text>
            <VerticalAlignBottomOutlined style={{marginRight: '.5em'}} />
            Data of {issue.sources.length === 1 ? 'type ' : 'types '}
            {issue.sources.map(leave => (
              <Tag color="green">{leave}</Tag>
            ))}
            flowing <i>from</i> <HumanReadable input={issue.callable} code />{' '}
            into sinks of {issue.sinks.length === 1 ? 'type ' : 'types '}
            {issue.sinks.map(leave => (
              <Tag color="red">{leave}</Tag>
            ))}
            .
          </Text>
          <Divider plain>Traces</Divider>
        </>
      );
    }
  }
  return (
    <>
      <Card>
        {description}
        {content}
      </Card>
      <br />
    </>
  );
}

function Trace(props: $ReadOnly<{|match: any|}>): React$Node {
  const run_id = props.match.params.run_id;
  const issue_instance_id = props.match.params.issue_instance_id;

  const IssueQuery = gql`
    query Issue($run_id: Int!, $issue_instance_id: Int!) {
      issues(run_id: $run_id, issue_instance_id: $issue_instance_id) {
        edges {
          node {
            issue_id
            issue_instance_id
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
  const {loading, error, data} = useQuery(IssueQuery, {
    variables: {run_id, issue_instance_id},
  });
  const issue = data?.issues?.edges[0]?.node || null;

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
          <Issue run_id={run_id} issue={issue} hideTitle={true} />
        )}
        <br />
        <Expansion
          issue_instance_id={issue_instance_id}
          issue={issue}
          kind="postcondition"
        />
        <TraceRoot data={data} loading={loading} />
        <Expansion
          issue_instance_id={issue_instance_id}
          issue={issue}
          kind="precondition"
        />
      </>
    );
  }

  return (
    <>
      <Breadcrumb style={{margin: '16px 0'}}>
        <Breadcrumb.Item href="/runs">Runs</Breadcrumb.Item>
        <Breadcrumb.Item href={`/run/${run_id}`}>Run {run_id}</Breadcrumb.Item>
        <Breadcrumb.Item>Issue {issue?.issue_id || ''}</Breadcrumb.Item>
      </Breadcrumb>
      {content}
    </>
  );
}

export default withRouter(Trace);
