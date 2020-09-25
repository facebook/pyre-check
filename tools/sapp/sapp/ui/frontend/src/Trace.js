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
import {Breadcrumb, Card, Modal, Skeleton, Typography} from 'antd';
import {useQuery, gql} from '@apollo/client';
import {Controlled as CodeMirror} from 'react-codemirror2';

import './Trace.css';
require('codemirror/lib/codemirror.css');
require('codemirror/mode/python/python.js');

const {Text} = Typography;

function Source(
  props: $ReadOnly<{|path: string, location: string|}>,
): React$Node {
  const SourceQuery = gql`
    query Issue($path: String) {
      file(path: $path) {
        edges {
          node {
            contents
          }
        }
      }
    }
  `;
  const {loading, error, data} = useQuery(SourceQuery, {
    variables: {path: props.path},
  });

  var value = '';
  if (error) {
    value = `Unable to load ${props.path}`;
  } else if (loading) {
    value = `Loading ${props.path}`;
  } else {
    value = data.file.edges[0].node.contents;
  }

  return (
    <div style={{border: '1px solid lightgray'}}>
      <CodeMirror
        value={value}
        options={{lineNumbers: true, readOnly: 'nocursor'}}
      />
    </div>
  );
}

function SourceTraces(props: $ReadOnly<{|issue_id: number|}>): React$Node {
  return (
    <>
      <Card title="Source Traces">
        <Skeleton active />
      </Card>
      <br />
    </>
  );
}

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

function SinkTraces(props: $ReadOnly<{|issue_id: number|}>): React$Node {
  return (
    <>
      <Card title="Sink Traces">
        <Skeleton active />
      </Card>
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
      <SourceTraces issue_id={issue_id} />
      <TraceRoot data={data} loading={loading} />
      <SinkTraces issue_id={issue_id} />
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
