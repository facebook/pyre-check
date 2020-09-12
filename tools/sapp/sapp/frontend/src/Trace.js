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
import {Breadcrumb, Card, Modal, Skeleton} from 'antd';
import {Controlled as CodeMirror} from 'react-codemirror2';

import {useQuery, gql} from '@apollo/client';

require('codemirror/lib/codemirror.css');
require('codemirror/mode/python/python.js');

const TraceQuery = gql`
  query Trace($issue_id: ID) {
    trace(issue_id: $issue_id) {
      edges {
        node {
          callee
          callee_port
          filename
          callee_location
          file_content
        }
      }
    }
  }
`;

function Trace(props) {
  const {loading, error, data} = useQuery(TraceQuery, {
    variables: {
      issue_id: props.match.params.issue_id,
    },
  });

  if (error) {
    return (
      <Modal title="Error" visible={true} footer={null}>
        <p>{error.toString()}</p>
      </Modal>
    );
  }

  var content = <></>;

  if (loading) {
    content = (
      <Card>
        <Skeleton active />
      </Card>
    );
  } else {
    content = (
      <>
        {data.trace.edges.map(({node}, index) => (
          <>
            <Card>
              <h4>{index + 1}</h4>
              <p>Callable: {node.callee}</p>
              <p>Port: {node.callee}</p>
              <p>
                Location: {node.filename}:{node.callee_location}
              </p>
              <br />
              <CodeMirror
                value={node.file_content}
                onBeforeChange={(editor, data, value) => {}}
                options={{
                  lineNumbers: true,
                }}
                onChange={(editor, data, value) => {}}
              />
            </Card>
            <br />
          </>
        ))}
      </>
    );
  }

  return (
    <>
      <Breadcrumb style={{margin: '16px 0'}}>
        <Breadcrumb.Item href="/">Issues</Breadcrumb.Item>
        <Breadcrumb.Item>
          Trace for Issue {props.match.params.issue_id}
        </Breadcrumb.Item>
      </Breadcrumb>
      {content}
    </>
  );
}

export default withRouter(Trace);
