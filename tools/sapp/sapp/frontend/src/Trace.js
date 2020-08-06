import React from 'react';
import { withRouter } from "react-router";
import {Controlled as CodeMirror} from 'react-codemirror2';

import {
  useQuery,
  gql,
} from '@apollo/client';

require('codemirror/lib/codemirror.css');
require('codemirror/mode/python/python.js');

const TraceQuery = gql`
  query Trace($issue_id: ID) {
    trace (issue_id: $issue_id) {
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
            issue_id: props.match.params.issue_id
        }
    });

    if (loading) return <p>Loading...</p>;
    if (error) return <p>Error:(</p>;

    return (
        <>
            <h3>Trace: {props.match.params.issue_id}</h3>
            <ul>
                {data.trace.edges.map(({node}, index) => (
                    <>
                      <h4>{index + 1}</h4>
                      <p>Callable: {node.callee}</p>
                      <p>Port: {node.callee}</p>
                      <p>Location: {node.filename}:{node.callee_location}</p>
                      <br/>
                      <CodeMirror
                        value={node.file_content}
                        onBeforeChange={(editor, data, value) => {
                        }}
                        options={{
                          lineNumbers: true
                        }}
                        onChange={(editor, data, value) => {
                        }}
                      />
                    </>
            ))}
            </ul>
        </>
    );
}

export default withRouter(Trace);
