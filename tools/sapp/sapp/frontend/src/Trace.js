import React from 'react';
import { withRouter } from "react-router";

import {
  useQuery,
  gql,
} from '@apollo/client';

const TraceQuery = gql`
  query Trace($issue_id: ID) {
    trace (issue_id: $issue_id) {
      edges {
        node {
          callee
          callee_port
          filename
          callee_location
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
                    </>
            ))}
            </ul>
        </>
    );
}

export default withRouter(Trace);
