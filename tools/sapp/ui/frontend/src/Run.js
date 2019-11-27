/**
 * (c) Facebook, Inc. and its affiliates. Confidential and proprietary.
 */

import React from 'react';
import './index.css';
import { gql} from 'apollo-boost';
import { useQuery } from '@apollo/react-hooks';

function Run(props) {
  const runId = props.match.params.runId;

  const { loading, error, data } = useQuery(
    gql`
      query Issues($runId:Int) {
        allIssues(runId:$runId) {
          edges {
            node {
              issueId,
              run
            }
          }
        }
      }
    `,
    { variables: { runId: runId } },
  );

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error</p>;

  return (
    <div>
      Issues for run {runId}<br/>
      <ul>
        {data.allIssues.edges.map(run => (
          <li key={run.node.issueId}>
            <a href={"../issue/"+run.node.issueId}>{run.node.issueId}</a>
          </li>
        ))}
      </ul>
    </div>);
}

export default Run;
