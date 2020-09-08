/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import {Link} from 'react-router-dom';

class Issues extends React.Component {
  constructor(props) {
    super(props);
    this.fetchIssues = this.fetchIssues.bind(this);
  }

  fetchIssues() {
    const endCursor = this.props.data.issues.pageInfo.endCursor;
    this.props.fetchMore({
      variables: {after: endCursor},
      updateQuery: (prevResult, {fetchMoreResult}) => {
        fetchMoreResult.issues.edges = [
          ...prevResult.issues.edges,
          ...fetchMoreResult.issues.edges,
        ];
        return fetchMoreResult;
      },
    });
  }

  render() {
    return (
      <div>
        <h2>Issues</h2>
        <ul>
          {this.props.data.issues.edges.map(({node}) => (
            <Issue
              issue_id={node.issue_id}
              filename={node.filename}
              location={node.location}
              code={node.code}
              callable={node.callable}
              message={node.message}
              min_trace_length_to_sources={node.min_trace_length_to_sources}
              min_trace_length_to_sinks={node.min_trace_length_to_sinks}
            />
          ))}
        </ul>
        <button onClick={this.fetchIssues}>More</button>
      </div>
    );
  }
}

function Issue(props) {
  return (
    <div className="issue_instance">
      <h3>Issue {props.issue_id}</h3>
      <p>Code: {props.code}</p>
      <p>Message: {props.message}</p>
      <p>Callable: {props.callable}</p>
      <p>
        Location: {props.filename}:{props.location}
      </p>
      <div id="trace_lengths">
        <strong>Min Trace Lengths</strong>
        <p>Sources: {props.min_trace_length_to_sources}</p>
        <p>Sinks: {props.min_trace_length_to_sinks}</p>
      </div>
      <br />
      <Link to={`/trace/${props.issue_id}`}>
        <button>See Trace >></button>
      </Link>
    </div>
  );
}

export default Issues;
