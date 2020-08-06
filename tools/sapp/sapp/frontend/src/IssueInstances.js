import React from 'react';


class IssueInstances extends React.Component{
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

  render (){
    return <div>
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
          />))}
      </ul>
      <button onClick={this.fetchIssues}>
        More
      </button>
    </div>
  }
}

class Issue extends React.Component {
  render() {
      return <div className="issue_instance">
        <h3>Issue {this.props.issue_id}</h3>
        <p>Code: {this.props.code}</p>
        <p>Message: {this.props.message}</p>
        <p>Callable: {this.props.callable}</p>
        <p>Location: {this.props.filename}:{this.props.location}</p>
        <div id="trace_lengths">
          <strong>Min Trace Lengths</strong>
          <p>Sources: {this.props.min_trace_length_to_sources}</p>
          <p>Sinks: {this.props.min_trace_length_to_sinks}</p>
        </div>
        <br/>
      </div>
  }
}

export default IssueInstances
