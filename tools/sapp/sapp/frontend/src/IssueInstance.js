import React from 'react';

class IssueInstance extends React.Component {
  render() {
      return <div className="issue_instance">
        <h3> Issue {this.props.issue_id}</h3>
        <p>Location: {this.props.location}</p>
        <p>File: {this.props.filename}</p>
        <p>Filename Id: {this.props.filename_id}</p>
        <p>Code: {this.props.code}</p>
        <p>Callable Id: {this.props.callable_id}</p>
        <p>Callable Count: {this.props.callable_count}</p>
        <p>isNewIssue: {this.props.is_new_issue}</p>
        <p>Run Id: {this.props.run_id}</p>
        <p>Message Id: {this.props.message_id}</p>
        <p>Rank: {this.props.rank}</p>
        <div id="trace_lengths">
          <strong>Min Trace Lengths</strong>
          <p>Sources: {this.props.min_trace_length_to_sources}</p>
          <p>Sinks: {this.props.min_trace_length_to_sinks}</p>
          <p>Entrypoints: {this.props.min_trace_length_to_entrypoints}</p>
        </div>
        <br/>
      </div>
  }
}

export default IssueInstance
