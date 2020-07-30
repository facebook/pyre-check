import React from 'react';

class IssueInstance extends React.Component {
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

export default IssueInstance
