import React from 'react';

class IssueInstance extends React.Component {
  render() {
      return <div className="issue_instance">
        <h3> Issue {this.props.issueId}</h3>
        <p>Location: {this.props.location}</p>
        <p>File: {this.props.filename}</p>
        <p>Filename Id: {this.props.filenameId}</p>
        <p>Code: {this.props.code}</p>
        <p>Callable Id: {this.props.callableId}</p>
        <p>Callable Count: {this.props.callableCount}</p>
        <p>isNewIssue: {this.props.isNewIssue}</p>
        <p>Run Id: {this.props.runId}</p>
        <p>Message Id: {this.props.messageId}</p>
        <p>Rank: {this.props.rank}</p>
        <div id="trace_lengths">
          <strong>Min Trace Lengths</strong>
          <p>Sources: {this.props.minTraceLengthToSources}</p>
          <p>Sinks: {this.props.minTraceLengthToSinks}</p>
          <p>Entrypoints: {this.props.minTraceLengthToEntrypoints}</p>
        </div>
        <br/>
      </div>
  }
}

export default IssueInstance
