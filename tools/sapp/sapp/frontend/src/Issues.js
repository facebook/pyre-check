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
import {Link} from 'react-router-dom';
import {Card, Skeleton} from 'antd';

type Props = $ReadOnly<{|
  data: any,
  fetchMore: any,
  loading: boolean,
|}>;

type State = $ReadOnly<{|
  recently_started_loading: boolean,
|}>;

class Issues extends React.Component<Props, State> {
  state: State = {
    recently_started_loading: false,
  };

  constructor(props: Props) {
    super(props);
    // $FlowFixMe
    this.fetchIssues = this.fetchIssues.bind(this);
    // $FlowFixMe
    this._handleScroll = this._handleScroll.bind(this);
  }

  componentDidMount(): void {
    window.addEventListener('scroll', this._handleScroll);
  }

  componentWillUnount(): void {
    window.removeEventListener('scroll', this._handleScroll);
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
      <>
        {this._renderIssues()}
        {this._renderLoadingSkeleton()}
      </>
    );
  }

  _handleScroll(event: any): void {
    let body = document.body;
    if (body === null) {
      return;
    }
    if (
      !this.props.loading &&
      !this.state.recently_started_loading &&
      window.pageYOffset + body.clientHeight >= body.scrollHeight - 100
    ) {
      this.setState({recently_started_loading: true});
      const captured = this;
      setTimeout(function() {
        // Avoid clobbering the server with too many requests.
        captured.setState({recently_started_loading: false});
      }, 1000);
      this.fetchIssues();
    }
  }

  _renderIssues() {
    if (this.props.loading) {
      return null;
    }
    return (
      <>
        {this.props.data.issues.edges.map(({node}) => (
          <>
            <Card extra={<>Issue {node.issue_id}</>}>
              <p>Code: {node.code}</p>
              <p>Message: {node.message}</p>
              <p>Callable: {node.callable}</p>
              <p>
                Location: {node.filename}:{node.location}
              </p>
              <div id="trace_lengths">
                <strong>Min Trace Lengths</strong>
                <p>Sources: {node.min_trace_length_to_sources}</p>
                <p>Sinks: {node.min_trace_length_to_sinks}</p>
              </div>
              <br />
              <Link to={`/trace/${node.issue_id}`}>
                <button>See Trace >></button>
              </Link>
            </Card>
            <br />
          </>
        ))}
      </>
    );
  }

  _renderLoadingSkeleton() {
    if (!this.props.loading && !this.state.recently_started_loading) {
      return null;
    }
    return (
      <>
        <Card>
          <Skeleton active />
        </Card>
        <br />
        <Card>
          <Skeleton active />
        </Card>
        <br />
        <Card>
          <Skeleton active />
        </Card>
        <br />
        <Card>
          <Skeleton active />
        </Card>
        <br />
        <Card>
          <Skeleton active />
        </Card>
        <br />
        <Card>
          <Skeleton active />
        </Card>
        <br />
        <Card>
          <Skeleton active />
        </Card>
        <br />
        <Card>
          <Skeleton active />
        </Card>
        <br />
      </>
    );
  }
}

export default Issues;
