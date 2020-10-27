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
import {Card, Skeleton} from 'antd';
import {Issue} from './Issue.js';

type Props = $ReadOnly<{|
  data: any,
  fetchMore: any,
  loading: boolean,
|}>;

type State = $ReadOnly<{|
  recently_started_loading: boolean,
|}>;

class IssuesList extends React.Component<Props, State> {
  state: State = {
    recently_started_loading: false,
  };

  constructor(props: Props) {
    super(props);
    // $FlowFixMe
    this._fetchIssues = this._fetchIssues.bind(this);
    // $FlowFixMe
    this._handleScroll = this._handleScroll.bind(this);
  }

  componentDidMount(): void {
    window.addEventListener('scroll', this._handleScroll);
  }

  componentWillUnount(): void {
    window.removeEventListener('scroll', this._handleScroll);
  }

  render() {
    return (
      <>
        {this._renderIssues()}
        {this._renderLoadingSkeleton()}
      </>
    );
  }

  _fetchIssues() {
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
      this._fetchIssues();
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
            <Issue issue={node} />
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

export default IssuesList;
