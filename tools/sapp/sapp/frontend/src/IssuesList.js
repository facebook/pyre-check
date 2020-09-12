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
import {Card, Skeleton, Row, Col, Typography, Divider} from 'antd';

const {Text, Link} = Typography;

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

    const gutter = [8, 8];
    const leftSpan = 4;
    const rightSpan = 20;

    return (
      <>
        {this.props.data.issues.edges.map(({node}) => (
          <>
            <Card
              size="small"
              title={<>Issue {node.issue_id}</>}
              extra={<Link href={`/trace/${node.issue_id}`}>Trace</Link>}>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">Code</Text>
                </Col>
                <Col span={rightSpan}>{node.code}</Col>
              </Row>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">Message</Text>
                </Col>
                <Col span={rightSpan}>{node.message}</Col>
              </Row>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">Callable</Text>
                </Col>
                <Col span={rightSpan}>
                  <Text code>{node.callable}</Text>
                </Col>
              </Row>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">Location</Text>
                </Col>
                <Col span={rightSpan}>
                  {node.filename}:{node.location}
                </Col>
              </Row>
              <Divider orientation="left" plain>
                Minimum Trace Lengths
              </Divider>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">To Sources</Text>
                </Col>
                <Col span={1}>{node.min_trace_length_to_sources}</Col>
                <Col span={2}>
                  <Text type="secondary" style={{textAlign: 'right'}}>
                    To Sinks
                  </Text>
                </Col>
                <Col span={rightSpan - 3}>{node.min_trace_length_to_sinks}</Col>
              </Row>
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

export default IssuesList;
