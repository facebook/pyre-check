/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import React, {useState} from 'react';
import {
  Card,
  Col,
  Popover,
  Row,
  Skeleton,
  Tag,
  Tooltip,
  Typography,
} from 'antd';
import {
  CodeTwoTone,
  MinusCircleOutlined,
  PlusCircleOutlined,
} from '@ant-design/icons';
import Source from './Source.js';
import {Documentation} from './Documentation.js';

const {Text, Link} = Typography;

function ShowMore(
  props: $ReadOnly<{|
    items: $ReadOnlyArray<string>,
    maximumElementsToShow: number,
  |}>,
): React$Node {
  const [showMore, setShowMore] = useState(false);
  const items = props.items;

  if (items.length <= props.maximumElementsToShow) {
    return (
      <>
        {items.map(feature => (
          <Tag>{feature}</Tag>
        ))}
      </>
    );
  } else {
    const truncatedItems = items.slice(
      0,
      showMore ? items.length : props.maximumElementsToShow,
    );
    const moreToShow = items.length - truncatedItems.length;
    return (
      <>
        {truncatedItems.map(item => (
          <Tag>{item}</Tag>
        ))}
        <Tag
          onClick={() => setShowMore(!showMore)}
          icon={!showMore ? <PlusCircleOutlined /> : <MinusCircleOutlined />}>
          {!showMore ? `Show ${moreToShow} more...` : 'Show fewer...'}
        </Tag>
      </>
    );
  }
}

function DelayedTooltip(
  props: $ReadOnly<{|
    content: string,
    children: React$Node,
    placement?: string,
  |}>,
): React$Node {
  return (
    <Tooltip
      mouseEnterDelay={1.0}
      title={props.content}
      placement={props.placement}>
      {props.children}
    </Tooltip>
  );
}

type LeafKind = 'sources' | 'sinks';

function Leaves(
  props: $ReadOnly<{|
    kind: LeafKind,
    kinds: $ReadOnlyArray<string>,
    names: $ReadOnlyArray<string>,
    distance: number,
  |}>,
): React$Node {
  return (
    <>
      <DelayedTooltip content={Documentation.issues.kind}>
        {props.kinds.map(kind => (
          <Tag color={props.kind === 'sources' ? 'volcano' : 'green'}>
            {kind}
          </Tag>
        ))}
      </DelayedTooltip>{' '}
      at{' '}
      <DelayedTooltip content={Documentation.issues.distance}>
        <Text underline>minimum distance {props.distance}.</Text>
      </DelayedTooltip>
      <br />
      <DelayedTooltip placement="right" content={Documentation.issues.name}>
        <div style={{marginTop: '.5em'}}>
          <ShowMore items={props.names} maximumElementsToShow={5} />
        </div>
      </DelayedTooltip>
    </>
  );
}

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
              extra={<Link href={`/trace/${node.issue_id}`}>Traces</Link>}>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">Code</Text>
                </Col>
                <Col span={rightSpan}>
                  {node.code}: {node.message}
                </Col>
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
                  <Tooltip title="Click to preview">
                    <Popover
                      title="Source preview"
                      content={
                        <div style={{width: '600px'}}>
                          <Source
                            path={node.filename}
                            location={node.location}
                          />
                        </div>
                      }
                      placement="bottomRight"
                      trigger="click">
                      <CodeTwoTone />
                    </Popover>
                  </Tooltip>{' '}
                  <Text code>{node.filename}</Text>
                </Col>
              </Row>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">Sources</Text>
                </Col>
                <Col span={rightSpan}>
                  <Leaves
                    kind="sources"
                    kinds={node.sources}
                    names={node.source_names}
                    distance={node.min_trace_length_to_sources}
                  />
                </Col>
              </Row>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">Sinks</Text>
                </Col>
                <Col span={rightSpan}>
                  <Leaves
                    kind="sinks"
                    kinds={node.sinks}
                    names={node.sink_names}
                    distance={node.min_trace_length_to_sinks}
                  />
                </Col>
              </Row>
              <Row gutter={gutter}>
                <Col span={leftSpan} style={{textAlign: 'right'}}>
                  <Text type="secondary">Features</Text>
                </Col>
                <Col span={rightSpan}>
                  <DelayedTooltip
                    content={Documentation.issues.feature}
                    placement="left">
                    <div>
                      <ShowMore
                        items={node.features}
                        maximumElementsToShow={5}
                      />
                    </div>
                  </DelayedTooltip>
                </Col>
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
