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
  FireOutlined,
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

type IssueDescription = {
  issue_id: number,
  code: number,
  message: string,
  callable: string,
  filename: string,
  location: string,
  sources: $ReadOnlyArray<string>,
  source_names: $ReadOnlyArray<string>,
  sinks: $ReadOnlyArray<string>,
  sink_names: $ReadOnlyArray<string>,
  features: $ReadOnlyArray<string>,
  min_trace_length_to_sources: number,
  min_trace_length_to_sinks: number,
};

export function Issue(
  props: $ReadOnly<{issue: IssueDescription, hideTitle?: boolean}>,
): React$Node {
  const gutter = [8, 8];
  const leftSpan = 4;
  const rightSpan = 20;

  return (
    <Card
      size="small"
      title={
        <>
          <FireOutlined style={{marginRight: '.5em'}} />
          {props.hideTitle ? 'Issue' : <>Issue {props.issue.issue_id}</>}
        </>
      }
      extra={
        props.hideTitle ? null : (
          <Link href={`/trace/${props.issue.issue_id}`}>Traces</Link>
        )
      }>
      <Row gutter={gutter}>
        <Col span={leftSpan} style={{textAlign: 'right'}}>
          <Text type="secondary">Code</Text>
        </Col>
        <Col span={rightSpan}>
          {props.issue.code}: {props.issue.message}
        </Col>
      </Row>
      <Row gutter={gutter}>
        <Col span={leftSpan} style={{textAlign: 'right'}}>
          <Text type="secondary">Callable</Text>
        </Col>
        <Col span={rightSpan}>
          <Text code>{props.issue.callable}</Text>
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
                    path={props.issue.filename}
                    location={props.issue.location}
                  />
                </div>
              }
              placement="bottomRight"
              trigger="click">
              <CodeTwoTone />
            </Popover>
          </Tooltip>{' '}
          <Text code>{props.issue.filename}</Text>
        </Col>
      </Row>
      <Row gutter={gutter}>
        <Col span={leftSpan} style={{textAlign: 'right'}}>
          <Text type="secondary">Sources</Text>
        </Col>
        <Col span={rightSpan}>
          <Leaves
            kind="sources"
            kinds={props.issue.sources}
            names={props.issue.source_names}
            distance={props.issue.min_trace_length_to_sources}
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
            kinds={props.issue.sinks}
            names={props.issue.sink_names}
            distance={props.issue.min_trace_length_to_sinks}
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
                items={props.issue.features}
                maximumElementsToShow={5}
              />
            </div>
          </DelayedTooltip>
        </Col>
      </Row>
    </Card>
  );
}

export function IssueSkeleton(): React$Node {
  return (
    <Card>
      <Skeleton active paragraph={{rows: 10}} />
    </Card>
  );
}
