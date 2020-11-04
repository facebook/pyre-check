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
  ExclamationCircleOutlined,
  FireOutlined,
  MinusCircleOutlined,
  PlusCircleOutlined,
} from '@ant-design/icons';
import Source from './Source.js';
import {Documentation} from './Documentation.js';
import {HumanReadable} from './HumanReadable';

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
          <Tag color={props.kind === 'sources' ? 'green' : 'red'}>{kind}</Tag>
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

export type IssueDescription = {
  issue_id: number,
  issue_instance_id: number,
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
  is_new_issue: boolean,
};

export function Issue(
  props: $ReadOnly<{
    run_id: number,
    issue: IssueDescription,
    hideTitle?: boolean,
  }>,
): React$Node {
  const gutter = [8, 8];

  const Label = (props: $ReadOnly<{children: React$Node}>): React$Node => {
    return (
      <Col span={4} style={{textAlign: 'right'}}>
        <Text type="secondary">{props.children}</Text>
      </Col>
    );
  };
  const Item = (props: $ReadOnly<{children: React$Node}>): React$Node => {
    return (
      <Col span={20}>
        <Text type="secondary">{props.children}</Text>
      </Col>
    );
  };

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
          <Link
            href={`/run/${props.run_id}/issue/${props.issue.issue_instance_id}`}>
            Traces
          </Link>
        )
      }>
      <Row gutter={gutter}>
        <Label>Code</Label>
        <Item>
          <Text code>{props.issue.code}</Text>
        </Item>
      </Row>
      <Row gutter={gutter}>
        <Label>Description</Label>
        <Item>{props.issue.message}</Item>
      </Row>
      {props.issue.is_new_issue ? (
        <Row gutter={gutter}>
          <Label>Status</Label>
          <Item>
            <Tooltip title={Documentation.issues.likelyNew}>
              <Tag icon={<ExclamationCircleOutlined />} color="warning">
                likely new
              </Tag>
            </Tooltip>
          </Item>
        </Row>
      ) : null}
      <Row gutter={gutter}>
        <Label>Callable</Label>
        <Item>
          <HumanReadable input={props.issue.callable} code />
        </Item>
      </Row>
      <Row gutter={gutter}>
        <Label>Location</Label>
        <Item>
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
          <HumanReadable input={props.issue.filename} code separator="/" />
        </Item>
      </Row>
      <Row gutter={gutter}>
        <Label>Sources</Label>
        <Item>
          <Leaves
            kind="sources"
            kinds={props.issue.sources}
            names={props.issue.source_names}
            distance={props.issue.min_trace_length_to_sources}
          />
        </Item>
      </Row>
      <Row gutter={gutter}>
        <Label>Sinks</Label>
        <Item>
          <Leaves
            kind="sinks"
            kinds={props.issue.sinks}
            names={props.issue.sink_names}
            distance={props.issue.min_trace_length_to_sinks}
          />
        </Item>
      </Row>
      <Row gutter={gutter}>
        <Label>Features</Label>
        <Item>
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
        </Item>
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
