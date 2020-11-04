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
import {useQuery, gql} from '@apollo/client';
import {Breadcrumb, Card, Col, Modal, Row, Typography} from 'antd';
import {LoadingOutlined, SyncOutlined} from '@ant-design/icons';

const {Text, Link} = Typography;

type RunDescription = $ReadOnly<{
  run_id: number,
  date: string,
}>;

function Run(props: $ReadOnly<{run: RunDescription}>): React$Node {
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
    <>
      <Card
        size="small"
        title={
          <>
            <SyncOutlined style={{marginRight: '.5em'}} />
            Run {props.run.run_id}
          </>
        }
        extra={<Link href={`/run/${props.run.run_id}`}>Issues</Link>}>
        <Row gutter={gutter}>
          <Label>Date</Label>
          <Item>
            <Text code>{props.run.date}</Text>
          </Item>
        </Row>
      </Card>
      <br />
    </>
  );
}

export default function Runs(props: $ReadOnly<{}>): React$Node {
  const RunsQuery = gql`
    query Run {
      runs {
        edges {
          node {
            run_id
            date
          }
        }
      }
    }
  `;

  const {loading, error, data} = useQuery(RunsQuery);

  if (error) {
    Modal.error({title: 'Unable to load run data', content: error.toString()});
    return null;
  }

  var content = null;
  if (loading) {
    content = (
      <Card>
        <div style={{height: '12em', textAlign: 'center', paddingTop: '5em'}}>
          <Text type="secondary">
            <LoadingOutlined />
            <br />
            Loading runs...
          </Text>
        </div>
      </Card>
    );
  }

  if (data) {
    content = data.runs.edges.map(edge => <Run run={edge.node} />);
  }

  return (
    <>
      <Breadcrumb style={{margin: '16px 0'}}>
        <Breadcrumb.Item>Runs</Breadcrumb.Item>
      </Breadcrumb>
      {content}
    </>
  );
}
