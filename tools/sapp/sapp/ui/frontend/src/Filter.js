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
import {useQuery, gql} from '@apollo/client';
import {
  Popover,
  Button,
  Form,
  Input,
  InputNumber,
  Row,
  Col,
  Divider,
  Select,
} from 'antd';
import {SearchOutlined} from '@ant-design/icons';

const Filter = (props: {refetch: any}) => {
  const [visible, setVisible] = useState(false);

  const codesQuery = gql`
    query Codes {
      codes {
        edges {
          node {
            code
          }
        }
      }
    }
  `;
  const {data: codes} = useQuery(codesQuery);

  const pathsQuery = gql`
    query Paths {
      paths {
        edges {
          node {
            path
          }
        }
      }
    }
  `;
  const {data: paths} = useQuery(pathsQuery);

  const onFinish = values => {
    const split = input => {
      if (input !== '' && input !== undefined) {
        return input.trim().split(';');
      }
      return undefined;
    };

    values.callables = split(values.callables);

    const parse_bound = input => {
      if (input !== '' && input !== undefined) {
        return parseInt(input.trim());
      }
      return undefined;
    };

    values.min_trace_length_to_sinks = parse_bound(
      values.min_trace_length_to_sinks,
    );
    values.max_trace_length_to_sinks = parse_bound(
      values.max_trace_length_to_sinks,
    );
    values.min_trace_length_to_sources = parse_bound(
      values.min_trace_length_to_sources,
    );
    values.max_trace_length_to_sources = parse_bound(
      values.max_trace_length_to_sources,
    );

    props.refetch(values);
    setVisible(false);
  };

  const [formHandle] = Form.useForm();

  const form = (
    <div style={{width: '300px'}}>
      <Form
        labelCol={{span: 10}}
        wrapperCol={{span: 30}}
        form={formHandle}
        name="basic"
        initialValues={{remember: true}}
        onFinish={onFinish}>
        <Form.Item label="Codes" name="codes">
          <Select
            mode="multiple"
            options={(codes?.codes?.edges || []).map(edge => {
              return {
                value: edge.node.code,
              };
            })}
          />
        </Form.Item>
        <Form.Item label="Paths" name="file_names">
          <Select
            mode="multiple"
            options={(paths?.paths?.edges || []).map(edge => {
              return {
                value: edge.node.path,
              };
            })}
          />
        </Form.Item>
        <Form.Item label="Callables" name="callables">
          <Input />
        </Form.Item>
        <Divider orientation="left" plain>
          Trace Lengths
        </Divider>
        <Form.Item label="to Sinks">
          <Row>
            <Col span={8}>
              <Form.Item
                name="min_trace_length_to_sinks"
                style={{marginBottom: 0}}>
                <InputNumber placeholder="min" />
              </Form.Item>
            </Col>
            <Col span={8}>
              <Form.Item
                name="max_trace_length_to_sinks"
                style={{marginBottom: 0}}>
                <InputNumber placeholder="max" />
              </Form.Item>
            </Col>
          </Row>
        </Form.Item>
        <Form.Item label="to Sources">
          <Row>
            <Col span={8}>
              <Form.Item
                name="min_trace_length_to_sources"
                style={{marginBottom: 0}}>
                <InputNumber placeholder="min" />
              </Form.Item>
            </Col>
            <Col span={8}>
              <Form.Item
                name="max_trace_length_to_sources"
                style={{marginBottom: 0}}>
                <InputNumber placeholder="max" />
              </Form.Item>
            </Col>
          </Row>
        </Form.Item>
        <Form.Item wrapperCol={{offset: 10, span: 20}}>
          <Button onClick={() => formHandle.resetFields()}>Clear</Button>{' '}
          <Button type="primary" htmlType="submit">
            Apply
          </Button>
        </Form.Item>
      </Form>
    </div>
  );

  return (
    <>
      <div style={{textAlign: 'right', margin: '10px 0 -40px 0'}}>
        <Popover
          visible={visible}
          content={form}
          placement="bottomRight"
          onClick={() => setVisible(!visible)}>
          <Button icon={<SearchOutlined />}>Filter...</Button>
        </Popover>
      </div>
    </>
  );
};

export default Filter;
