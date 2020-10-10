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
  Alert,
  Popover,
  Button,
  Form,
  InputNumber,
  List,
  Row,
  Col,
  Select,
  Skeleton,
  Tabs,
  Typography,
} from 'antd';
import {SearchOutlined} from '@ant-design/icons';

const {TabPane} = Tabs;
const {Text} = Typography;

type Values = {
  codes?: $ReadOnlyArray<number>,
  file_names?: $ReadOnlyArray<string>,
  callables?: string,
  min_trace_length_to_sinks?: number,
  max_trace_length_to_sinks?: number,
  min_trace_length_to_sources?: number,
  max_trace_length_to_sources?: number,
};

const valuesChanged = (submittedValues: Values, values: Values): boolean => {
  return (
    submittedValues.codes !== values.codes ||
    submittedValues.file_names !== values.file_names ||
    submittedValues.callables !== values.callables ||
    submittedValues.min_trace_length_to_sinks !==
      values.min_trace_length_to_sinks ||
    submittedValues.max_trace_length_to_sinks !==
      values.max_trace_length_to_sinks ||
    submittedValues.min_trace_length_to_sources !==
      values.min_trace_length_to_sources ||
    submittedValues.max_trace_length_to_sources !==
      values.max_trace_length_to_sources
  );
};

const valuesSet = (values: Values): boolean => {
  return Boolean(
    (values.codes || []).length > 0 ||
      (values.file_names || []).length > 0 ||
      values.callables ||
      (values.min_trace_length_to_sinks || 0) > 0 ||
      (values.max_trace_length_to_sinks || 0) > 0 ||
      (values.min_trace_length_to_sources || 0) > 0 ||
      (values.max_trace_length_to_sources || 0) > 0,
  );
};

const FilterForm = (props: {
  form: any,
  refetch: any,
  refetching: boolean,
  setVisible: boolean => void,
}): React$Node => {
  const [submittedValues, setSubmittedValues] = useState(
    props.form.getFieldValue(),
  );
  const [currentValues, setCurrentValues] = useState(
    props.form.getFieldValue(),
  );

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

  const callablesQuery = gql`
    query Callables {
      callables {
        edges {
          node {
            callable
          }
        }
      }
    }
  `;
  const {data: callables} = useQuery(callablesQuery);

  const onFinish = values => {
    setSubmittedValues(props.form.getFieldValue());
    props.refetch(values);
    props.setVisible(false);
  };

  return (
    <Form
      layout="vertical"
      form={props.form}
      name="basic"
      initialValues={{remember: true}}
      onFinish={onFinish}
      onValuesChange={() => {
        setCurrentValues(props.form.getFieldValue());
      }}>
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
        <Select
          mode="multiple"
          options={(callables?.callables?.edges || []).map(edge => {
            return {
              value: edge.node.callable,
            };
          })}
        />
      </Form.Item>
      <Form.Item label="Trace Lengths from Sources">
        <Row>
          <Col span={6}>
            <Form.Item
              name="min_trace_length_to_sources"
              style={{marginBottom: 0}}>
              <InputNumber placeholder="min" min={0} />
            </Form.Item>
          </Col>
          <Col span={6}>
            <Form.Item
              name="max_trace_length_to_sources"
              style={{marginBottom: 0}}>
              <InputNumber placeholder="max" min={0} />
            </Form.Item>
          </Col>
        </Row>
      </Form.Item>
      <Form.Item label="Trace Lengths to Sinks">
        <Row>
          <Col span={6}>
            <Form.Item
              name="min_trace_length_to_sinks"
              style={{marginBottom: 0}}>
              <InputNumber placeholder="min" min={0} />
            </Form.Item>
          </Col>
          <Col span={6}>
            <Form.Item
              name="max_trace_length_to_sinks"
              style={{marginBottom: 0}}>
              <InputNumber placeholder="max" min={0} />
            </Form.Item>
          </Col>
        </Row>
      </Form.Item>
      <Form.Item>
        <div style={{textAlign: 'right'}}>
          <Button
            onClick={() => {
              props.form.resetFields();
              setCurrentValues(props.form.getFieldValue());
            }}
            disabled={!valuesSet(currentValues)}>
            Clear
          </Button>{' '}
          <Button
            type="primary"
            htmlType="submit"
            disabled={!valuesChanged(submittedValues, currentValues)}
            loading={props.refetching}>
            Apply
          </Button>
        </div>
      </Form.Item>
    </Form>
  );
};

type FilterNode = {
  name: string,
  description: string,
  codes: $ReadOnlyArray<number>,
};

const SavedFilters = (
  props: $ReadOnly<{|onChange: FilterNode => mixed|}>,
): React$Node => {
  const filtersQuery = gql`
    query Filters {
      filters {
        edges {
          node {
            name
            description
            codes
          }
        }
      }
    }
  `;
  const {loading, error, data} = useQuery(filtersQuery);

  if (loading) {
    return <Skeleton active />;
  }

  if (error) {
    return <Alert type="error">{error.toString()}</Alert>;
  }

  return (
    <List
      dataSource={data.filters.edges}
      renderItem={edge => (
        <List.Item>
          <div
            style={{cursor: 'pointer'}}
            onClick={() => props.onChange(edge.node)}>
            {edge.node.name}
            <br />
            <Text type="secondary">{edge.node.description}</Text>
          </div>
        </List.Item>
      )}
    />
  );
};

const Filter = (props: {refetch: any, refetching: boolean}) => {
  const [form] = Form.useForm();
  const [visible, setVisible] = useState(false);
  const [activeKey, setActiveKey] = useState('custom');

  const updateForm = (filter: FilterNode): void => {
    form.setFieldsValue({
      codes: filter.codes,
    });
    setActiveKey('custom');
    form.submit();
  };

  const content = (
    <div style={{width: '500px'}}>
      <Tabs activeKey={activeKey} onTabClick={setActiveKey} type="card">
        <TabPane tab="Custom" key="custom">
          <FilterForm
            form={form}
            refetch={props.refetch}
            refetching={props.refetching}
            setVisible={setVisible}
          />
        </TabPane>
        <TabPane tab="Saved" key="saved">
          <SavedFilters onChange={updateForm} />
        </TabPane>
      </Tabs>
    </div>
  );

  return (
    <>
      <div style={{textAlign: 'right', margin: '10px 0 -40px 0'}}>
        <Popover
          title="Filter Issues..."
          visible={visible || props.refetching}
          content={content}
          placement="bottomRight"
          onClick={() => setVisible(!visible)}>
          <Button icon={<SearchOutlined />}>Filter...</Button>
        </Popover>
      </div>
    </>
  );
};

export default Filter;
