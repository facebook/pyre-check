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
  AutoComplete,
  Divider,
  Popover,
  Button,
  Form,
  Input,
  InputNumber,
  Row,
  Col,
  Select,
  Typography,
} from 'antd';
import {SearchOutlined} from '@ant-design/icons';

const {Text} = Typography;

const FilterForm = (props: {
  form: any,
  refetch: any,
  refetching: boolean,
  setVisible: boolean => void,
  onClear: void => void,
}): React$Node => {
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
    props.refetch(values);
    props.setVisible(false);
  };

  return (
    <Form
      layout="vertical"
      form={props.form}
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
          <Button onClick={props.onClear}>Clear</Button>{' '}
          <Button type="primary" htmlType="submit" loading={props.refetching}>
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
  props: $ReadOnly<{|onChange: FilterNode => mixed, filter: ?FilterNode|}>,
): React$Node => {
  const [search, setSearch] = useState(null);

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

  if (error) {
    return <Alert type="error">{error.toString()}</Alert>;
  }

  const filters = loading ? [] : data.filters.edges;

  const options = filters
    .filter(edge => {
      if (search === null) {
        return true;
      }
      return (
        edge.node.name.toLowerCase().includes(search.toLowerCase()) ||
        edge.node.description.toLowerCase().includes(search.toLowerCase())
      );
    })
    .map(edge => {
      return {
        value: edge.node.name,
        label: (
          <div>
            {edge.node.name}
            <br />
            <Text type="secondary">{edge.node.description}</Text>
          </div>
        ),
      };
    });

  var filterMap = {};
  filters.forEach(edge => (filterMap[edge.node.name] = edge.node));
  const onSelect = (value: string): void => {
    props.onChange(filterMap[value]);
  };

  return (
    <AutoComplete
      style={{width: '100%'}}
      options={options}
      onSelect={onSelect}
      onSearch={setSearch}
      value={props.filter?.name || null}>
      <Input.Search placeholder="saved filter" />
    </AutoComplete>
  );
};

const Filter = (props: {refetch: any, refetching: boolean}) => {
  const [form] = Form.useForm();
  const [visible, setVisible] = useState(false);
  const [filter, setFilter] = useState(null);

  const updateForm = (filter: FilterNode): void => {
    setFilter(filter);
    form.setFieldsValue({
      codes: filter.codes,
    });
  };

  const onClear = (): void => {
    setFilter(null);
    form.resetFields();
  };

  const content = (
    <div style={{width: '500px'}}>
      <SavedFilters onChange={updateForm} filter={filter} />
      <Divider />
      <FilterForm
        form={form}
        refetch={props.refetch}
        refetching={props.refetching}
        setVisible={setVisible}
        onClear={onClear}
      />
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
