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
import {useQuery, useMutation, gql} from '@apollo/client';
import {
  Alert,
  AutoComplete,
  Divider,
  Popover,
  Button,
  Form,
  Input,
  InputNumber,
  Modal,
  Row,
  Col,
  Select,
  Tooltip,
  Typography,
} from 'antd';
import {SearchOutlined, PlusOutlined, MinusOutlined} from '@ant-design/icons';

import './Filter.css';

const {Text} = Typography;

type FilterDescription = {
  name?: string,
  description?: string,
  codes?: $ReadOnlyArray<number>,
  paths?: $ReadOnlyArray<string>,
  callables?: $ReadOnlyArray<string>,
  min_trace_length_to_sinks?: number,
  max_trace_length_to_sinks?: number,
  min_trace_length_to_sources?: number,
  max_trace_length_to_sources?: number,
};

const FilterForm = (props: {
  refetch: any,
  refetching: boolean,
  setVisible: boolean => void,
  currentFilter: FilterDescription,
  setCurrentFilter: FilterDescription => void,
}): React$Node => {
  const [form] = Form.useForm();

  var values = props.currentFilter;
  // Remove `null` values to work around display issues in form with lists.
  Object.keys(values).forEach(key => values[key] == null && delete values[key]);
  form.setFieldsValue({
    codes: values.codes,
    paths: values.paths,
    callables: values.callables,
    min_trace_length_to_sinks: values.min_trace_length_to_sinks,
    max_trace_length_to_sinks: values.max_trace_length_to_sinks,
    min_trace_length_to_sources: values.min_trace_length_to_sources,
    max_trace_length_to_sources: values.max_trace_length_to_sources,
  });

  const [appliedFilter, setAppliedFilter] = useState<FilterDescription>({});

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

  const onFinish = (filter: FilterDescription) => {
    setAppliedFilter(filter);
    props.refetch(filter);
    props.setVisible(false);
  };

  return (
    <Form
      layout="vertical"
      form={form}
      name="basic"
      initialValues={{remember: true}}
      onFinish={onFinish}
      onFieldsChange={() => props.setCurrentFilter(form.getFieldsValue())}>
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
      <Form.Item label="Paths" name="paths">
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
      <Divider />
      <Form.Item>
        <div style={{textAlign: 'right'}}>
          <Button
            onClick={() => {
              props.setCurrentFilter({});
              form.resetFields();
            }}
            disabled={Object.keys(props.currentFilter).length === 0}>
            Clear
          </Button>{' '}
          <Button
            type="primary"
            htmlType="submit"
            loading={props.refetching}
            disabled={
              JSON.stringify(appliedFilter) ===
              JSON.stringify(props.currentFilter)
            }>
            Apply
          </Button>
        </div>
      </Form.Item>
    </Form>
  );
};

const SaveFilterModal = (
  props: $ReadOnly<{|
    currentFilter: FilterDescription,
    visible: boolean,
    hide: void => void,
    onSave: FilterDescription => void,
  |}>,
): React$Node => {
  const [form] = Form.useForm();

  const onCompleted = (data: any): void => {
    props.onSave(data.save_filter.node);
  };
  const saveFilterMutation = gql`
    mutation SaveFilter(
      $name: String!
      $description: String
      $codes: [Int]
      $paths: [String]
      $callables: [String]
      $min_trace_length_to_sinks: Int
      $max_trace_length_to_sinks: Int
      $min_trace_length_to_sources: Int
      $max_trace_length_to_sources: Int
    ) {
      save_filter(
        input: {
          name: $name
          description: $description
          codes: $codes
          paths: $paths
          callables: $callables
          min_trace_length_to_sinks: $min_trace_length_to_sinks
          max_trace_length_to_sinks: $max_trace_length_to_sinks
          min_trace_length_to_sources: $min_trace_length_to_sources
          max_trace_length_to_sources: $max_trace_length_to_sources
        }
      ) {
        node {
          name
          description
          codes
          paths
          callables
          min_trace_length_to_sinks
          max_trace_length_to_sinks
          min_trace_length_to_sources
          max_trace_length_to_sources
        }
      }
    }
  `;
  const [saveFilter, {error}] = useMutation(saveFilterMutation, {onCompleted});

  if (error) {
    return <Alert type="error">{error.toString()}</Alert>;
  }

  const onOk = (): void => {
    const values = form.getFieldsValue();
    saveFilter({
      variables: {
        name: values.name,
        description: values.description,
        ...props.currentFilter,
      },
    });
    props.hide();
  };

  return (
    <Modal
      title="Save Filters..."
      visible={props.visible}
      onOk={onOk}
      okText="Save"
      onCancel={props.hide}
      zIndex={2000}>
      <Form layout="vertical" form={form}>
        <Form.Item label="Name" name="name">
          <Input />
        </Form.Item>
        <Form.Item label="Description" name="description">
          <Input />
        </Form.Item>
      </Form>
    </Modal>
  );
};

const SavedFilters = (
  props: $ReadOnly<{|
    currentFilter: FilterDescription,
    setCurrentFilter: FilterDescription => mixed,
  |}>,
): React$Node => {
  const [search, setSearch] = useState(null);
  const [saveModalVisible, setSaveModalVisible] = useState(false);
  const [deleteModalVisible, setDeleteModalVisible] = useState(false);

  const filtersQuery = gql`
    query Filters {
      filters {
        edges {
          node {
            name
            description
            codes
            paths
            callables
            min_trace_length_to_sinks
            max_trace_length_to_sinks
            min_trace_length_to_sources
            max_trace_length_to_sources
          }
        }
      }
    }
  `;
  const {loading, error: filterError, data, refetch} = useQuery(filtersQuery);

  const deleteFilterMutation = gql`
    mutation DeleteFilter($name: String!) {
      delete_filter(input: {name: $name}) {
        clientMutationId
      }
    }
  `;
  const [deleteFilter, {error: deleteError}] = useMutation(
    deleteFilterMutation,
    {
      onCompleted: refetch,
    },
  );

  if (filterError) {
    return <Alert type="error">{filterError.toString()}</Alert>;
  }
  if (deleteError) {
    return <Alert type="error">{deleteError.toString()}</Alert>;
  }

  const filters = loading ? [] : data.filters.edges;

  const options = filters
    .filter(edge => {
      if (search === null) {
        return true;
      }
      return (
        edge.node.name.toLowerCase().includes(search.toLowerCase()) ||
        edge.node.description?.toLowerCase()?.includes(search.toLowerCase())
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
    setSearch(null);
    props.setCurrentFilter(filterMap[value]);
  };

  const onSave = (filter: FilterDescription): void => {
    props.setCurrentFilter(filter);
    refetch();
  };

  const onDelete = (): void => {
    deleteFilter({variables: {name: props.currentFilter.name}});
    setDeleteModalVisible(false);
    props.setCurrentFilter({});
  };

  return (
    <Row justify="space-between">
      <Col span={20}>
        <AutoComplete
          style={{width: '100%'}}
          options={options}
          onSelect={onSelect}
          onSearch={setSearch}
          value={search || props.currentFilter?.name || null}>
          <Input.Search placeholder="saved filter" />
        </AutoComplete>
      </Col>
      <Col>
        <Modal
          visible={deleteModalVisible}
          okText="Delete"
          onOk={onDelete}
          onCancel={() => setDeleteModalVisible(false)}
          zIndex={2000}>
          Do you really want to delete{' '}
          <Text keyboard>{props.currentFilter.name}</Text>
        </Modal>
        <Tooltip title="Delete Selected Filter">
          <Button
            icon={<MinusOutlined />}
            onClick={() => setDeleteModalVisible(true)}
            disabled={props.currentFilter?.name === undefined}
          />
        </Tooltip>{' '}
        <SaveFilterModal
          currentFilter={props.currentFilter}
          visible={saveModalVisible}
          hide={() => setSaveModalVisible(false)}
          onSave={onSave}
        />
        <Tooltip title="Save Current Filter">
          <Button
            icon={<PlusOutlined />}
            onClick={() => setSaveModalVisible(true)}
            disabled={
              Object.keys(props.currentFilter).length === 0 ||
              props.currentFilter?.name !== undefined
            }
          />
        </Tooltip>
      </Col>
    </Row>
  );
};

const Filter = (props: {refetch: any, refetching: boolean}) => {
  const [visible, setVisible] = useState(false);
  const [currentFilter, setCurrentFilter] = useState<FilterDescription>({});

  const content = (
    <div style={{width: '500px'}}>
      <SavedFilters
        currentFilter={currentFilter}
        setCurrentFilter={setCurrentFilter}
      />
      <Divider />
      <FilterForm
        refetch={props.refetch}
        refetching={props.refetching}
        setVisible={setVisible}
        currentFilter={currentFilter}
        setCurrentFilter={setCurrentFilter}
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
