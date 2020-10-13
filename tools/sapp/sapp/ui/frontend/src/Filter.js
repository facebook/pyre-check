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
import {SearchOutlined, PlusOutlined} from '@ant-design/icons';

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
  form.setFieldsValue({
    codes: props.currentFilter.codes,
    paths: props.currentFilter.paths,
    callables: props.currentFilter.callables,
    min_trace_length_to_sinks: props.currentFilter.min_trace_length_to_sinks,
    max_trace_length_to_sinks: props.currentFilter.max_trace_length_to_sinks,
    min_trace_length_to_sources:
      props.currentFilter.min_trace_length_to_sources,
    max_trace_length_to_sources:
      props.currentFilter.max_trace_length_to_sources,
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
    mutation SaveFilter($name: String!, $description: String, $codes: [Int]) {
      save_filter(
        input: {name: $name, description: $description, codes: $codes}
      ) {
        node {
          name
          description
          codes
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
        codes: props.currentFilter.codes,
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
  const {loading, error, data, refetch} = useQuery(filtersQuery);

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
    props.setCurrentFilter(filterMap[value]);
  };

  const onSave = (filter: FilterDescription): void => {
    props.setCurrentFilter(filter);
    refetch();
  };

  return (
    <Row justify="space-between">
      <Col span={22}>
        <AutoComplete
          style={{width: '100%'}}
          options={options}
          onSelect={onSelect}
          onSearch={setSearch}
          value={props.currentFilter?.name || null}>
          <Input.Search placeholder="saved filter" />
        </AutoComplete>
      </Col>
      <Col>
        <Tooltip title="Save Current Filter">
          <SaveFilterModal
            currentFilter={props.currentFilter}
            visible={saveModalVisible}
            hide={() => setSaveModalVisible(false)}
            onSave={onSave}
          />
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
