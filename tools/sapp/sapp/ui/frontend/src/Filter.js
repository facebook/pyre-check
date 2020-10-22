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
  Dropdown,
  Popover,
  Button,
  Form,
  Input,
  InputNumber,
  Menu,
  Modal,
  Row,
  Col,
  Select,
  Tooltip,
  Typography,
} from 'antd';
import {
  FilterOutlined,
  PlusOutlined,
  MinusOutlined,
  InfoCircleOutlined,
  MoreOutlined,
  SaveOutlined,
  DeleteOutlined,
} from '@ant-design/icons';
import {Documentation} from './Documentation.js';

import './Filter.css';

const {Text} = Typography;

type FeatureCondition = {
  mode: string,
  features: $ReadOnlyArray<string>,
};

type FilterDescription = {
  name?: string,
  description?: string,
  codes?: $ReadOnlyArray<number>,
  paths?: $ReadOnlyArray<string>,
  callables?: $ReadOnlyArray<string>,
  features?: $ReadOnlyArray<FeatureCondition>,
  min_trace_length_to_sinks?: number,
  max_trace_length_to_sinks?: number,
  min_trace_length_to_sources?: number,
  max_trace_length_to_sources?: number,
};

const emptyFilter = {
  features: [{mode: 'all of', features: []}],
};

const Label = (props: $ReadOnly<{title: string}>): React$Node => {
  const title = Documentation.filter[props.title];
  return (
    <>
      {props.title.charAt(0).toUpperCase() + props.title.slice(1)}
      {title !== undefined ? (
        <Tooltip title={title}>
          <InfoCircleOutlined style={{marginLeft: 3}} />
        </Tooltip>
      ) : null}
    </>
  );
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
    features: values.features,
    min_trace_length_to_sinks: values.min_trace_length_to_sinks,
    max_trace_length_to_sinks: values.max_trace_length_to_sinks,
    min_trace_length_to_sources: values.min_trace_length_to_sources,
    max_trace_length_to_sources: values.max_trace_length_to_sources,
  });

  const [appliedFilter, setAppliedFilter] = useState<FilterDescription>(
    emptyFilter,
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

  const featuresQuery = gql`
    query Features {
      features {
        edges {
          node {
            feature
          }
        }
      }
    }
  `;
  const {data: features} = useQuery(featuresQuery);

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
      autoComplete="off"
      initialValues={{remember: true}}
      onFinish={onFinish}
      onFieldsChange={() => props.setCurrentFilter(form.getFieldsValue())}>
      <Form.Item label={<Label title="codes" />} name="codes">
        <Select
          mode="multiple"
          options={(codes?.codes?.edges || []).map(edge => {
            return {
              value: edge.node.code,
            };
          })}
        />
      </Form.Item>
      <Form.Item label={<Label title="paths" />} name="paths">
        <Select
          mode="multiple"
          options={(paths?.paths?.edges || []).map(edge => {
            return {
              value: edge.node.path,
            };
          })}
        />
      </Form.Item>
      <Form.Item label={<Label title="callables" />} name="callables">
        <Select
          mode="multiple"
          options={(callables?.callables?.edges || []).map(edge => {
            return {
              value: edge.node.callable,
            };
          })}
        />
      </Form.Item>
      <Form.Item>
        {(props.currentFilter.features || []).map((feature, index) => {
          return (
            <Form.Item
              key={index}
              label={index === 0 ? <Label title="features" /> : null}>
              <Row style={{marginTop: 5}}>
                <Col span={6}>
                  <Form.Item name={['features', index, 'mode']}>
                    <Select
                      options={[
                        {value: 'all of'},
                        {value: 'any of'},
                        {value: 'none of'},
                      ]}
                    />
                  </Form.Item>
                </Col>
                <Col span={15}>
                  <Form.Item name={['features', index, 'features']}>
                    <Select
                      mode="multiple"
                      options={(features?.features?.edges || []).map(edge => {
                        return {
                          value: edge.node.feature,
                        };
                      })}
                    />
                  </Form.Item>
                </Col>
                <Col span={3} style={{textAlign: 'right'}}>
                  <Button
                    type="dashed"
                    onClick={() => {
                      const newFeatureConditions = (
                        props.currentFilter.features || []
                      ).filter((_, filterIndex) => index !== filterIndex);
                      props.setCurrentFilter({
                        ...props.currentFilter,
                        features: newFeatureConditions,
                      });
                    }}
                    disabled={props.currentFilter.features.length < 2}>
                    <MinusOutlined />
                  </Button>
                </Col>
              </Row>
            </Form.Item>
          );
        })}
        {props.currentFilter.features[props.currentFilter.features.length - 1]
          .features.length > 0 ? (
          <Form.Item style={{marginTop: 5, textAlign: 'right'}}>
            <Button
              type="dashed"
              onClick={() => {
                const newFeatureConditions = [
                  ...(props.currentFilter.features || []),
                  {mode: 'all of', features: []},
                ];
                props.setCurrentFilter({
                  ...props.currentFilter,
                  features: newFeatureConditions,
                });
              }}>
              <PlusOutlined /> Add Condition
            </Button>
          </Form.Item>
        ) : null}
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
              props.setCurrentFilter(emptyFilter);
              form.resetFields();
            }}
            disabled={
              Object.keys(props.currentFilter).length === 0 || props.refetching
            }>
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
    const filter = {
      ...data.save_filter.node,
      ...JSON.parse(data.save_filter.node.json),
    };
    props.onSave(filter);
  };
  const saveFilterMutation = gql`
    mutation SaveFilter($name: String!, $description: String, $json: String!) {
      save_filter(
        input: {name: $name, description: $description, json: $json}
      ) {
        node {
          name
          description
          json
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
        json: JSON.stringify(props.currentFilter),
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
      <Form layout="vertical" form={form} autoComplete="off">
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
            json
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

  const filters = loading
    ? []
    : data.filters.edges.map(edge => {
        const decoded = JSON.parse(edge.node.json);
        return {
          ...edge.node,
          ...decoded,
        };
      });

  const options = filters
    .filter(filter => {
      if (search === null) {
        return true;
      }
      return (
        filter.name.toLowerCase().includes(search.toLowerCase()) ||
        filter.description?.toLowerCase()?.includes(search.toLowerCase())
      );
    })
    .map(filter => {
      return {
        value: filter.name,
        label: (
          <div>
            {filter.name}
            <br />
            <Text type="secondary">{filter.description}</Text>
          </div>
        ),
      };
    });

  var filterMap = {};
  filters.forEach(filter => (filterMap[filter.name] = filter));
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
    props.setCurrentFilter(emptyFilter);
  };

  return (
    <Row justify="space-between">
      <Col span={22}>
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
        <SaveFilterModal
          currentFilter={props.currentFilter}
          visible={saveModalVisible}
          hide={() => setSaveModalVisible(false)}
          onSave={onSave}
        />
        <Dropdown
          overlay={
            <Menu>
              <Menu.Item
                disabled={
                  Object.keys(props.currentFilter).length === 0 ||
                  props.currentFilter?.name !== undefined
                }
                onClick={() => setSaveModalVisible(true)}
                icon={<SaveOutlined />}>
                Save...
              </Menu.Item>
              <Menu.Item
                disabled={props.currentFilter?.name === undefined}
                onClick={() => setDeleteModalVisible(true)}
                icon={<DeleteOutlined />}>
                Delete...
              </Menu.Item>
            </Menu>
          }
          placement="bottomRight">
          <Button type="text" icon={<MoreOutlined />} ghost />
        </Dropdown>
      </Col>
    </Row>
  );
};

const Filter = (props: {refetch: any, refetching: boolean}) => {
  const [visible, setVisible] = useState(false);
  const [currentFilter, setCurrentFilter] = useState<FilterDescription>(
    emptyFilter,
  );

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
          visible={visible || props.refetching}
          content={content}
          placement="bottomRight"
          onClick={() => setVisible(!visible)}>
          <Button icon={<FilterOutlined />}>Filter</Button>
        </Popover>
      </div>
    </>
  );
};

export default Filter;
