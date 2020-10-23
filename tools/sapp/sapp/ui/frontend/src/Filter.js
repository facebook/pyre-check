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
  Menu,
  Modal,
  Row,
  Col,
  Select,
  Slider,
  Tooltip,
  Typography,
} from 'antd';
import {
  FilterOutlined,
  PlusOutlined,
  MinusCircleOutlined,
  MoreOutlined,
  SaveOutlined,
  DeleteOutlined,
} from '@ant-design/icons';
import {DocumentationTooltip} from './Documentation.js';

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
  trace_lenght_from_sources?: $ReadOnlyArray<number>,
  trace_length_to_sinks?: $ReadOnlyArray<number>,
};

const emptyFilter = {
  features: [{mode: 'all of', features: []}],
};

const infiniteTraceLength = 31;

const Label = (props: $ReadOnly<{label: string}>): React$Node => {
  const title = props.label
    .split(/(?=[A-Z])/)
    .map(value => value.charAt(0).toUpperCase() + value.slice(1))
    .join(' ');
  return (
    <>
      {title}
      <DocumentationTooltip path={'filter.' + props.label} />
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

  const [callablesMode, setCallablesMode] = useState('is');

  var values = props.currentFilter;
  // Remove `null` values to work around display issues in form with lists.
  Object.keys(values).forEach(key => values[key] == null && delete values[key]);
  form.setFieldsValue({
    codes: values.codes,
    paths: values.paths,
    features: values.features,
    trace_length_from_sources: values.trace_length_from_sources,
    trace_length_to_sinks: values.trace_length_to_sinks,
  });
  if (callablesMode === 'is') {
    form.setFieldsValue({
      callables: values.callables,
    });
  }

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
  const {data: codesData} = useQuery(codesQuery);
  const allCodes = (codesData?.codes?.edges || []).map(edge => edge.node.code);

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
  const {data: pathsData} = useQuery(pathsQuery);
  const allPaths = (pathsData?.paths?.edges || []).map(edge => edge.node.path);

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
  const {data: callablesData} = useQuery(callablesQuery);
  const allCallables = (callablesData?.callables?.edges || []).map(
    edge => edge.node.callable,
  );

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
  const {data: featuresData} = useQuery(featuresQuery);
  const allFeatures = (featuresData?.features?.edges || []).map(
    edge => edge.node.feature,
  );

  const onFinish = (filter: FilterDescription) => {
    const rangeValue = value => {
      if (value === 0) {
        return undefined;
      }
      if (value === infiniteTraceLength) {
        return undefined;
      }
      return value;
    };
    setAppliedFilter(filter);
    props.refetch({
      ...filter,
      min_trace_length_to_sources: rangeValue(
        (filter.trace_length_from_sources || [])[0],
      ),
      max_trace_length_to_sources: rangeValue(
        (filter.trace_length_from_sources || [])[1],
      ),
      min_trace_length_to_sinks: rangeValue(
        (filter.trace_length_to_sinks || [])[0],
      ),
      max_trace_length_to_sinks: rangeValue(
        (filter.trace_length_to_sinks || [])[1],
      ),
    });
    props.setVisible(false);
  };

  const onFieldsChange = () => {
    values = form.getFieldsValue();
    var callables = values.callables;
    if (callables !== undefined && !Array.isArray(callables)) {
      if (callables === '') {
        callables = [];
      } else {
        const pattern = callables;
        callables = allCallables.filter(callable => callable.match(pattern));
      }
    }
    props.setCurrentFilter({...values, callables});
  };

  return (
    <Form
      layout="vertical"
      form={form}
      name="basic"
      autoComplete="off"
      initialValues={{remember: true}}
      onFinish={onFinish}
      onFieldsChange={onFieldsChange}>
      <Form.Item label={<Label label="codes" />} name="codes">
        <Row>
          <Col span={22}>
            <Select
              mode="multiple"
              options={allCodes.map(value => ({value}))}
            />
          </Col>
        </Row>
      </Form.Item>
      <Form.Item label={<Label label="paths" />} name="paths">
        <Row>
          <Col span={22}>
            <Select
              mode="multiple"
              options={allPaths.map(value => ({value}))}
            />
          </Col>
        </Row>
      </Form.Item>
      <Form.Item label={<Label label="callables" />}>
        <Row style={{marginTop: 5}}>
          <Col span={6}>
            <Form.Item>
              <Select
                options={[{value: 'is'}, {value: 'matches'}]}
                value={callablesMode}
                onChange={value => {
                  props.setCurrentFilter({
                    ...props.currentFilter,
                    callables: undefined,
                  });
                  setCallablesMode(value);
                }}
              />
            </Form.Item>
          </Col>
          <Col span={16}>
            <Form.Item name="callables">
              {callablesMode === 'is' ? (
                <Select
                  mode="multiple"
                  options={allCallables.map(value => ({value}))}
                />
              ) : (
                <Input
                  placeholder="regular experssion"
                  onChange={onFieldsChange}
                  suffix={
                    <Tooltip
                      title={(props.currentFilter.callables || []).join('\n')}
                      placement="bottom">
                      <Text type="secondary" size="small">{`[${
                        (props.currentFilter.callables || []).length
                      }]`}</Text>
                    </Tooltip>
                  }
                />
              )}
            </Form.Item>
          </Col>
        </Row>
      </Form.Item>
      <div class="form-features">
        <Form.Item>
          {(props.currentFilter.features || []).map((feature, index) => {
            return (
              <Form.Item
                key={index}
                label={index === 0 ? <Label label="features" /> : null}>
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
                  <Col span={16}>
                    <Form.Item name={['features', index, 'features']}>
                      <Select
                        mode="multiple"
                        options={allFeatures.map(value => ({value}))}
                      />
                    </Form.Item>
                  </Col>
                  <Col span={2}>
                    <Button
                      type="text"
                      onClick={() => {
                        const newFeatureConditions = (
                          props.currentFilter.features || []
                        ).filter((_, filterIndex) => index !== filterIndex);
                        props.setCurrentFilter({
                          ...props.currentFilter,
                          features: newFeatureConditions,
                        });
                      }}
                      disabled={props.currentFilter.features.length < 2}
                      icon={<MinusCircleOutlined />}
                    />
                  </Col>
                </Row>
              </Form.Item>
            );
          })}
          <Row>
            <Col span={22}>
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
                }}
                icon={<PlusOutlined />}
                style={{width: '100%', marginTop: 5}}
                block>
                {' '}
                Add Condition
              </Button>
            </Col>
          </Row>
        </Form.Item>
      </div>
      <div class="form-trace-lengths">
        <Form.Item label={<Label label="traceLengths" />}>
          <Row>
            <Col span={6} style={{paddingTop: 4}}>
              <Text type="secondary">From Sources </Text>
            </Col>
            <Col span={15}>
              <Form.Item name="trace_length_from_sources">
                <Slider
                  range
                  min={0}
                  max={infiniteTraceLength}
                  defaultValue={[0, infiniteTraceLength]}
                  marks={{
                    0: '0',
                    31: '+inf',
                  }}
                  tipFormatter={value =>
                    value === infiniteTraceLength ? '+inf' : value
                  }
                />
              </Form.Item>
            </Col>
          </Row>
          <Row>
            <Col span={6} style={{paddingTop: 4}}>
              <Text type="secondary">To Sinks </Text>
            </Col>
            <Col span={15}>
              <Form.Item name="trace_length_to_sinks">
                <Slider
                  range
                  min={0}
                  max={infiniteTraceLength}
                  defaultValue={[0, infiniteTraceLength]}
                  marks={{
                    0: '0',
                    31: '+inf',
                  }}
                  tipFormatter={value =>
                    value === infiniteTraceLength ? '+inf' : value
                  }
                />
              </Form.Item>
            </Col>
          </Row>
        </Form.Item>
      </div>
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
