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
import {Popover, Button} from 'antd';
import {SearchOutlined} from '@ant-design/icons';

const Filter = (props: {refetch: any}) => {
  const [codes, setCodes] = useState('');
  const [file_names, setFileNames] = useState('');
  const [callables, setCallables] = useState('');
  const [min_trace_length_to_sinks, setMinLengthToSink] = useState('');
  const [max_trace_length_to_sinks, setMaxLengthToSink] = useState('');
  const [min_trace_length_to_sources, setMinLengthToSource] = useState('');
  const [max_trace_length_to_sources, setMaxLengthToSource] = useState('');

  function parse_filter(filter) {
    if (filter !== '' && filter != null) {
      return filter.trim().split(';');
    }
    return null;
  }

  const handleSubmit = event => {
    event.preventDefault();

    let variables = {};
    let parsed = parse_filter(codes);
    variables.codes = parsed !== null ? parsed.map(Number) : null;
    variables.names = parse_filter(file_names);
    variables.callables = parse_filter(callables);

    variables.min_trace_length_to_sinks =
      parse_filter(min_trace_length_to_sinks) !== null
        ? parseInt(parse_filter(min_trace_length_to_sinks))
        : null;

    variables.max_trace_length_to_sinks =
      parse_filter(max_trace_length_to_sinks) !== null
        ? parseInt(parse_filter(max_trace_length_to_sinks))
        : null;

    variables.min_trace_length_to_sources =
      parse_filter(min_trace_length_to_sources) !== null
        ? parseInt(parse_filter(min_trace_length_to_sources))
        : null;

    variables.max_trace_length_to_sources =
      parse_filter(max_trace_length_to_sources) !== null
        ? parseInt(parse_filter(max_trace_length_to_sources))
        : null;

    props.refetch(variables);
  };

  const form = (
    <form onSubmit={handleSubmit}>
      <label>
        Codes
        <input
          type="text"
          value={codes}
          onChange={e => setCodes(e.target.value)}
        />
      </label>
      <br />
      <label>
        File Names
        <input
          type="text"
          value={file_names}
          onChange={e => setFileNames(e.target.value)}
        />
      </label>
      <br />
      <label>
        Callables
        <input
          type="text"
          value={callables}
          onChange={e => setCallables(e.target.value)}
        />
      </label>
      <br />
      <label>
        Min Trace Length To Sinks
        <input
          type="text"
          value={min_trace_length_to_sinks}
          onChange={e => setMinLengthToSink(e.target.value)}
        />
      </label>
      <br />
      <label>
        Max Trace Length to Sinks
        <input
          type="text"
          value={max_trace_length_to_sinks}
          onChange={e => setMaxLengthToSink(e.target.value)}
        />
      </label>
      <br />
      <label>
        Min Trace Length To Sources
        <input
          type="text"
          value={min_trace_length_to_sources}
          onChange={e => setMinLengthToSource(e.target.value)}
        />
      </label>
      <br />
      <label>
        Max Trace Length to Sources
        <input
          type="text"
          value={max_trace_length_to_sources}
          onChange={e => setMaxLengthToSource(e.target.value)}
        />
      </label>
      <br />
      <input type="submit" value="Submit" />
    </form>
  );

  return (
    <>
      <div style={{textAlign: 'right', margin: '10px 0 -40px 0'}}>
        <Popover
          content={form}
          placement="bottomRight"
          title="Filter"
          trigger="click">
          <Button type="primary" icon={<SearchOutlined />}>
            Filter
          </Button>
        </Popover>
      </div>
    </>
  );
};

export default Filter;
