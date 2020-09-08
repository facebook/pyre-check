/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useState} from 'react';
import {useQuery, gql} from '@apollo/client';

import Issues from './Issues';

const IssueQuery = gql`
  query Issue(
    $after: String
    $codes: [Int]
    $file_names: [String]
    $callables: [String]
    $min_trace_length_to_sinks: Int
    $max_trace_length_to_sinks: Int
    $min_trace_length_to_sources: Int
    $max_trace_length_to_sources: Int
  ) {
    issues(
      first: 2
      after: $after
      codes: $codes
      file_names: $file_names
      callables: $callables
      min_trace_length_to_sinks: $min_trace_length_to_sinks
      max_trace_length_to_sinks: $max_trace_length_to_sinks
      min_trace_length_to_sources: $min_trace_length_to_sources
      max_trace_length_to_sources: $max_trace_length_to_sources
    ) {
      edges {
        node {
          issue_id
          filename
          location
          code
          callable
          message
          min_trace_length_to_sources
          min_trace_length_to_sinks
        }
      }
      pageInfo {
        endCursor
      }
    }
  }
`;

const Filter = props => {
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
    return undefined;
  }

  const handleSubmit = event => {
    event.preventDefault();

    let variables = {};
    variables.codes =
      parse_filter(codes) !== undefined
        ? parse_filter(codes).map(Number)
        : undefined;
    variables.names = parse_filter(file_names);
    variables.callables = parse_filter(callables);

    variables.min_trace_length_to_sinks =
      parse_filter(min_trace_length_to_sinks) !== undefined
        ? parseInt(parse_filter(min_trace_length_to_sinks))
        : undefined;

    variables.max_trace_length_to_sinks =
      parse_filter(max_trace_length_to_sinks) !== undefined
        ? parseInt(parse_filter(max_trace_length_to_sinks))
        : undefined;

    variables.min_trace_length_to_sources =
      parse_filter(min_trace_length_to_sources) !== undefined
        ? parseInt(parse_filter(min_trace_length_to_sources))
        : undefined;

    variables.max_trace_length_to_sources =
      parse_filter(max_trace_length_to_sources) !== undefined
        ? parseInt(parse_filter(max_trace_length_to_sources))
        : undefined;

    props.refetch(variables);
  };

  return (
    <>
      <h2>Filter</h2>
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
    </>
  );
};

const Application = () => {
  const {loading, error, data, fetchMore, refetch} = useQuery(IssueQuery);

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error:(</p>;

  return (
    <>
      <Filter refetch={refetch} />
      <Issues data={data} fetchMore={fetchMore} />
    </>
  );
};

export default Application;
