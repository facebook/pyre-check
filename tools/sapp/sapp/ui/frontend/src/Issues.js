/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import React, {useEffect, useState} from 'react';
import {useQuery, gql} from '@apollo/client';
import {Modal, Breadcrumb} from 'antd';
import IssuesList from './IssuesList';
import Filter from './Filter';

import './Issues.css';

const IssueQuery = gql`
  query Issue(
    $after: String
    $codes: [Int]
    $paths: [String]
    $callables: [String]
    $features: [FeatureCondition]
    $min_trace_length_to_sinks: Int
    $max_trace_length_to_sinks: Int
    $min_trace_length_to_sources: Int
    $max_trace_length_to_sources: Int
  ) {
    issues(
      first: 20
      after: $after
      codes: $codes
      paths: $paths
      callables: $callables
      features: $features
      min_trace_length_to_sinks: $min_trace_length_to_sinks
      max_trace_length_to_sinks: $max_trace_length_to_sinks
      min_trace_length_to_sources: $min_trace_length_to_sources
      max_trace_length_to_sources: $max_trace_length_to_sources
    ) {
      edges {
        node {
          issue_id
          code
          message
          callable
          filename
          location
          sources
          source_names
          sinks
          sink_names
          features
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

const Issues = () => {
  const [oldData, setOldData] = useState(null);
  const [refetching, setRefetching] = useState(false);
  const {loading, error, data, fetchMore, refetch} = useQuery(IssueQuery);

  // Ridiculous workaround for https://github.com/apollographql/react-apollo/issues/3709.
  const clearAndRefetch = values => {
    setOldData(data);
    setRefetching(true);
    refetch(values);
  };
  useEffect(() => {
    if (data !== oldData) {
      setRefetching(false);
    }
  }, [data, oldData, setOldData]);

  var content = (
    <IssuesList data={data} fetchMore={fetchMore} loading={loading} />
  );

  if (error) {
    content = (
      <Modal title="Error" visible={true} footer={null}>
        <p>{error.toString()}</p>
      </Modal>
    );
  }

  return (
    <>
      <Filter refetch={clearAndRefetch} refetching={refetching} />
      <Breadcrumb style={{margin: '16px 0'}}>
        <Breadcrumb.Item>Issues</Breadcrumb.Item>
      </Breadcrumb>
      {content}
    </>
  );
};

export default Issues;
