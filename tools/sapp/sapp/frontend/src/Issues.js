/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import React from 'react';
import {useQuery, gql} from '@apollo/client';
import {Modal, Breadcrumb} from 'antd';
import IssuesList from './IssuesList';
import Filter from './Filter';

import './Issues.css';

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
      first: 20
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

const Issues = () => {
  const {loading, error, data, fetchMore, refetch} = useQuery(IssueQuery);

  var content = (
    <IssuesList data={data} fetchMore={fetchMore} loading={loading} />
  );

  if (error) {
    content = (
      <>
        <Modal title="Error" visible={true} footer={null}>
          <p>{error.toString()}</p>
        </Modal>
      </>
    );
  }

  return (
    <>
      <Filter refetch={refetch} />
      <Breadcrumb style={{margin: '16px 0'}}>
        <Breadcrumb.Item>Issues</Breadcrumb.Item>
      </Breadcrumb>
      {content}
    </>
  );
};

export default Issues;
