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
import {Layout, Skeleton, Modal, Card, Breadcrumb} from 'antd';
import Issues from './Issues';
import Filter from './Filter';

import 'antd/dist/antd.css';
import './Application.css';

const {Header, Content, Footer} = Layout;

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

const Application = () => {
  const {loading, error, data, fetchMore, refetch} = useQuery(IssueQuery);

  var content = <Issues data={data} fetchMore={fetchMore} loading={loading} />;

  const skeleton = (
    <>
      <Card>
        <Skeleton active />
      </Card>
      <br />
      <Card>
        <Skeleton active />
      </Card>
      <br />
      <Card>
        <Skeleton active />
      </Card>
      <br />
      <Card>
        <Skeleton active />
      </Card>
      <br />
    </>
  );

  if (loading) {
    content = skeleton;
  }

  if (error) {
    content = (
      <>
        <Modal title="Error" visible={true} footer={null}>
          <p>{error.toString()}</p>
        </Modal>
        {skeleton}
      </>
    );
  }

  return (
    <Layout>
      <Header>
        <div>
          <h1 class="logo">SAPP</h1>
        </div>
      </Header>
      <Content>
        <div class="main">
          <Filter refetch={refetch} />
          <Breadcrumb style={{margin: '16px 0'}}>
            <Breadcrumb.Item>Issues</Breadcrumb.Item>
          </Breadcrumb>
          {content}
        </div>
      </Content>
      <Footer />
    </Layout>
  );
};

export default Application;
