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
import Filter, {loadFilter, filterToVariables} from './Filter';
import {Issue, IssueSkeleton} from './Issue.js';

import './Issues.css';

type Props = $ReadOnly<{|
  run_id: number,
  data: any,
  fetchMore: any,
  loading: boolean,
|}>;

type State = $ReadOnly<{|
  recently_started_loading: boolean,
|}>;

class IssuesList extends React.Component<Props, State> {
  state: State = {
    recently_started_loading: false,
  };

  constructor(props: Props) {
    super(props);
    // $FlowFixMe
    this._fetchIssues = this._fetchIssues.bind(this);
    // $FlowFixMe
    this._handleScroll = this._handleScroll.bind(this);
  }

  componentDidMount(): void {
    window.addEventListener('scroll', this._handleScroll);
  }

  componentWillUnount(): void {
    window.removeEventListener('scroll', this._handleScroll);
  }

  render() {
    return (
      <>
        {this._renderIssues()}
        {this._renderLoadingSkeleton()}
      </>
    );
  }

  _fetchIssues() {
    const endCursor = this.props.data.issues.pageInfo.endCursor;
    this.props.fetchMore({
      variables: {after: endCursor},
      updateQuery: (prevResult, {fetchMoreResult}) => {
        fetchMoreResult.issues.edges = [
          ...prevResult.issues.edges,
          ...fetchMoreResult.issues.edges,
        ];
        return fetchMoreResult;
      },
    });
  }

  _handleScroll(event: any): void {
    let body = document.body;
    if (body === null) {
      return;
    }
    if (
      !this.props.loading &&
      !this.state.recently_started_loading &&
      window.pageYOffset + body.clientHeight >= body.scrollHeight - 100
    ) {
      this.setState({recently_started_loading: true});
      const captured = this;
      setTimeout(function() {
        // Avoid clobbering the server with too many requests.
        captured.setState({recently_started_loading: false});
      }, 1000);
      this._fetchIssues();
    }
  }

  _renderIssues() {
    if (this.props.loading) {
      return null;
    }

    return (
      <>
        {this.props.data.issues.edges.map(({node}) => (
          <>
            <Issue run_id={this.props.run_id} issue={node} />
            <br />
          </>
        ))}
      </>
    );
  }

  _renderLoadingSkeleton() {
    if (!this.props.loading && !this.state.recently_started_loading) {
      return null;
    }
    return (
      <>
        {[...Array(5).keys()].map(() => (
          <>
            <IssueSkeleton />
            <br />
          </>
        ))}
      </>
    );
  }
}

const IssueQuery = gql`
  query Issue(
    $after: String
    $run_id: Int!
    $codes: [Int]
    $paths: [String]
    $callables: [String]
    $features: [FeatureCondition]
    $min_trace_length_to_sinks: Int
    $max_trace_length_to_sinks: Int
    $min_trace_length_to_sources: Int
    $max_trace_length_to_sources: Int
    $is_new_issue: Boolean
  ) {
    issues(
      first: 20
      after: $after
      run_id: $run_id
      codes: $codes
      paths: $paths
      callables: $callables
      features: $features
      min_trace_length_to_sinks: $min_trace_length_to_sinks
      max_trace_length_to_sinks: $max_trace_length_to_sinks
      min_trace_length_to_sources: $min_trace_length_to_sources
      max_trace_length_to_sources: $max_trace_length_to_sources
      is_new_issue: $is_new_issue
    ) {
      edges {
        node {
          issue_id
          issue_instance_id
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
          is_new_issue
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

const Issues = (props: $ReadOnly<{match: any}>): React$Node => {
  const run_id = props.match.params.run_id;

  const savedFilter = loadFilter();
  const variables = savedFilter ? filterToVariables(savedFilter) : null;

  const [oldData, setOldData] = useState(null);
  const [refetching, setRefetching] = useState(false);
  const {loading, error, data, fetchMore, refetch} = useQuery(IssueQuery, {
    variables: {...variables, run_id},
  });

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
    <IssuesList
      run_id={run_id}
      data={data}
      fetchMore={fetchMore}
      loading={loading}
    />
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
        <Breadcrumb.Item href="/runs">Runs</Breadcrumb.Item>
        <Breadcrumb.Item>Run {run_id}</Breadcrumb.Item>
      </Breadcrumb>
      {content}
    </>
  );
};

export default Issues;
