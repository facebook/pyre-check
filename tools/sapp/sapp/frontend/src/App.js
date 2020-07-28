import React from 'react';
import {
  ApolloClient,
  InMemoryCache,
  ApolloProvider,
  useQuery,
  gql,
} from '@apollo/client';
import IssueInstance from './IssueInstance'

const client = new ApolloClient({
  uri: 'http://localhost:5000/graphql',
  cache: new InMemoryCache(),
});

const ISSUE_QUERY = gql`
    query IssueInstances($after: String) {
      issue_instances(first: 1 after: $after){
        edges {
          node {
            location
            filename_id
            filename {
              contents
            }
            callable_id
            is_new_issue
            run_id
            issue_id
            message_id
            min_trace_length_to_sources
            min_trace_length_to_sinks
            rank
            callable_count
            min_trace_length_to_entrypoints
            issue {
              code
            }
          }
        }
        pageInfo {
          endCursor
        }
      }
    }
  `;

function IssueInstances() {
  const {loading, error, data, fetchMore} = useQuery(ISSUE_QUERY, {variables: {after: null}});

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error :(</p>;

  return (
    <>
      <ul>
        {data.issue_instances.edges.map(({node}) => (
          <IssueInstance
            location={node.location}
            filename_id={node.filename_id}
            filename={node.filename.contents}
            code={node.issue.code}
            callable_id={node.callable_id}
            is_new_issue={node.is_new_issue}
            run_id={node.run_id}
            issue_id={node.issue_id}
            message_id={node.message_id}
            min_trace_length_to_sources={node.min_trace_length_to_sources}
            min_trace_length_to_sinks={node.min_trace_length_to_sinks}
            rank={node.rank}
            callable_count={node.callable_count}
            min_trace_length_to_entrypoints={node.min_trace_length_to_entrypoints}
          />))}
      </ul>
      <button onClick={() => {
        const endCursor = data.issue_instances.pageInfo.endCursor;
        fetchMore({
          variables: {after: endCursor},
          updateQuery: (prevResult, {fetchMoreResult}) => {
            fetchMoreResult.issue_instances.edges = [
              ...prevResult.issue_instances.edges,
              ...fetchMoreResult.issue_instances.edges,
            ];
            return fetchMoreResult
          }
        });
        }}> More </button>
      </>
  )
}

function App() {
  return (
    <ApolloProvider client={client}>
      <div>
        <h2>IssueInstances</h2>
        <IssueInstances />
      </div>
    </ApolloProvider>
  );
}

export default App;
