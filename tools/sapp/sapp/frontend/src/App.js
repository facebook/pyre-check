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
      issues(first: 1 after: $after){
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

function IssueInstances() {
  const {loading, error, data, fetchMore} = useQuery(ISSUE_QUERY, {variables: {after: null}});

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error :(</p>;

  return (
    <>
      <ul>
        {data.issues.edges.map(({node}) => (
          <IssueInstance
            issue_id={node.issue_id}
            filename={node.filename}
            location={node.location}
            code={node.code}
            callable={node.callable}
            message={node.message}
            min_trace_length_to_sources={node.min_trace_length_to_sources}
            min_trace_length_to_sinks={node.min_trace_length_to_sinks}
          />))}
      </ul>
      <button onClick={() => {
        const endCursor = data.issues.pageInfo.endCursor;
        fetchMore({
          variables: {after: endCursor},
          updateQuery: (prevResult, {fetchMoreResult}) => {
            fetchMoreResult.issues.edges = [
              ...prevResult.issues.edges,
              ...fetchMoreResult.issues.edges,
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
