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
      issueInstances(first: 1 after: $after){
        edges {
          node {
            location
            filenameId
            filename {
              contents
            }
            callableId
            isNewIssue
            runId
            issueId
            messageId
            minTraceLengthToSources
            minTraceLengthToSinks
            rank
            callableCount
            minTraceLengthToEntrypoints
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
        {data.issueInstances.edges.map(({node}) => (

          <IssueInstance
            location={node.location}
            filenameId={node.filenameId}
            filename={node.filename.contents}
            code={node.issue.code}
            callableId={node.callableId}
            isNewIssue={node.isNewIssue}
            runId={node.runId}
            issueId={node.issueId}
            messageId={node.issueId}
            minTraceLengthToSources={node.minTraceLengthToSources}
            minTraceLengthToSinks={node.minTraceLengthToSinks}
            rank={node.rank}
            callableCount={node.callableCount}
            minTraceLengthToEntrypoints={node.minTraceLengthToEntrypoints}
          />))}
      </ul>
      <button onClick={() => {
        const endCursor = data.issueInstances.pageInfo.endCursor;
        fetchMore({
          variables: {after: endCursor},
          updateQuery: (prevResult, {fetchMoreResult}) => {
            fetchMoreResult.issueInstances.edges = [
              ...prevResult.issueInstances.edges,
              ...fetchMoreResult.issueInstances.edges,
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
