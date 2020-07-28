import React from 'react';
import {render} from 'react-dom';
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

function IssueInstances() {
  const {loading, error, data} = useQuery(gql`
    {
      issueInstances {
        location
        filenameId
        filename
        callableId
        isNewIssue
        runId
        issueId
        messageId
        minTraceLengthToSources
        minTraceLengthToSinks
        minTraceLengthToEntrypoints
        rank
        callableCount
      }
    }
  `);

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error :(</p>;

  return data.issueInstances.map(
    ({
      location,
      filenameId,
      filename,
      callableId,
      isNewIssue,
      runId,
      issueId,
      messageId,
      minTraceLengthToSources,
      minTraceLengthToSinks,
      rank,
      callableCount,
      minTraceLengthToEntrypoints,
    }) => (
        <IssueInstance
          location={location}
          filenameId={filenameId}
          filename={filename}
          callableId={callableId}
          isNewIssue={isNewIssue}
          runId={runId}
          issueId={issueId}
          messageId={issueId}
          minTraceLengthToSources={minTraceLengthToSources}
          minTraceLengthToSinks={minTraceLengthToSinks}
          rank={rank}
          callableCount={callableCount}
          minTraceLengthToEntrypoints={minTraceLengthToEntrypoints}
        />
    ),
  );
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
