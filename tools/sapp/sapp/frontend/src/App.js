import React, {useState} from 'react';
import {
  ApolloClient,
  InMemoryCache,
  ApolloProvider,
  useQuery,
  gql,
} from '@apollo/client';
import IssueInstances from './IssueInstances';

const client = new ApolloClient({
  uri: 'http://localhost:5000/graphql',
  cache: new InMemoryCache(),
});

const ISSUE_QUERY = gql`
  query IssueInstances($after: String, $codes: [Int], $file_names: [String], $callables: [String]) {
    issues(first: 2, after: $after, codes: $codes, file_names: $file_names, callables: $callables) {
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

function IssuesPage() {
  const [codes, setCodes] = useState("");
  const [file_names, setFileNames] = useState("");
  const [callables, setCallables] = useState("");

  const {loading, error, data, fetchMore, refetch} = useQuery(ISSUE_QUERY);

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error :(</p>;

  function parse_filter(filter) {
    if (filter !== '' && filter != null) {
      return filter.trim().split(';');
    }
    return undefined;
  }

  const handleSubmit = event => {
    event.preventDefault();

    let variables = {};
    variables.codes = parse_filter(codes) !== undefined ? parse_filter(codes).map(Number) : undefined;
    variables.names = parse_filter(file_names);
    variables.callables = parse_filter(callables);

    refetch(variables);
  };

  return (
    <>
      <div>
        <h2>IssueInstances</h2>
        <IssueInstances data={data} fetchMore={fetchMore}/>
        <form onSubmit={handleSubmit}>
          <label>
            Codes
            <input
              type="text"
              value={codes}
              onChange={e => setCodes(e.target.value)}
            />
            File Names
            <input
              type="text"
              value={file_names}
              onChange={e => setFileNames(e.target.value)}
            />
            Callables
            <input
              type="text"
              value={callables}
              onChange={e => setCallables(e.target.value)}
            />
          </label>
          <input type="submit" value="Submit" />
        </form>
      </div>
    </>
  );
}

function App() {
  return (
    <ApolloProvider client={client}>
      <IssuesPage />
    </ApolloProvider>
  );
}

export default App;
