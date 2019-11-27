/**
 * (c) Facebook, Inc. and its affiliates. Confidential and proprietary.
 *
 * @format
 */

import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import {Switch, Route, BrowserRouter as Router} from 'react-router-dom';
import {ApolloClient, gql} from 'apollo-boost';
import {InMemoryCache} from 'apollo-cache-inmemory';
import {HttpLink} from 'apollo-link-http';
import { ApolloProvider, useQuery } from '@apollo/react-hooks';

import Run from './Run.js';
import Issue from './Issue.js';

function Index(_) {
  const { loading, error, data } = useQuery(
    gql`
      {
        allRuns {
          edges {
            node {
              runId
            }
          }
        }
      }
    `,
  );

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error</p>;

  return (
    <div>
      Runs<br/>
      <ul>
        {data.allRuns.edges.map(run => (
          <li key={run.node.runId}>
            <a href={"run/"+run.node.runId}>{run.node.runId}</a>
          </li>
        ))}
      </ul>
    </div>);
}

// Setup GraphQL client.
const cache = new InMemoryCache();
const link = new HttpLink({
  uri: 'http://127.0.0.1:5000/graphql',
});
const client = new ApolloClient({cache, link});

// Setup routing.
ReactDOM.render(
  <ApolloProvider client={client}>
    <Router>
      <Switch>
        <Route exact path="/" component={Index} />
        <Route path="/run/:runId" component={Run} />
        <Route path="/issue/:issueId" component={Issue} />
      </Switch>
    </Router>
  </ApolloProvider>,
  document.getElementById('root'),
);
