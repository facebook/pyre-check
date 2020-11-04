/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import ReactDOM from 'react-dom';
import {Layout} from 'antd';
import './index.css';
import Issues from './Issues';
import Traces from './Traces';
import Runs from './Runs';
import * as serviceWorker from './serviceWorker';
import {Route, BrowserRouter as Router} from 'react-router-dom';
import {ApolloClient, InMemoryCache, ApolloProvider} from '@apollo/client';
import {FireFilled} from '@ant-design/icons';
import {useQuery, gql} from '@apollo/client';
import {Card, Modal, Typography} from 'antd';
import {LoadingOutlined} from '@ant-design/icons';

import 'antd/dist/antd.css';

const {Header, Content, Footer} = Layout;
const {Text} = Typography;

const client = new ApolloClient({
  uri: 'http://localhost:5000/graphql',
  cache: new InMemoryCache(),
});

function IssuesForLatestRun(): React$Node {
  const RunsQuery = gql`
    query Run {
      runs {
        edges {
          node {
            run_id
            date
          }
        }
      }
    }
  `;

  const {loading, error} = useQuery(RunsQuery, {
    onCompleted: data => {
      // TODO(T71492980): pass run id to issues.
      window.location = '/issues';
    },
  });

  if (error) {
    Modal.error({title: 'Unable to load run data', content: error.toString()});
  }

  if (loading) {
    return (
      <Card>
        <div style={{height: '12em', textAlign: 'center', paddingTop: '5em'}}>
          <Text type="secondary">
            <LoadingOutlined />
            <br />
            Loading runs...
          </Text>
        </div>
      </Card>
    );
  }

  return null;
}

const routing = (
  <ApolloProvider client={client}>
    <Router>
      <Layout>
        <Header>
          <div class="logo">
            <FireFilled style={{marginRight: '.2em'}} />
            SAPP
          </div>
        </Header>
        <Content>
          <div class="main">
            <Route exact path="/" component={IssuesForLatestRun} />
            <Route exact path="/runs/" component={Runs} />
            <Route exact path="/issues/" component={Issues} />
            <Route exact path="/traces/:issue_id/" component={Traces} />
          </div>
        </Content>
        <Footer />
      </Layout>
    </Router>
  </ApolloProvider>
);

ReactDOM.render(routing, document.getElementById('root'));

serviceWorker.unregister();
