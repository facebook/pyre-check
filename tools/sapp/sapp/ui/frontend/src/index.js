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
import Trace from './Trace';
import * as serviceWorker from './serviceWorker';
import {Route, BrowserRouter as Router} from 'react-router-dom';
import {ApolloClient, InMemoryCache, ApolloProvider} from '@apollo/client';
import {FireFilled} from '@ant-design/icons';

import 'antd/dist/antd.css';

const {Header, Content, Footer} = Layout;

const client = new ApolloClient({
  uri: 'http://localhost:5000/graphql',
  cache: new InMemoryCache(),
});

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
            <Route exact path="/" component={Issues} />
            <Route path="/trace/:issue_id" component={Trace} />
          </div>
        </Content>
        <Footer />
      </Layout>
    </Router>
  </ApolloProvider>
);

ReactDOM.render(routing, document.getElementById('root'));

serviceWorker.unregister();
