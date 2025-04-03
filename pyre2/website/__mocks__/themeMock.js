/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

// Mock for @theme/Layout
const Layout = ({ children, title, description }) => {
    // Set document.title when Layout is rendered with a title prop
    if (title) {
      document.title = title;
    }

  return React.createElement('div', {  }, [
    React.createElement('div', { key: 'title' }, title),
    React.createElement('div', { key: 'description' }, description),
    React.createElement('div', { key: 'children' }, children)
  ])
};

module.exports = Layout;
