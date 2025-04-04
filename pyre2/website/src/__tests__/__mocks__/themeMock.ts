/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import React from 'react';

interface LayoutProps {
    children: React.ReactNode;
    title?: string;
    description?: string;
}

// Mock for @theme/Layout
const Layout = ({ children, title, description }: LayoutProps): React.ReactElement => {
    // Set document.title when Layout is rendered with a title prop
    if (title) {
        document.title = title;
    }

    return React.createElement('div', {}, [
        React.createElement('div', { key: 'title' }, title),
        React.createElement('div', { key: 'description' }, description),
        React.createElement('div', { key: 'children' }, children)
    ]);
};

module.exports = Layout;
