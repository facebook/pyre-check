/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* List of projects/orgs using Pyre for the users page */
const users = [
  {
    caption: 'Instagram',
    image: '/pyre/img/ig.png',
    infoLink: 'https://www.instagram.com',
    pinned: true,
  },
];

const siteConfig = {
  title: 'Pyre',
  tagline: 'A performant typechecker for Python',
  url: 'https://pyre-check.org',
  baseUrl: '/',
  organizationName: 'facebook',
  projectName: 'pyre-check',
  headerLinks: [
    { doc: 'installation', label: 'Getting Started' },
    { doc: 'documentation', label: 'Documentation' },
    { href: 'https://github.com/facebook/pyre-check', label: 'GitHub' },
  ],
  disableHeaderTitle: true,
  // TODO(T29078584): Add Algolia search
  users,
  headerIcon: 'img/integrated_logo_light.png',
  favicon: 'img/favicon.ico',
  colors: {
    primaryColor: '#2d2020',
    secondaryColor: '#aa9493',
  },
  copyright:
    'Copyright \u{00A9} ' +
    new Date().getFullYear() +
    ' Facebook',
  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks
    theme: 'default',
  },
  scripts: ['https://buttons.github.io/buttons.js'],
  repoUrl: 'https://github.com/facebook/pyre-check',
};

module.exports = siteConfig;
