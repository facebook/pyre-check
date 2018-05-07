/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* List of projects/orgs using your project for the users page */
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
  tagline: 'A fast typechecker for Python',
  url: 'https://github.com/facebookexperimental/pyre-check',
  baseUrl: '/pyre/' /* base url for your project */,
  projectName: 'Pyre',
  headerLinks: [
    {doc: 'installation', label: 'Getting Started'},
    {doc: 'documentation', label: 'Documentation'},
  ],
  users,
  headerIcon: 'img/pyre.png',
  favicon: 'img/favicon.ico',
  colors: {
    primaryColor: '#2d2020',
    secondaryColor: '#aa9493',
  },
  // This copyright info is used in /core/Footer.js and blog rss/atom feeds.
  copyright:
    'Copyright \u{00A9} ' +
    new Date().getFullYear() +
    ' Facebook',
  // organizationName: 'deltice', // or set an env variable ORGANIZATION_NAME
  // projectName: 'test-site', // or set an env variable PROJECT_NAME
  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks
    theme: 'default',
  },
  scripts: ['https://buttons.github.io/buttons.js'],
  // You may provide arbitrary config keys to be used as needed by your template.
  repoUrl: 'https://github.com/facebook/test-site',
  /* On page navigation for the current documentation page */
  // onPageNav: 'separate',
};

module.exports = siteConfig;
