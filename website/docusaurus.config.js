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

module.exports = {
  // ...
  title: 'Pyre',
  tagline: 'A performant type-checker for Python 3',
  url: 'https://pyre-check.org',
  baseUrl: '/',
  organizationName: 'facebook',
  projectName: 'pyre-check',
  favicon: 'img/favicon.ico',
  scripts: ['https://buttons.github.io/buttons.js'],
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          // Docs folder path relative to website dir.
          path: '../docs',
          // Sidebars file relative to website dir.
          sidebarPath: require.resolve('./sidebars.json'),
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        // ...
      },
    ],
  ],

  themeConfig: {
    colorMode: {
      disableSwitch: true,
    },
    navbar: {
      logo: {
        alt: 'Pyre Logo',
        src: 'img/integrated_logo_light.png',
      },
      items: [
        {to: 'docs/installation', label: 'Getting Started', position: 'left'},
        {to: 'docs/overview', label: 'Documentation', position: 'left'},
        {
          href: 'https://github.com/facebook/pyre-check',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
      footer: {
      logo: {
        alt: 'Facebook Open Source Logo',
        src: 'https://docusaurus.io/img/oss_logo.png',
        href: 'https://opensource.facebook.com/',
      },
      copyright: `Copyright &#169; ${new Date().getFullYear()} Facebook, Inc.`,
    },
    image: 'img/docusaurus.png',
  },
};
