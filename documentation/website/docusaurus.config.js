/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {fbContent} = require('docusaurus-plugin-internaldocs-fb/internal');

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
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  trailingSlash: true,
  organizationName: 'facebook',
  projectName: 'pyre-check',
  favicon: 'img/favicon.ico',
  scripts: ['https://buttons.github.io/buttons.js'],
  plugins: [
    [
      '@docusaurus/plugin-client-redirects',
      {
        fromExtensions: ['html'],
      },
    ],
  ],
  presets: [
    [
      require.resolve('docusaurus-plugin-internaldocs-fb/docusaurus-preset'),
      {
        docs: {
          // Docs folder path relative to website dir.
          path: 'docs',
          // Sidebars file relative to website dir.
          sidebarPath: require.resolve('./sidebars.js'),
          // Where to point users when they click "Edit this page"
          editUrl: fbContent({
            internal:
              'https://www.internalfb.com/intern/diffusion/FBS/browse/master/fbcode/tools/pyre/documentation/website/',
            external:
              'https://github.com/facebook/pyre-check/tree/main/documentation/website',
          }),
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        enableEditor: true,
        staticDocsProject: 'pyre',
        trackingFile: 'xplat/staticdocs/WATCHED_FILES',
        'remark-code-snippets': {
          baseDir: '../..',
        },
        gtag: {
          trackingID: 'G-8CK1L365DB',
        },

        // ...
      },
    ],
  ],

  themeConfig: {
    announcementBar: {
      id: 'support_ukraine',
      content:
        'Support Ukraine 🇺🇦 <a target="_blank" rel="noopener noreferrer" href="https://opensource.fb.com/support-ukraine"> Help Provide Humanitarian Aid to Ukraine</a>.',
      backgroundColor: '#20232a',
      textColor: '#fff',
      isCloseable: true,
    },
    colorMode: {
      defaultMode: 'light',
      disableSwitch: true,
    },
    navbar: {
      logo: {
        alt: 'Pyre Logo',
        src: 'img/integrated_logo_light.png',
      },
      items: [
        {
          label: 'Documentation',
          position: 'left',
          items: [
            {
              label: 'Type Checking (Pyre)',
              to: 'docs/getting-started',
            },
            {
              label: 'Static Analysis (Pysa)',
              to: 'docs/pysa-basics',
            },
          ],
        },
        ...fbContent({
          internal: [
            {
              to: 'docs/fb/development-getting-started',
              label: 'Development',
              position: 'left',
            },
          ],
          external: [],
        }),
        {
          href: 'https://github.com/facebook/pyre-check',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      logo: {
        alt: 'Meta Open Source Logo',
        src: 'img/meta_open_source_logo.svg',
        href: 'https://opensource.fb.com/',
      },
      links: [
        {
          title: 'Learn',
          items: [
            {
              label: 'Getting Started',
              href: 'docs/getting-started',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
            {
              label: 'Types in Python',
              href: 'docs/types-in-python',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/facebook/pyre-check',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
            {
              label: 'Issues',
              href: 'https://github.com/facebook/pyre-check/issues',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'Change Log',
              href: 'docs/change-log',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
          ],
        },
        {
          title: 'Legal',
          // Please do not remove the privacy and terms, it's a legal requirement.
          items: [
            {
              label: 'Privacy',
              href: 'https://opensource.facebook.com/legal/privacy/',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
            {
              label: 'Terms',
              href: 'https://opensource.facebook.com/legal/terms/',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
          ],
        },
      ],
      copyright: `Copyright &#169; ${new Date().getFullYear()} Meta Platforms, Inc.`,
    },
    image: 'img/docusaurus.png',
  },
  customFields: {
    fbRepoName: 'fbsource',
    ossRepoPath: 'fbcode/tools/pyre',
  },
};
