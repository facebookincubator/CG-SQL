/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const path = require('path');

module.exports = {
  title: 'CG/SQL',
  tagline: 'Code Generator for SQLite',
  url: 'https://cgsql.dev',
  baseUrl: '/',
  onBrokenLinks: 'ignore',
  favicon: 'img/favicon.ico',
  organizationName: 'facebookincubator',
  projectName: 'cg-sql',
  themeConfig: {
    navbar: {
      title: 'CG/SQL',
      items: [
        {
          to: 'docs/introduction',
          label: 'Docs',
          position: 'left',
        },
        {
          to: 'cql-guide/ch01',
          label: 'CQL Guide',
          position: 'left',
        },
        {
          to: 'cql-guide/int01',
          label: 'CQL Internals',
          position: 'left',
        },
        {to: 'program-diagram', label: 'Railroad Diagram', position: 'left'},
        {to: 'json-diagram', label: 'Railroad Diagram: JSON', position: 'left'},
        {to: 'blog', label: 'Blog', position: 'left'},
        {
          href: 'https://github.com/facebookincubator/CG-SQL',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Learn',
          items: [
            {
              label: 'Getting Started',
              to: 'docs/introduction',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'Youtube',
              href: 'https://www.youtube.com/channel/UC2lTapw2Um90sZpGQVaynEg',
            },
            {
              label: 'Twitter',
              href: 'https://twitter.com/metaOpenSource',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'Blog',
              to: 'blog',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/facebookincubator/CG-SQL',
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
            },
            {
              label: 'Terms',
              href: 'https://opensource.facebook.com/legal/terms/',
            },
          ],
        },
      ],
      logo: {
        alt: 'Meta Platforms Open Source Logo',
        src: 'img/oss_logo.png',
        href: 'https://opensource.facebook.com',
      },
      // Please do not remove the credits, help to publicize Docusaurus :)
      copyright: `Copyright © ${new Date().getFullYear()} Meta Platforms, Inc. Built with Docusaurus.`,
    },
  },
  plugins: [
    [
      '@docusaurus/plugin-content-docs',
      {
        id: 'cql-guide',
        path: '../CQL_Guide',
        routeBasePath: 'cql-guide',
        sidebarPath: require.resolve('./sidebarsGuide.js'),
        showLastUpdateAuthor: true,
        showLastUpdateTime: true,
      },
    ],
    'my-loaders',
  ],
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          path: '../docs',
          sidebarPath: require.resolve('./sidebars.js'),
        },
        googleAnalytics: {
          trackingID: 'UA-44373548-49',
          anonymizeIP: true,
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          editUrl:
            'https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};
