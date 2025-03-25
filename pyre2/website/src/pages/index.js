/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import * as stylex from '@stylexjs/stylex';
import NewLandingPage from './newLandingPage';
import Firefly from '../components/landing-page/firefly';
import LandingPageHeader from '../components/landing-page/landingPageHeader';

// This import serves no runtime purposes, but we import it to force webpack to run babel on it,
// so we can test whether babel can handle newer syntax.
import '../js/parser-playground';

export default component Home() {
  const context = useDocusaurusContext();
  const {siteConfig} = context;

  // TODO (T218370640): replace current landing page with new landing page
  return process.env.INTERNAL_STATIC_DOCS ? (
    <NewLandingPage />
  ) : (
    <Layout
      title="Pyrefly: A Static Type Checker for Python"
      description={siteConfig.description}>
      <LandingPageHeader />
    </Layout>
  );
}

const styles: any = stylex.create({
  root: {
    '--title-font-size': '7rem',
    '--subtitle-font-size': '3.5rem',
  },
  featureHero: {
    padding: '7rem 0',
    width: '100%',
    alignItems: 'center',
    position: 'relative',
    overflow: 'hidden',
    background: 'var(--flow-gray-darker)',
    color: 'var(--off-white)',
    WebkitFontSmoothing: 'antialiased',
    marginLeft: 'auto',
    marginRight: 'auto',
    lineHeight: 1.1,
    flex: 1,
  },
  title: {
    fontFamily: 'Source Serif Pro',
    textAlign: 'center',
    marginBottom: '4rem',
    marginTop: '3rem',
    fontSize: '7rem',
    '@media screen and (max-width: 346px)': {
      fontSize: '3.5rem',
    },
    '@media screen and (max-width: 404px)': {
      fontSize: '3.8rem',
    },
    '@media screen and (max-width: 460px)': {
      fontSize: '4rem',
    },
    '@media screen and (max-width: 518px)': {
      fontSize: '4.5rem',
    },
    '@media screen and (max-width: 548px)': {
      fontSize: '5.5rem',
    },
    '@media screen and (max-width: 576px)': {
      fontSize: 'calc(6rem)',
      maxWidth: '100%',
    },
    '@media screen and (max-width: 768px)': {
      fontSize: 'var(--title-font-size)',
    },
  },
  subtitle: {
    fontFamily: 'Source Serif Pro',
    fontWeight: 'bold',
    textAlign: 'center',
    marginBottom: '1rem',
    fontSize: '3.5rem',
    '@media screen and (max-width: 346px)': {
      fontSize: '1.5rem',
    },
    '@media screen and (max-width: 404px)': {
      fontSize: '1.8rem',
    },
    '@media screen and (max-width: 460px)': {
      fontSize: '1.9rem',
    },
    '@media screen and (max-width: 518px)': {
      fontSize: '2rem',
    },
    '@media screen and (max-width: 548px)': {
      fontSize: '2.5rem',
    },
    '@media screen and (max-width: 576px)': {
      fontSize: 'calc(3rem)',
      maxWidth: '100%',
    },
    '@media screen and (max-width: 768px)': {
      fontSize: 'var(--subtitle-font-size)',
    },
  },
  yellowLink: {
    color: 'var(--flow-yellow)',
    textDecoration: 'underline',
    transition:
      'color var(--ifm-transition-fast) var(--ifm-transition-timing-default)',
    ':hover': {
      color: '#BA8E23',
      textDecoration: 'var(--ifm-link-decoration)',
    },
  },
});

export {styles as landingPageStyles};
