/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import classnames from 'classnames';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';

const features = [
  {
    title: <>Fast</>,
    // imageUrl: 'img/pyre.svg',
    description: (
      <>
        Built from the ground up to support gradual typing and deliver
        responsive incremental checks. Performant on large codebases with
        millions of lines of Python.
      </>
    ),
  },
  {
    title: <>Integrated</>,
    // imageUrl: 'img/undraw_docusaurus_tree.svg',
    description: (
      <>
        Designed to help improve code quality and development speed by flagging
        type errors interactively in your terminal or live in your favorite
        editor.
      </>
    ),
  },
  {
    title: <>Fully Featured</>,
    // imageUrl: 'img/undraw_docusaurus_react.svg',
    description: (
      <>
        Follows the typing standards introduced in{' '}
        <Link to="https://www.python.org/dev/peps/pep-0484/">PEPs 484</Link>,{' '}
        <Link to="https://www.python.org/dev/peps/pep-0526/">526</Link>,{' '}
        <Link to="https://www.python.org/dev/peps/pep-0612/">612</Link>, and is
        being actively developed and constantly improved.
      </>
    ),
  },
  {
    title: <>Built For Security</>,
    // imageUrl: 'img/undraw_docusaurus_react.svg',
    description: (
      <>
        Pyre ships with <Link to="docs/pysa-basics">Pysa</Link>, a security
        focused static analysis tool we've built to reason about data flows in
        Python applications at scale.
      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={classnames('col col--3', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="A performant type-checker for Python 3.">
      <header className={classnames('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <img src="img/pyre.svg" alt="Pyre logo." width="100" />
          <h1 className="hero__title">{siteConfig.title}</h1>
          <p className="hero__subtitle">{siteConfig.tagline}</p>
          <div className={styles.buttons}>
            <Link
              className={classnames(
                'button button--outline button--secondary button--lg',
                styles.getStarted,
              )}
              to={useBaseUrl('docs/getting-started')}>
              Getting Started
            </Link>
            <Link
              className={classnames(
                'button button--outline button--secondary button--lg',
                styles.getStarted,
              )}
              to={useBaseUrl('play')}>
              Playground
            </Link>
          </div>
        </div>
      </header>
      <main>
        {features && features.length && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {features.map(({title, imageUrl, description}) => (
                  <Feature
                    key={title.id}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}
      </main>
    </Layout>
  );
}

export default Home;
