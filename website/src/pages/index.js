/**
 * Copyright (c) Facebook, Inc. and its affiliates.
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
        Pyre is a fast, scalable type checker for large Python 3 codebases,
        designed to help improve code quality and development speed by flagging
        type errors interactively in your terminal or favorite editor.
      </>
    ),
  },
  {
    title: <>Standards Based</>,
    // imageUrl: 'img/undraw_docusaurus_tree.svg',
    description: (
      <>
        We follow the typing standards introduced in PEP484 and PEP526.
      </>
    ),
  },
  {
    title: <>Powered by React</>,
    // imageUrl: 'img/undraw_docusaurus_react.svg',
    description: (
      <>
        Pyre is under active development. We hope you'll join the
        community and try it out on your own projects, leave us
        feedback, and consider contributing!
      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={classnames('col col--4', styles.feature)}>
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
          <img src='img/pyre.svg' alt="Pyre logo." width="100"/>
          <h1 className="hero__title">{siteConfig.title}</h1>
          <p className="hero__subtitle">{siteConfig.tagline}</p>
          <div className={styles.buttons}>
            <Link
              className={classnames(
                'button button--outline button--secondary button--lg',
                styles.getStarted,
              )}
              to={useBaseUrl('docs/installation')}>
              Get Started
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
        <div
          style={{
             display: "flex",
             justifyContent: "center",
             alignItems: "center",
          }}>
           <h3>Pyre in Action</h3>
           <div class="clearfix"></div>
           <script src="https://asciinema.org/a/u1v8WDqHIHhRD8Uk2hzrjNz14.js"
           id="asciicast-u1v8WDqHIHhRD8Uk2hzrjNz14" async></script>
       </div>
      </main>
    </Layout>
  );
}

export default Home;
