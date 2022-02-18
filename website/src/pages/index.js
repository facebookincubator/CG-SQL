/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';

const features = [
  {
    title: 'Stored Procedures Made Easy',
    description: (
      <>
        Create highly complex stored procedures with very large queries, without
        the manual code checking.
      </>
    ),
  },
  {
    title: 'Take Control of the Schema',
    description: (
      <>
        Manage and upgrade your schema automatically with the annotation system
        of CG/SQL.
      </>
    ),
  },
  {
    title: 'Create Well-Tested Code',
    description: (
      <>
        Ensure SQLite API's and the schema's consistency with annotated
        procedures.
      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
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

function Video({heading, title, url}) {
  return (
    <div id="video" className={styles.videoSection}>
      <div className="container padding-vert--xl text--left">
        <div className="row">
          <div className="col">
            <h1 className="text--center margin-bottom--lg">{heading}</h1>
            <div align="center">
              <iframe
                width="560"
                height="315"
                title={title}
                src={url}
                frameborder="0"
                allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                allowfullscreen></iframe>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="CG/SQL is a compiler that converts a SQL Stored Procedure like language into C for SQLite. CG/CQL also generates other useful artifacts for testing and schema maintenance.">
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <img
            className={styles.heroLogo}
            src="img/CGSQL-Icon.png"
            alt="CG/SQL Logo"
            width="170"
          />
          <h1 className="hero__title">{siteConfig.title}</h1>
          <p className="hero__subtitle">{siteConfig.tagline}</p>
          <div className={styles.buttons}>
            <Link
              className={clsx(
                'button button--outline button--secondary button--lg',
                styles.buttons,
              )}
              to={useBaseUrl('docs/introduction')}>
              Get Started
            </Link>
            <Link
              className={clsx(
                'button button--outline button--secondary button--lg',
                styles.buttons,
              )}
              to={'#video'}>
              Watch Demos
            </Link>
          </div>
        </div>
      </header>
      <main>
      <Video heading="Watch Brief Intro Video" 
        title="Explain Like I'm 5: CG/SQL" 
        url="https://www.youtube.com/embed/gQbbdcLLYHs"/>
        <div className="container padding-vert--xl text--left">
          <div className="row">
            <div className="col">
              <h1 className="text--center">Key Features</h1>
              {features && features.length > 0 && (
                <section className={styles.features}>
                  <div className="container">
                    <div className="row">
                      {features.map(({title, imageUrl, description}) => (
                        <Feature
                          key={title}
                          title={title}
                          imageUrl={imageUrl}
                          description={description}
                        />
                      ))}
                    </div>
                  </div>
                </section>
              )}
            </div>
          </div>
        </div>
        <Video heading="See CG/SQL in Practice" 
         title="See CG/SQL in Practice" 
         url="https://www.youtube.com/embed/videoseries?list=PLJE37RiwD_9pvAT-1bpuCx7CGM-Nkwjj0"/>
      </main>
    </Layout>
  );
}

export default Home;
