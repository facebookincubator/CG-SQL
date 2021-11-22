/**
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import Layout from '@theme/Layout';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import DiagramContent from '../components/DiagramContent';

var resultsTemplate = require('../../../diagrams/railroad_diagram.html');

function Diagrams() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title="Railroad Diagram"
      description="CG/SQL is a compiler that converts a SQL Stored Procedure like language into C for SQLite. CG/CQL also generates other useful artifacts for testing and schema maintenance.">
      <DiagramContent html={resultsTemplate} />
    </Layout>
  );
}

export default Diagrams;
