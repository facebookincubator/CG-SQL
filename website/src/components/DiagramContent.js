/**
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
class DiagramContent extends React.Component {
  render() {
    return (
      <div className="container">
        <div className="row">
          <div className="col">
            <div dangerouslySetInnerHTML={{__html: this.props.html}} />
          </div>
        </div>
      </div>
    );
  }
}

export default DiagramContent;
