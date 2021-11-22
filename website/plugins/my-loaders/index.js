/**
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

module.exports = function(context, options) {
  return {
    name: 'loaders',
    configureWebpack(config, isServer) {
      return {
        module: {
          rules: [
            {
              test: /\.(html)$/,
              use: {
                loader: 'html-loader',
              },
            },
          ],
        },
      };
    },
  };
};
