/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

module.exports = {
    // The test environment to use (in this case, jsdom)
    testEnvironment: 'jsdom',
    // The file extensions to look for when searching for tests
    testMatch: ['**/*.test.js', '**/*.spec.js'],
    // The module file extensions to resolve
    moduleFileExtensions: ['js', 'jsx', 'json', 'ts', 'tsx'],
    // The transform configuration
    transform: {
      '^.+\\.js$': 'babel-jest',
      '^.+\\.jsx$': 'babel-jest',
    },
  };
