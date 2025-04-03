/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import type { Config } from 'jest';

const config: Config = {
    // The test environment to use (in this case, jsdom)
    testEnvironment: 'jsdom',
    // The file extensions to look for when searching for tests
    testMatch: ['**/*.test.ts', '**/*.test.tsx'],
    // The module file extensions to resolve
    moduleFileExtensions: ['js', 'jsx', 'json', 'ts', 'tsx'],
    // The transform configuration
    transform: {
        '^.+\\.jsx?': 'babel-jest',
        '^.+\\.tsx?': ['ts-jest', {
            tsconfig: 'tsconfig.json',
        }]
    },
    // Setup files for TypeScript tests
    preset: 'ts-jest',
};

export default config;
