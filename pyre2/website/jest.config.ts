/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import type { Config } from '@jest/types';
import { pathsToModuleNameMapper } from 'ts-jest';
import { compilerOptions } from './tsconfig.json';

const config: Config.InitialOptions = {
    // The root directory that Jest should scan for tests and modules
    rootDir: 'src/__tests__',
    // The test environment to use (in this case, jsdom)
    testEnvironment: 'jsdom',
    // The file extensions to look for when searching for tests
    testMatch: ['**/*.test.ts', '**/*.test.tsx'],
    // The module file extensions to resolve
    moduleFileExtensions: ['js', 'jsx', 'json', 'ts', 'tsx'],
    // The transform configuration
    transform: {
        '^.+\\.(js|jsx)$': 'babel-jest',
        '^.+\\.(ts|tsx)$': ['ts-jest', {
            tsconfig: 'tsconfig.json',
        }]
    },
    // Setup files for TypeScript tests
    preset: 'ts-jest',
    // Module name mapper for imports
    roots: ['<rootDir>'],
    modulePaths: [compilerOptions.baseUrl], // <-- This will be set to 'baseUrl' value
    moduleNameMapper: {
        ...pathsToModuleNameMapper(compilerOptions.paths, { prefix: '<rootDir>/' }),
        '@theme/Layout': '<rootDir>/__mocks__/themeMock.ts',
        '\\.(css|less|sass|scss)$': '<rootDir>/__mocks__/stylexMock.ts',
        '@stylexjs/stylex': '<rootDir>/__mocks__/stylexMock.ts',
        '@docusaurus/useDocusaurusContext': '<rootDir>/__mocks__/docusaurusContextMock.ts'
    },
    setupFilesAfterEnv: ['@testing-library/jest-dom'],
};

export default config;
