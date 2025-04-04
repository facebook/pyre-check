/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mock implementation of useDocusaurusContext for testing
const useDocusaurusContext = (): {
    siteConfig: {
        title: string;
        tagline: string;
        description: string;
        url: string;
        baseUrl: string;
        organizationName: string;
        projectName: string;
    };
} => {
    return {
        siteConfig: {
            title: 'Pyrefly',
            tagline: 'A Static Type Checker for Python',
            description: 'Pyrefly is a static type checker for Python',
            url: 'https://pyrefly.io',
            baseUrl: '/',
            organizationName: 'Meta',
            projectName: 'pyrefly',
        },
    };
};

module.exports = useDocusaurusContext;
