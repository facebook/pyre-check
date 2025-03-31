/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

declare module '@docusaurus/BrowserOnly' {
    export default function BrowserOnly({
        children,
    }: {
        children: () => React.ReactNode;
    }): React.ReactNode;
}

declare module '@docusaurus/core' {
    const content: any;
    export = content;
}

declare module '@docusaurus/Link' {
    const content: any;
    export = content;
}

declare module '@docusaurus/Translate' {
    const content: any;
    export = content;
}

declare module '@docusaurus/theme-common' {
    const content: any;
    export = content;
}

declare module '@docusaurus/useBaseUrl' {
    const content: any;
    export = content;
}

declare module '@docusaurus/useDocusaurusContext' {
    const content: any;
    export = content;
}

declare module '@docusaurus/utils-common' {
    const content: any;
    export = content;
}

declare module '@theme-original/CodeBlock' {
    const content: any;
    export = content;
}

declare module '@docusaurus/theme-common/internal' {
    const content: any;
    export = content;
}

declare module '@theme/BlogPostAuthors' {
    const content: any;
    export = content;
}

declare module '@theme/BlogSidebar' {
    const content: any;
    export = content;
}

declare module '@theme/DocCardList' {
    const content: any;
    export = content;
}

declare module '@theme/EditThisPage' {
    const content: any;
    export = content;
}

declare module '@theme/Layout' {
    const content: any;
    export = content;
}

declare module '@theme/MDXContent' {
    const content: any;
    export = content;
}

declare module '@theme/Navbar' {
    const content: any;
    export = content;
}

declare module '@theme/TagsListInline' {
    const content: any;
    export = content;
}

declare module '@theme/TOC' {
    const content: any;
    export = content;
}
