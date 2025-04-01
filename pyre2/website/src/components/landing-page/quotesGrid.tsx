/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import * as stylex from '@stylexjs/stylex';
import QuoteCard from './quoteCard';

export default function QuotesGrid(): React.ReactElement {
    return (
        <div {...stylex.props(styles.quotesGrid)}>
            <QuoteCard
                quote="Pyre and Lint are hands down the most important investments Instagram has made into engineering productivity for our Python codebase. We could not have maintained our product dev velocity with so many people working in the codebase, all while serving 2 billion active users, without the safety and security of static analysis."
                author="<TO BE ADDED>"
                project="Instagram Leadership"
            />
            <QuoteCard
                quote="The enhanced type system and clear error messages have made our
        codebase more maintainable and easier to debug."
                author="<TO BE ADDED>"
                project="core Contributor, PyTorch"
            />
            <QuoteCard
                quote="As an early adopter, I'm impressed by Pyrefly's extensibility
        and the active community support. It's the future of Python type
        checking."
                author="<TO BE ADDED>"
                project="<Open Source Project Name - TO BE ADDED>"
            />
        </div>
    );
}

const styles = stylex.create({
    quotesGrid: {
        display: 'grid',
        gridTemplateColumns: 'repeat(3, 1fr)',
        gap: '2rem',
        '@media (max-width: 800px)': {
            gridTemplateColumns: 'repeat(1, 1fr)',
        },
    },

});
