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

interface WhyPyreflyGridItemProps {
    title: string;
    content: string;
}

export default function WhyPyreflyGridItem({
    title,
    content,
}: WhyPyreflyGridItemProps): React.ReactElement {
    return (
        <div {...stylex.props(styles.whyPyreflyCard)}>
            <h3>{title}</h3>
            <p {...stylex.props(styles.contentText)}>{content}</p>
        </div>
    );
}

const styles = stylex.create({
    whyPyreflyCard: {
        padding: '1.5rem',
        borderRadius: '30px',
        boxShadow: '0 2px 8px rgba(0, 0, 0, 0.1)',
    },
    contentText: {
        fontSize: '1.1rem',
        lineHeight: '1.6',
        marginBottom: '0rem',
    },
});
