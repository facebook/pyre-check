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
import typography from './typography'
interface QuoteCardProps {
  quote: string;
  author: string;
  project: string;
}

export default function QuoteCard({
  quote,
  author,
  project,
}: QuoteCardProps): React.ReactElement {
  return (
    <div {...stylex.props(styles.quoteCard)}>
      <p {...stylex.props(styles.quoteText, typography.p)}>{quote}</p>
      <div {...stylex.props(styles.quoteAuthor, typography.p)}>
        <strong>{author}</strong>
        <span>{project}</span>
      </div>
    </div>
  );
}

const styles = stylex.create({
  quoteCard: {
    padding: '2rem',
  },
  quoteText: {
    lineHeight: '1.6',
    marginBottom: '1.5rem',
    fontStyle: 'italic',
  },
  quoteAuthor: {
    display: 'flex',
    flexDirection: 'column',
    gap: '0.25rem',
  },
});
