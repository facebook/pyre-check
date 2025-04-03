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

export default function Firefly(): React.ReactElement {
    return <li id="firefly" {...stylex.props(styles.firefly)}></li>;
}

const leftright = stylex.keyframes({
    '0%, 100%': {
        left: '80%',
    },
    '16.666%': {
        left: '95%',
    },
    '33.333%': {
        left: '10%',
    },
    '50%': {
        left: '60%',
    },
    '66.666%': {
        left: '70%',
    },
    '83.333%': {
        left: '5%',
    },
});

const leftright2 = stylex.keyframes({
    '0%, 100%': {
        left: '30%',
    },
    '16.666%': {
        left: '50%',
    },
    '33.333%': {
        left: '20%',
    },
    '50%': {
        left: '80%',
    },
    '66.666%': {
        left: '40%',
    },
    '83.333%': {
        left: '70%',
    },
});

const updown = stylex.keyframes({
    '0%, 100%': {
        top: '1%',
    },
    '25%': {
        top: '90%',
    },
    '50%': {
        top: '50%',
    },
    '75%': {
        top: '95%',
    },
});

const updown2 = stylex.keyframes({
    '0%, 100%': {
        top: '99%',
    },
    '25%': {
        top: '70%',
    },
    '50%': {
        top: '10%',
    },
    '75%': {
        top: '30%',
    },
});

const blinking = stylex.keyframes({
    '0%, 100%': {
        boxShadow: '0 0 1px 1px var(--color-primary)',
        opacity: 0.1,
    },
    '50%': {
        boxShadow: '0 0 5px 5px var(--color-primary)',
        opacity: 1,
    },
});

const styles = stylex.create({
    firefly: {
        listStyleType: 'none',
        borderRadius: '50%',
        backgroundColor: 'var(--color-primary)',
        height: '10px',
        width: '10px',
        opacity: 0,
        position: 'absolute',
        ':nth-of-type(1)': {
            animation: `
        ${leftright} 40s infinite cubic-bezier(0.39, 0, 0.63, 1),
        ${updown} 40s infinite cubic-bezier(0.39, 0, 0.63, 1),
        ${blinking} 4s infinite cubic-bezier(0.39, 0, 0.63, 1)
      `,
        },
        ':nth-of-type(2)': {
            animation: `
        ${leftright2} 40s infinite cubic-bezier(0.39, 0, 0.63, 1),
        ${updown} 60s infinite cubic-bezier(0.39, 0, 0.63, 1),
        ${blinking} 6s infinite cubic-bezier(0.39, 0, 0.63, 1)
      `,
            animationDelay: '1s',
        },
        ':nth-of-type(3)': {
            animation: `
        ${leftright} 60s infinite cubic-bezier(0.39, 0, 0.63, 1),
        ${updown2} 60s infinite cubic-bezier(0.39, 0, 0.63, 1),
        ${blinking} 8s infinite cubic-bezier(0.39, 0, 0.63, 1)
      `,
            animationDelay: '2s',
        },
        ':nth-of-type(4)': {
            animation: `
          ${leftright2} 60s infinite cubic-bezier(0.39, 0, 0.63, 1),
          ${updown2} 40s infinite cubic-bezier(0.39, 0, 0.63, 1),
          ${blinking} 10s infinite cubic-bezier(0.39, 0, 0.63, 1)
        `,
            animationDelay: '3s',
        },
    },
});
