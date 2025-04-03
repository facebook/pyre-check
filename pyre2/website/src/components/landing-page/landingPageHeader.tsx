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
import Firefly from './firefly';
import typography from './typography';
import { display } from 'docusaurus-plugin-internaldocs-fb/internal/internComponent';
import { StyleXClassNameTag } from '@stylexjs/stylex/lib/StyleXTypes';

export default function LandingPageHeader(): React.ReactElement {
    return (
        <header {...stylex.props(styles.featureHero)}>
            <div className="container">
                <h1 {...stylex.props(styles.title, typography.h1)}>
                    pyrefly<span>.</span>
                </h1>
                <p {...stylex.props(styles.subtitle, typography.h3)}>
                    <span>
                        A faster Python type checker written in Rust
                    </span>
                </p>
                <section {...stylex.props(styles.buttonGroup)}>
                    <a href="https://github.com/facebook/pyrefly/milestone/1"
                        {...stylex.props(styles.button, typography.p)}> Github </a>
                    <a href="https://pyrefly.org/try/" {...stylex.props(styles.button, typography.p)}> Demo </a>
                    <a href="https://pyrefly.org/en/docs/learn-python-typing/" {...stylex.props(styles.button, typography.p)}> Docs </a>
                </section>
                <section {...stylex.props(styles.buttonGroup,
                )}>
                    <p {...stylex.props(typography.p, typography.italic)}>Launching Spring 2025</p>
                </section>
                <section>
                    <Firefly />
                    <Firefly />
                    <Firefly />
                    <Firefly />
                    <Firefly />
                </section>
            </div >
        </header >
    );
}

const styles = stylex.create({
    root: {
        '--title-font-size': '7rem',
        '--subtitle-font-size': '3.5rem',
    },
    featureHero: {
        padding: '7rem 0',
        width: '100%',
        alignItems: 'center',
        position: 'relative',
        overflow: 'hidden',
        background: 'var(--color-background)',
        color: 'var(--color-text)',
        WebkitFontSmoothing: 'antialiased',
        marginLeft: 'auto',
        marginRight: 'auto',
        lineHeight: 1.1,
        flex: 1,
    },
    title: {
        textAlign: 'center',
        marginBottom: '4rem',
        marginTop: '3rem',
        '@media screen and (max-width: 346px)': {
            fontSize: '3.5rem',
        },
        '@media screen and (max-width: 404px)': {
            fontSize: '3.8rem',
        },
        '@media screen and (max-width: 460px)': {
            fontSize: '4rem',
        },
        '@media screen and (max-width: 518px)': {
            fontSize: '4.5rem',
        },
        '@media screen and (max-width: 548px)': {
            fontSize: '5.5rem',
        },
        '@media screen and (max-width: 576px)': {
            fontSize: 'calc(6rem)',
            maxWidth: '100%',
        },
        '@media screen and (max-width: 768px)': {
            fontSize: 'var(--title-font-size)',
        },
    },
    subtitle: {
        textAlign: 'center',
        marginBottom: '1rem',
        '@media screen and (max-width: 346px)': {
            fontSize: '1.5rem',
        },
        '@media screen and (max-width: 404px)': {
            fontSize: '1.8rem',
        },
        '@media screen and (max-width: 460px)': {
            fontSize: '1.9rem',
        },
        '@media screen and (max-width: 518px)': {
            fontSize: '2rem',
        },
        '@media screen and (max-width: 548px)': {
            fontSize: '2.5rem',
        },
        '@media screen and (max-width: 576px)': {
            fontSize: 'calc(3rem)',
            maxWidth: '100%',
        },
        '@media screen and (max-width: 768px)': {
            fontSize: 'var(--subtitle-font-size)',
        },
    },
    link: {
        color: 'var(--color-primary)',
        textDecoration: 'underline',
        transition:
            'color var(--ifm-transition-fast) var(--ifm-transition-timing-default)',
        ':hover': {
            color: '#BA8E23',
            textDecoration: 'var(--ifm-link-decoration)',
        },
    },
    buttonGroup: {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        marginTop: '20px'
    },
    button: {
        padding: '0.75rem 1.5rem',
        borderRadius: '4px',
        border: '1px solid var(--color-text)',
        backgroundColor: 'transparent',
        color: 'var(--color-text)',
        cursor: 'pointer',
        transition: 'all 0.2s',
        marginLeft: '10px',
        MarginRight: '10px',
    },
});
