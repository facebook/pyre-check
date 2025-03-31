/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * Type definitions for @stylexjs/stylex
 */

declare module '@stylexjs/stylex' {
    export type StyleXStyles = {
        [key: string]: any;
    };

    export type StyleXKeyframes = {
        [key: string]: {
            [key: string]: string | number;
        };
    };

    export type StyleXStylesArg = StyleXStyles | boolean | null | undefined;

    export function create<T extends StyleXStyles>(styles: T): T;

    export function keyframes(keyframes: StyleXKeyframes): any;

    export function props(...styles: StyleXStylesArg[]): { className: string };

    export function defineVars<T extends Record<string, string>>(vars: T): T;
}
