/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Type definitions for clsx 1.x
// Based on the Flow type definition in flow-typed/npm/clsx_v1.x.x.js

declare module 'clsx' {
    type ClassValue =
        | ClassArray
        | ClassDictionary
        | string
        | number
        | boolean
        | undefined
        | null;

    interface ClassDictionary {
        [id: string]: any;
    }

    interface ClassArray extends Array<ClassValue> { }

    /**
     * A utility function for conditionally joining class names together
     * @param classes Any number of class names or class name dictionaries
     * @returns A string of joined class names
     */
    function clsx(...classes: ClassValue[]): string;

    export = clsx;
}
