/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

export enum Project {
    INSTAGRAM = 'Instagram',
    PYTORCH = 'PyTorch',
    EXAMPLE = 'Example',
}

export type ProjectValue = typeof Project[keyof typeof Project];

export enum TypeChecker {
    PYREFLY = 'Pyrefly',
    PYRIGHT = 'Pyright',
    MYPY = 'MyPy',
    PYTYPE = 'Pytype',
    PYRE1 = 'Pyre1',
}

export type TypeCheckerValue = typeof TypeChecker[keyof typeof TypeChecker];
