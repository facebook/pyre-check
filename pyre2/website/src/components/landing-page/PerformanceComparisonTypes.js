/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

// TODO: convert this to enum when we migrate to typescript
export type ProjectValue = 'Instagram' | 'PyTorch' | 'Example';
export const Project = Object.freeze({
  INSTAGRAM: 'Instagram',
  PYTORCH: 'PyTorch',
  EXAMPLE: 'Example',
});

// TODO: convert this to enum when we migrate to typescript
export type TypeCheckerValue = 'Pyrefly' | 'Pyright' | 'MyPy' | 'Pytype';
export const TypeChecker = Object.freeze({
  PYREFLY: 'Pyrefly',
  PYRIGHT: 'Pyright',
  MYPY: 'MyPy',
  PYTYPE: 'Pytype',
  PYRE1: 'Pyre1',
});
