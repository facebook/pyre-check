# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Defines the core logic for loading a patch spec file and executing
transforms on upstream typeshed stub files.
"""

from __future__ import annotations

import dataclasses
import difflib
import logging
import pathlib
import sys

from . import buck, patch_specs, transforms, typeshed

logger: logging.Logger = logging.getLogger(__name__)


def compute_diff_view(
    original_code: str,
    patched_code: str,
    path: pathlib.Path,
) -> str:
    def as_diff_lines(content: str) -> list[str]:
        # The difflib code requires content to be split into lines,
        # preserving the trailing newline.
        return [line + "\n" for line in content.splitlines()]

    diff_lines = difflib.context_diff(
        as_diff_lines(original_code),
        as_diff_lines(patched_code),
        fromfile=f"original {path}",
        tofile=f"patched {path}",
    )
    return "".join(diff_lines)


def patch_one_file(
    original_typeshed: typeshed.Typeshed,
    file_patch: patch_specs.FilePatch,
) -> tuple[str, str]:
    original_code = original_typeshed.get_file_content(file_patch.path)
    if original_code is None:
        raise ValueError(f"Could not find content for {file_patch.path}")
    else:
        try:
            patched_code = transforms.apply_patches_in_sequence(
                code=original_code,
                patches=file_patch.patches,
            )
        except NotImplementedError as e:
            e.args = (f"{file_patch.path}: {e.args[0]}",) + e.args[1:]
            raise e
        diff_view = compute_diff_view(
            original_code=original_code,
            patched_code=patched_code,
            path=file_patch.path,
        )
        return patched_code, diff_view


def load_file_patch_from_toml(
    patch_specs_toml: pathlib.Path,
    stub_path: pathlib.Path,
) -> patch_specs.FilePatch:
    file_patches = [
        file_patch
        for file_patch in patch_specs.FilePatch.from_toml_path(patch_specs_toml)
        if file_patch.path == stub_path
    ]
    if len(file_patches) > 1:
        raise RuntimeError(
            f"Found multiple patches for {stub_path}, this should be impossible"
        )
    elif len(file_patches) == 0:
        raise ValueError(f"No patches found in {patch_specs_toml} for {stub_path}")
    return file_patches[0]


def patch_one_file_entrypoint(
    source_root: pathlib.Path,
    relative_path: pathlib.Path,
    patch_specs_toml: pathlib.Path,
    show_diff: bool,
) -> None:
    """
    Plumbing around `patch_one_file` to make patching a single file, viewing the diff,
    and optionally writing the result to disk easy.

    The production flow of applying perfect patches to typeshed won't use this logic,
    we'll just pull a typeshed and apply all patches at once. But this function should
    make it much easier to rapidly iterate on patches for a single stub file.
    """
    original_typeshed = typeshed.DirectoryBackedTypeshed(source_root)
    file_patch = load_file_patch_from_toml(patch_specs_toml, relative_path)
    patched_code, diff_view = patch_one_file(
        original_typeshed=original_typeshed,
        file_patch=file_patch,
    )
    if show_diff:
        sys.stderr.write(f"Diff of original content vs patch:\n{diff_view}\n")
    sys.stdout.write(patched_code)


@dataclasses.dataclass
class PatchResult:
    patched_typeshed: typeshed.Typeshed
    # This is an unexpected hack - Typshed isn't really modeling a typeshed
    # per-se, just a directory of files. It's convenient to use the same code
    # for storing and dumping the diffs from patching.
    patch_diffs: dict[pathlib.Path, str]


def patch_typeshed(
    original_typeshed: typeshed.Typeshed,
    file_patches: list[patch_specs.FilePatch],
) -> PatchResult:
    patch_outputs = {
        file_patch.path: patch_one_file(original_typeshed, file_patch)
        for file_patch in file_patches
    }
    patch_results = {
        path: patched_code for path, (patched_code, _) in patch_outputs.items()
    }
    patch_diffs = {
        path: patched_code for path, (patched_code, _) in patch_outputs.items()
    }
    return PatchResult(
        patched_typeshed=typeshed.PatchedTypeshed(
            base=original_typeshed,
            patch_results=patch_results,
        ),
        patch_diffs=patch_diffs,
    )


def patch_typeshed_directory(
    source_root: pathlib.Path,
    patch_specs_toml: pathlib.Path,
    destination_root: pathlib.Path,
    diffs_directory: pathlib.Path | None,
) -> None:
    file_patches = patch_specs.FilePatch.from_toml_path(patch_specs_toml)
    original_typeshed = typeshed.DirectoryBackedTypeshed(source_root)
    result = patch_typeshed(
        original_typeshed=original_typeshed,
        file_patches=file_patches,
    )
    typeshed.write_to_directory(result.patched_typeshed, destination_root)
    logger.info(f"Wrote patched typeshed to {destination_root}")
    buck.write_buck_file_to_directory(
        buck.generate_buck_file(result.patched_typeshed),
        destination_root,
    )
    logger.info(f"Wrote Buck file to {destination_root}")
    if diffs_directory is not None:
        typeshed.write_content_map_to_directory(result.patch_diffs, diffs_directory)
        logger.info(f"Wrote diffs of all patched stubs to {diffs_directory}")
