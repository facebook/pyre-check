# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Defines the core logic for loading a patch spec file and executing
transforms on upstream typeshed stub files.
"""

from __future__ import annotations

import dataclasses

import difflib
import pathlib
import shutil

from . import patch_specs, transforms, typeshed


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
        fromfile="original {path}",
        tofile="patched {path}",
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
        patched_code = transforms.apply_patches_in_sequence(
            code=original_code,
            patches=file_patch.patches,
        )
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
    source: pathlib.Path,
    stub_path: pathlib.Path,
    patch_specs_toml: pathlib.Path,
    target: pathlib.Path | None,
    overwrite: bool,
) -> None:
    """
    Plumbing around `patch_one_file` to make patching a single file, viewing the diff,
    and optionally writing the result to disk easy.

    The production flow of applying perfect patches to typeshed won't use this logic,
    we'll just pull a typeshed and apply all patches at once. But this function should
    make it much easier to rapidly iterate on patches for a single stub file.
    """
    original_typeshed = typeshed.DirectoryBackedTypeshed(source)
    file_patch = load_file_patch_from_toml(patch_specs_toml, stub_path)
    patched_code, diff_view = patch_one_file(
        original_typeshed=original_typeshed,
        file_patch=file_patch,
    )
    print("Successfully applied patch!")
    print("Diff of original vs patched content:")
    print(diff_view)
    if target is not None:
        if target.exists():
            if overwrite and not target.is_dir():
                target.unlink()
            else:
                raise RuntimeError(
                    f"Refusing to overwrite existing file at {target}. "
                    "Use --overwrite to overwrite a file, remove an existing directory"
                )

        with open(target, "w") as f:
            f.write(patched_code)
        print(f"Wrote output to {target}")


@dataclasses.dataclass
class PatchResult:
    patched_typeshed: typeshed.Typeshed
    # This is an unexpected hack - Typshed isn't really modeling a typeshed
    # per-se, just a directory of files. It's convenient to use the same code
    # for storing and dumping the diffs from patching.
    patch_diffs: typeshed.Typeshed


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
    patch_diff_views = {
        path: patched_code for path, (patched_code, _) in patch_outputs.items()
    }
    return PatchResult(
        patched_typeshed=typeshed.PatchedTypeshed(
            base=original_typeshed,
            patch_results=patch_results,
        ),
        patch_diffs=typeshed.MemoryBackedTypeshed(
            contents=patch_diff_views,
        ),
    )


def patch_typeshed_directory(
    source: pathlib.Path,
    patch_specs_toml: pathlib.Path,
    target: pathlib.Path,
    diffs_directory: pathlib.Path | None,
    overwrite: bool,
) -> None:
    def handle_overwrite_directory(directory: pathlib.Path) -> None:
        if directory.exists():
            if overwrite:
                shutil.rmtree(directory)
            else:
                raise RuntimeError(
                    f"Refusing to overwrite existing {directory}. "
                    "Use --overwrite to overwrite a directory, remove any existing file"
                )

    file_patches = patch_specs.FilePatch.from_toml_path(patch_specs_toml)
    original_typeshed = typeshed.DirectoryBackedTypeshed(source)
    result = patch_typeshed(
        original_typeshed=original_typeshed,
        file_patches=file_patches,
    )
    handle_overwrite_directory(target)
    typeshed.write_to_directory(result.patched_typeshed, target)
    print(f"Wrote patched typeshed to {target}")
    if diffs_directory is not None:
        handle_overwrite_directory(diffs_directory)
        typeshed.write_to_directory(result.patch_diffs, diffs_directory)
        print(f"Wrote diffs of all patched stubs to {diffs_directory}")
