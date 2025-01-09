/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::env;
use std::fs::File;
use std::path::Path;
use std::path::PathBuf;

fn get_input_path() -> PathBuf {
    match env::var_os("TYPESHED_ROOT") {
        Some(root) => {
            // When building with Buck, typeshed filegroup dir is passed in using TYPESHED_ROOT
            Path::new(&root).join("typeshed")
        }
        None => {
            // When building with Cargo, we could locate typeshed directly using relative dir
            PathBuf::from("third_party/typeshed")
        }
    }
}

fn get_output_path() -> Result<PathBuf, std::env::VarError> {
    // When building with Buck, output artifact path is specified directly using this env var
    match env::var_os("OUT") {
        Some(path) => Ok(PathBuf::from(path)),
        None => {
            // When building with Cargo, this env var is the containing directory of the artifact
            let out_dir = env::var("OUT_DIR")?;
            Ok(Path::new(&out_dir).join("typeshed.tar.zst"))
        }
    }
}

fn main() -> Result<(), std::io::Error> {
    // Only watch for metadata changes to avoid having Cargo repeatedly crawling for
    // changes in the entire typeshed dir.
    println!("cargo::rerun-if-changed=third_party/typeshed_metadata.json");

    let input_path = get_input_path();
    let output_path = get_output_path().unwrap();
    let output_file = File::create(output_path)?;
    let encoder = zstd::stream::write::Encoder::new(output_file, 0)?;
    let mut tar = tar::Builder::new(encoder);
    tar.append_dir_all("", input_path)?;
    let encoder = tar.into_inner()?;
    encoder.finish()?;
    Ok(())
}
