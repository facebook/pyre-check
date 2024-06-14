// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;

#[derive(Debug, clap::Parser)]
struct Opts {
    /// The directory containing snapshot test cases (.rs) and expected output
    /// files (.rs.exp).
    cases: PathBuf,

    /// The rust_to_ocaml binary.
    rust_to_ocaml_bin: PathBuf,

    /// Update the expected output files instead of testing that the actual
    /// output matches the expected.
    #[clap(long)]
    update_snapshots: bool,

    /// Arguments which will be passed to the rust_to_ocaml binary.
    rust_to_ocaml_args: Vec<PathBuf>,
}

const IN_EXT: &str = "rs";
const EXP_EXT: &str = "exp";

fn main() -> anyhow::Result<()> {
    let opts = <Opts as clap::Parser>::from_args();
    if opts.update_snapshots {
        update_snapshots(&opts)?;
    } else {
        run_test_cases(&opts)?;
    }
    Ok(())
}

fn get_test_cases(dir: &Path) -> impl Iterator<Item = PathBuf> + '_ {
    walkdir::WalkDir::new(dir)
        .sort_by_file_name()
        .into_iter()
        .map(|e| e.unwrap())
        .filter(|e| e.file_type().is_file())
        .filter(|e| e.path().extension().and_then(OsStr::to_str) == Some(IN_EXT))
        .map(|e| e.path().to_owned())
}

fn expected_output_path(test_case: &Path) -> PathBuf {
    assert_eq!(test_case.extension().and_then(OsStr::to_str), Some(IN_EXT));
    let mut path = test_case.to_owned();
    path.set_extension(format!("{IN_EXT}.{EXP_EXT}"));
    path
}

fn run_test_cases(opts: &Opts) -> anyhow::Result<()> {
    let mut failures = vec![];
    for path in get_test_cases(&opts.cases) {
        if let Err(diff) = run_test_case(opts, &path)? {
            let rel_path = path.strip_prefix(&opts.cases).unwrap_or(&path).to_owned();
            eprintln!("=== TEST FAILED: {} ==================", rel_path.display());
            eprintln!("{diff}");
            failures.push(rel_path);
        }
    }
    if !failures.is_empty() {
        anyhow::bail!("{} test cases failed: {:?}", failures.len(), failures);
    }
    Ok(())
}

fn run_test_case(opts: &Opts, test_case: &Path) -> anyhow::Result<Result<(), String>> {
    let exp_path = expected_output_path(test_case);
    let expected = std::fs::read_to_string(exp_path)?;
    let actual = rust_to_ocaml(opts, test_case)?;
    if expected == actual {
        Ok(Ok(()))
    } else {
        Ok(Err(similar::TextDiff::from_lines(&expected, &actual)
            .unified_diff()
            .context_radius(10)
            .header("expected", "actual")
            .to_string()))
    }
}

fn rust_to_ocaml(opts: &Opts, rust_file: &Path) -> anyhow::Result<String> {
    let mut cmd = std::process::Command::new(&opts.rust_to_ocaml_bin);
    for arg in &opts.rust_to_ocaml_args {
        cmd.arg(arg);
    }
    cmd.arg(rust_file);
    let output = cmd.output()?;
    let mut output_str = String::from_utf8(output.stdout)?;
    output_str.push_str(std::str::from_utf8(&output.stderr)?);
    Ok(output_str)
}

fn update_snapshots(opts: &Opts) -> anyhow::Result<()> {
    let test_cases: Vec<PathBuf> = get_test_cases(&opts.cases).collect();
    println!(
        "Updating snapshots for {} test cases in {}",
        test_cases.len(),
        opts.cases
            .canonicalize()
            .unwrap_or_else(|_| opts.cases.clone())
            .display(),
    );
    let mut num_updated = 0;
    for test_case in test_cases {
        let exp_path = expected_output_path(&test_case);
        let actual = rust_to_ocaml(opts, &test_case)?;
        if let Ok(expected) = std::fs::read_to_string(&exp_path) {
            if expected == actual {
                continue;
            }
        }
        let mut expected = std::fs::File::create(&exp_path)?;
        std::io::Write::write_all(&mut expected, actual.as_bytes())?;
        num_updated += 1;
    }
    println!("Updated {num_updated} snapshots.");
    Ok(())
}
