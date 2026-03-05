/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;
use std::path::Path;

use anyhow::Context;
use anyhow::Result;
use rayon::prelude::*;
use serde::Deserialize;

/// Position of an entry within a result file.
#[derive(Debug, Clone)]
pub struct FilePosition {
    /// Index into the files list identifying the file.
    pub file_index: usize,
    /// Byte offset of the JSON entry within the file.
    pub offset: u64,
    /// Byte length of the JSON entry.
    pub length: u64,
}

/// Locations of all data associated with a single callable.
#[derive(Debug, Clone, Default)]
pub struct CallableIndex {
    /// Position of the taint model, if present.
    pub model: Option<FilePosition>,
    /// Positions of all issues associated with this callable.
    pub issues: Vec<FilePosition>,
    /// Position of the higher-order call graph entry, if present.
    pub higher_order_call_graph: Option<FilePosition>,
}

/// Top-level index mapping callable names to their data positions.
#[derive(Debug)]
pub struct Index {
    /// Sorted list of result file paths.
    pub files: Vec<String>,
    /// Map from fully qualified callable name to its index entry.
    pub callables: HashMap<String, CallableIndex>,
}

/// Minimal struct for parsing just the `kind` field from a JSON line.
#[derive(Deserialize)]
struct EntryHeader {
    kind: String,
    data: EntryData,
}

/// Minimal struct for parsing just the `callable` field from the `data` object.
#[derive(Deserialize)]
struct EntryData {
    callable: String,
}

const INDEX_FILE_NAME: &str = "model-explorer-index.db";

const KIND_MODEL: i64 = 0;
const KIND_ISSUE: i64 = 1;
const KIND_HIGHER_ORDER_CALL_GRAPH: i64 = 2;

/// SQLite-backed index for efficient lookups.
pub struct IndexDatabase {
    connection: rusqlite::Connection,
    files: Vec<String>,
}

/// Scan all result files in `result_dir` and build an in-memory index.
pub fn build_index(result_dir: &Path) -> Result<Index> {
    let mut files: Vec<String> = Vec::new();

    for entry in
        fs::read_dir(result_dir).with_context(|| format!("reading {}", result_dir.display()))?
    {
        let entry = entry?;
        let file_name = entry.file_name();
        let name = file_name.to_string_lossy();
        if (name.starts_with("taint-output") && name.ends_with(".json"))
            || (name.starts_with("higher-order-call-graph") && name.ends_with(".json"))
        {
            files.push(
                entry
                    .path()
                    .to_str()
                    .context("non-UTF-8 file path")?
                    .to_string(),
            );
        }
    }

    files.sort();

    let start = std::time::Instant::now();
    eprintln!("Indexing {} files...", files.len());

    // Build a file_index lookup so each parallel task knows its index.
    let file_entries: Vec<(usize, &str)> = files
        .iter()
        .enumerate()
        .map(|(i, f)| (i, f.as_str()))
        .collect();

    // Index files in parallel, each producing its own HashMap.
    let per_file_results: Vec<Result<HashMap<String, CallableIndex>>> = file_entries
        .par_iter()
        .map(|(file_index, file_path)| {
            index_single_file(*file_index, file_path)
                .with_context(|| format!("indexing {}", file_path))
        })
        .collect();

    // Merge per-file results into a single map.
    let mut callables: HashMap<String, CallableIndex> = HashMap::new();
    for file_map in per_file_results {
        for (callable, file_callable_index) in file_map? {
            let entry = callables.entry(callable).or_default();
            if file_callable_index.model.is_some() {
                entry.model = file_callable_index.model;
            }
            entry.issues.extend(file_callable_index.issues);
            if file_callable_index.higher_order_call_graph.is_some() {
                entry.higher_order_call_graph = file_callable_index.higher_order_call_graph;
            }
        }
    }

    // Print summary stats.
    let model_count = callables.values().filter(|c| c.model.is_some()).count();
    let issue_count: usize = callables.values().map(|c| c.issues.len()).sum();
    let call_graph_count = callables
        .values()
        .filter(|c| c.higher_order_call_graph.is_some())
        .count();
    eprintln!(
        "Indexed {} models, {} issues, {} call graphs in {:.1}s",
        model_count,
        issue_count,
        call_graph_count,
        start.elapsed().as_secs_f64()
    );

    Ok(Index { files, callables })
}

/// Index a single NDJSON file, returning a map of callable -> CallableIndex.
fn index_single_file(file_index: usize, file_path: &str) -> Result<HashMap<String, CallableIndex>> {
    let file = File::open(file_path)?;
    let mut reader = BufReader::new(file);
    let mut callables: HashMap<String, CallableIndex> = HashMap::new();
    let mut offset: u64 = 0;
    let mut buf = Vec::new();
    let mut is_first_line = true;

    loop {
        buf.clear();
        let bytes_read = reader.read_until(b'\n', &mut buf)?;
        if bytes_read == 0 {
            break;
        }

        let length = bytes_read as u64;

        if is_first_line {
            is_first_line = false;
            // Skip metadata header (has file_version but no kind).
            offset += length;
            continue;
        }

        // Parse minimal header to get kind and callable.
        let header: EntryHeader = serde_json::from_slice(&buf)
            .with_context(|| format!("parsing entry at byte offset {}", offset))?;
        let position = FilePosition {
            file_index,
            offset,
            length,
        };
        let entry = callables.entry(header.data.callable).or_default();
        match header.kind.as_str() {
            "model" => {
                entry.model = Some(position);
            }
            "issue" => {
                entry.issues.push(position);
            }
            "higher_order_call_graph" => {
                entry.higher_order_call_graph = Some(position);
            }
            other => {
                anyhow::bail!("unknown entry kind {:?} at byte offset {}", other, offset);
            }
        }

        offset += length;
    }

    Ok(callables)
}

/// Read the "data" field from a single JSON entry on disk using the given file position and files list.
pub fn read_entry_from_files(
    files: &[String],
    position: &FilePosition,
) -> Result<serde_json::Value> {
    let file_path = files
        .get(position.file_index)
        .context("file_index out of bounds")?;

    let mut file = File::open(file_path).with_context(|| format!("opening {}", file_path))?;
    file.seek(SeekFrom::Start(position.offset))?;

    let mut buf = vec![0u8; position.length as usize];
    file.read_exact(&mut buf)
        .with_context(|| format!("reading {} bytes from {}", position.length, file_path))?;

    let message: serde_json::Value =
        serde_json::from_slice(&buf).with_context(|| format!("parsing JSON from {}", file_path))?;

    message
        .get("data")
        .cloned()
        .context("missing 'data' field in entry")
}

/// Load the files list from the SQLite database.
fn load_files_from_db(connection: &rusqlite::Connection) -> Result<Vec<String>> {
    let mut stmt = connection.prepare("SELECT file_path FROM files ORDER BY file_index")?;
    let files: Vec<String> = stmt
        .query_map([], |row| row.get(0))?
        .collect::<std::result::Result<Vec<String>, _>>()?;
    Ok(files)
}

/// Write the in-memory index into a SQLite database.
fn write_index_to_db(connection: &rusqlite::Connection, index: &Index) -> Result<()> {
    let start = std::time::Instant::now();
    eprintln!("Writing index to database...");

    // Performance PRAGMAs (page_size must be set before table creation).
    connection.execute_batch(
        "PRAGMA page_size=8192;
         PRAGMA journal_mode=OFF;
         PRAGMA synchronous=OFF;
         PRAGMA locking_mode=EXCLUSIVE;
         PRAGMA cache_size=-200000;",
    )?;

    // Create tables WITHOUT indexes; indexes are built after bulk inserts.
    connection.execute_batch(
        "CREATE TABLE files (file_index INTEGER PRIMARY KEY, file_path TEXT NOT NULL);
         CREATE TABLE callables (callable_index INTEGER PRIMARY KEY, callable TEXT NOT NULL);
         CREATE TABLE entries (callable_index INTEGER NOT NULL, kind INTEGER NOT NULL, file_index INTEGER NOT NULL, offset INTEGER NOT NULL, length INTEGER NOT NULL);",
    )?;

    connection.execute_batch("BEGIN TRANSACTION")?;

    {
        let mut file_stmt =
            connection.prepare("INSERT INTO files (file_index, file_path) VALUES (?1, ?2)")?;
        for (i, path) in index.files.iter().enumerate() {
            file_stmt.execute(rusqlite::params![i as i64, path])?;
        }
    }

    {
        let mut callable_stmt = connection
            .prepare("INSERT INTO callables (callable_index, callable) VALUES (?1, ?2)")?;
        let mut entry_stmt = connection.prepare(
            "INSERT INTO entries (callable_index, kind, file_index, offset, length) VALUES (?1, ?2, ?3, ?4, ?5)",
        )?;
        for (callable_index, (callable, callable_data)) in index.callables.iter().enumerate() {
            callable_stmt.execute(rusqlite::params![callable_index as i64, callable])?;
            if let Some(ref position) = callable_data.model {
                entry_stmt.execute(rusqlite::params![
                    callable_index as i64,
                    KIND_MODEL,
                    position.file_index as i64,
                    position.offset as i64,
                    position.length as i64,
                ])?;
            }
            for position in &callable_data.issues {
                entry_stmt.execute(rusqlite::params![
                    callable_index as i64,
                    KIND_ISSUE,
                    position.file_index as i64,
                    position.offset as i64,
                    position.length as i64,
                ])?;
            }
            if let Some(ref position) = callable_data.higher_order_call_graph {
                entry_stmt.execute(rusqlite::params![
                    callable_index as i64,
                    KIND_HIGHER_ORDER_CALL_GRAPH,
                    position.file_index as i64,
                    position.offset as i64,
                    position.length as i64,
                ])?;
            }
        }
    }

    connection.execute_batch("COMMIT")?;

    // Create indexes after all data is inserted for faster bulk load.
    connection.execute_batch(
        "CREATE INDEX idx_callables_callable ON callables(callable);
         CREATE INDEX idx_entries_callable_index_kind ON entries(callable_index, kind);",
    )?;

    eprintln!(
        "Wrote index to database in {:.1}s",
        start.elapsed().as_secs_f64()
    );

    Ok(())
}

/// Open a cached SQLite index, or build and cache one if absent/stale.
pub fn open_or_build_index(result_dir: &Path) -> Result<IndexDatabase> {
    let index_path = result_dir.join(INDEX_FILE_NAME);

    if index_path.exists() {
        // Check freshness: compare index mtime against all NDJSON files.
        let index_mtime = fs::metadata(&index_path)?.modified()?;
        let mut stale = false;

        for entry in fs::read_dir(result_dir)? {
            let entry = entry?;
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if (name.starts_with("taint-output") && name.ends_with(".json"))
                || (name.starts_with("higher-order-call-graph") && name.ends_with(".json"))
            {
                let file_mtime = entry.metadata()?.modified()?;
                if file_mtime > index_mtime {
                    stale = true;
                    break;
                }
            }
        }

        if !stale {
            let start = std::time::Instant::now();
            let connection = rusqlite::Connection::open(&index_path)
                .with_context(|| format!("opening {}", index_path.display()))?;
            connection.execute_batch("PRAGMA cache_size=-200000;")?;
            let files = load_files_from_db(&connection)?;
            eprintln!(
                "Loaded cached index in {:.1}s",
                start.elapsed().as_secs_f64()
            );
            return Ok(IndexDatabase { connection, files });
        }

        eprintln!("Cached index is stale, rebuilding...");
    }

    let index = build_index(result_dir)?;

    // Delete old database if present.
    if index_path.exists() {
        fs::remove_file(&index_path)
            .with_context(|| format!("removing {}", index_path.display()))?;
    }

    let connection = rusqlite::Connection::open(&index_path)
        .with_context(|| format!("creating {}", index_path.display()))?;
    write_index_to_db(&connection, &index)?;

    let files = load_files_from_db(&connection)?;

    Ok(IndexDatabase { connection, files })
}

impl IndexDatabase {
    /// Get the callable_index for a callable, or error if not found.
    pub fn get_callable_index(&self, callable: &str) -> Result<i64> {
        let mut stmt = self
            .connection
            .prepare_cached("SELECT callable_index FROM callables WHERE callable = ?1")?;
        let mut rows = stmt.query(rusqlite::params![callable])?;
        match rows.next()? {
            Some(row) => Ok(row.get(0)?),
            None => anyhow::bail!("callable `{}` not found", callable),
        }
    }

    /// Check if a callable exists in the index.
    pub fn callable_exists(&self, callable: &str) -> Result<bool> {
        let mut stmt = self
            .connection
            .prepare_cached("SELECT 1 FROM callables WHERE callable = ?1")?;
        let mut rows = stmt.query(rusqlite::params![callable])?;
        Ok(rows.next()?.is_some())
    }

    /// Get the model position for a callable.
    pub fn get_model_position(&self, callable: &str) -> Result<Option<FilePosition>> {
        let callable_index = self.get_callable_index(callable)?;
        let mut stmt = self.connection.prepare_cached(
            "SELECT file_index, offset, length FROM entries WHERE callable_index = ?1 AND kind = ?2",
        )?;
        let mut rows = stmt.query(rusqlite::params![callable_index, KIND_MODEL])?;
        match rows.next()? {
            Some(row) => {
                let file_index: i64 = row.get(0)?;
                let offset: i64 = row.get(1)?;
                let length: i64 = row.get(2)?;
                Ok(Some(FilePosition {
                    file_index: file_index as usize,
                    offset: offset as u64,
                    length: length as u64,
                }))
            }
            None => Ok(None),
        }
    }

    /// Get all issue positions for a callable.
    pub fn get_issue_positions(&self, callable: &str) -> Result<Vec<FilePosition>> {
        let callable_index = self.get_callable_index(callable)?;
        let mut stmt = self.connection.prepare_cached(
            "SELECT file_index, offset, length FROM entries WHERE callable_index = ?1 AND kind = ?2",
        )?;
        let rows = stmt.query_map(rusqlite::params![callable_index, KIND_ISSUE], |row| {
            let file_index: i64 = row.get(0)?;
            let offset: i64 = row.get(1)?;
            let length: i64 = row.get(2)?;
            Ok(FilePosition {
                file_index: file_index as usize,
                offset: offset as u64,
                length: length as u64,
            })
        })?;
        let mut positions = Vec::new();
        for row in rows {
            positions.push(row?);
        }
        Ok(positions)
    }

    /// Get the higher-order call graph position for a callable.
    pub fn get_call_graph_position(&self, callable: &str) -> Result<Option<FilePosition>> {
        let callable_index = self.get_callable_index(callable)?;
        let mut stmt = self.connection.prepare_cached(
            "SELECT file_index, offset, length FROM entries WHERE callable_index = ?1 AND kind = ?2",
        )?;
        let mut rows = stmt.query(rusqlite::params![
            callable_index,
            KIND_HIGHER_ORDER_CALL_GRAPH
        ])?;
        match rows.next()? {
            Some(row) => {
                let file_index: i64 = row.get(0)?;
                let offset: i64 = row.get(1)?;
                let length: i64 = row.get(2)?;
                Ok(Some(FilePosition {
                    file_index: file_index as usize,
                    offset: offset as u64,
                    length: length as u64,
                }))
            }
            None => Ok(None),
        }
    }

    /// Search for callables matching a regex pattern.
    pub fn search_callables(&self, pattern: &regex::Regex) -> Result<Vec<String>> {
        let mut stmt = self
            .connection
            .prepare_cached("SELECT callable FROM callables")?;
        let rows = stmt.query_map([], |row| {
            let callable: String = row.get(0)?;
            Ok(callable)
        })?;
        let mut matches: Vec<String> = Vec::new();
        for row in rows {
            let callable = row?;
            if pattern.is_match(&callable) {
                matches.push(callable);
            }
        }
        matches.sort();
        Ok(matches)
    }

    /// Read a single JSON entry from disk using the given file position.
    pub fn read_entry(&self, position: &FilePosition) -> Result<serde_json::Value> {
        read_entry_from_files(&self.files, position)
    }
}
