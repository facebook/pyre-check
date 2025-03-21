/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;
use std::str::FromStr;
use std::thread;
use std::time::Duration;

use crossbeam_channel::bounded;
use crossbeam_channel::RecvTimeoutError;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use pretty_assertions::assert_eq;

use crate::commands::lsp::run_lsp;
use crate::commands::lsp::Args;
use crate::test::util::init_test;

// Fake python root directory (not necessary to exist on the filesystem)
// since we simply send the file contents on file open
const TEST_PYTHON_PATH: &str = "/tmp/test_python_root";

struct TestCase {
    test_messages: Vec<Message>,
    expected_responses: Vec<Response>,
}
fn run_test_lsp(test_case: TestCase) {
    init_test();
    let timeout = Duration::from_secs(25);
    let args = Args {
        search_path: vec![PathBuf::from_str(TEST_PYTHON_PATH).unwrap()],
    };
    let (writer_sender, writer_receiver) = bounded::<Message>(0);
    let (reader_sender, reader_receiver) = bounded::<Message>(0);

    // spawn thread to handle writes from language server to client
    let writer_thread: thread::JoinHandle<Result<(), std::io::Error>> = thread::spawn(move || {
        let mut responses = test_case.expected_responses.clone();

        loop {
            if responses.is_empty() {
                break;
            }

            match writer_receiver.recv_timeout(timeout) {
                Ok(msg) => {
                    match msg {
                        Message::Response(response) => {
                            let expected_response = responses.remove(0);
                            assert_eq!(
                                (response.id, &response.result, &response.error.is_none()),
                                (
                                    expected_response.id,
                                    &expected_response.result,
                                    &expected_response.error.is_none()
                                ),
                                "Response mismatch"
                            );
                        }
                        Message::Notification(notification) => {
                            eprintln!("Received notification: {:?}", notification);
                        }
                        Message::Request(_) => {
                            panic!("Unexpected message {:?}", msg);
                        }
                    };
                }
                Err(RecvTimeoutError::Timeout) => {
                    panic!("Timeout waiting for response. Expected ${:?}.", responses,);
                }
                Err(RecvTimeoutError::Disconnected) => {
                    panic!("Channel disconnected. Expected ${:?}.", responses);
                }
            }
        }

        Ok(())
    });

    // spawn thread to handle reads of messages from client to language server
    let reader_thread: thread::JoinHandle<Result<(), std::io::Error>> = thread::spawn(move || {
        test_case.test_messages.iter().for_each(|msg| {
            if let Err(err) = reader_sender.send_timeout(msg.clone(), timeout) {
                panic!("Failed to send message to language server: {:?}", err);
            }
        });
        Ok(())
    });

    let connection = Connection {
        sender: writer_sender,
        receiver: reader_receiver,
    };

    // spawn thread to run the language server
    let lsp_thread: thread::JoinHandle<Result<(), std::io::Error>> = thread::spawn(move || {
        run_lsp(&connection, || Ok(()), args)
            .map(|_| ())
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))
    });

    let threads = vec![
        ("reader", reader_thread),
        ("writer", writer_thread),
        ("lsp", lsp_thread),
    ];

    for (name, handle) in threads {
        match handle.join() {
            Ok(result) => {
                if let Err(err) = result {
                    panic!("{} thread error: {:?}", name, err);
                }
            }
            Err(err) => panic!("{} thread panicked: {:?}", name, err),
        }
    }
}

fn get_initialize_params() -> serde_json::Value {
    serde_json::json!({
        "rootPath": "/",
        "workspaceFolders": [],
        "processId": std::process::id(),
        "trace": "verbose",
        "clientInfo": { "name": "debug" },
        "capabilities": {
            "workspace": {
                "workspaceFolders": true,
            },
            "textDocument": {
                "publishDiagnostics": {
                    "relatedInformation": true,
                    "versionSupport": false,
                    "tagSupport": {
                        "valueSet": [1, 2],
                    },
                    "codeDescriptionSupport": true,
                    "dataSupport": true,
                },
            },
        },
    })
}

fn get_initialize_messages() -> std::vec::Vec<lsp_server::Message> {
    vec![
        Message::from(Request {
            id: RequestId::from(1),
            method: "initialize".to_owned(),
            params: get_initialize_params(),
        }),
        Message::from(Notification {
            method: "initialized".to_owned(),
            params: serde_json::json!({}),
        }),
    ]
}

fn get_initialize_responses() -> std::vec::Vec<lsp_server::Response> {
    vec![Response {
        id: RequestId::from(1),
        result: Some(serde_json::json!({
            "capabilities": {
                "completionProvider": { "triggerCharacters": ["."]},
                "definitionProvider": true,
                "hoverProvider": true,
                "inlayHintProvider": true,
                "textDocumentSync": 1
            }
        }
        )),
        error: None,
    }]
}

fn build_did_open_notification(path: PathBuf) -> lsp_server::Notification {
    Notification {
        method: "textDocument/didOpen".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": format!("file://{}", path.to_str().unwrap()),
                "languageId": "python",
                "version": 1,
                "text": std::fs::read_to_string(path).unwrap()
            }
        }),
    }
}

fn get_test_files_root() -> PathBuf {
    let current_dir = std::env::current_dir().expect("std:env::current_dir() unavailable for test");
    let test_files_path = std::env::var("TEST_FILES_PATH")
        .expect("TEST_FILES_PATH env var not set: cargo or buck should set this automatically");
    current_dir.join(test_files_path)
}

#[test]
fn test_initialize() {
    run_test_lsp(TestCase {
        test_messages: get_initialize_messages(),
        expected_responses: get_initialize_responses(),
    });
}

#[test]
fn test_go_to_def() {
    if cfg!(windows) {
        // This test fails on Windows due to Server::did_open() being unable to find a filepath.
        return;
    }

    let mut test_messages = get_initialize_messages();
    let mut expected_responses = get_initialize_responses();
    let root = get_test_files_root();

    test_messages.push(Message::from(build_did_open_notification(
        root.join("foo.py"),
    )));

    test_messages.push(Message::from(Request {
        id: RequestId::from(2),
        method: "textDocument/definition".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": format!("file://{}/foo.py", root.to_str().unwrap())
            },
            "position": {
                "line": 9,
                "character": 0
            }
        }),
    }));

    expected_responses.push(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!({
            "uri": format!("file://{}/foo.py", root.to_str().unwrap()),
            "range": {
                "start": {
                    "line": 6,
                    "character": 6
                },
                "end": {
                    "line": 6,
                    "character": 9
                }
            }
        })),
        error: None,
    });

    run_test_lsp(TestCase {
        test_messages,
        expected_responses,
    });
}
