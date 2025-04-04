/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;
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
use lsp_types::Url;
use pretty_assertions::assert_eq;

use crate::commands::lsp::run_lsp;
use crate::commands::lsp::Args;
use crate::test::util::init_test;

struct TestCase {
    test_messages: Vec<Message>,
    expected_responses: Vec<Response>,
}
fn run_test_lsp(test_case: TestCase) {
    init_test();
    let timeout = Duration::from_secs(25);
    let args = Args {
        search_path: vec![get_test_files_root()],
        site_package_path: Vec::new(),
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

fn get_initialize_params(workspace_folders: Option<Vec<(&str, Url)>>) -> serde_json::Value {
    let mut params = serde_json::json!({
        "rootPath": "/",
        "processId": std::process::id(),
        "trace": "verbose",
        "clientInfo": { "name": "debug" },
        "capabilities": {
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
    });

    if let Some(folders) = workspace_folders {
        params["capabilities"]["workspace"]["workspaceFolders"] = serde_json::json!(true);
        params["workspaceFolders"] = serde_json::json!(
            folders
                .iter()
                .map(|(name, path)| serde_json::json!({"name": name, "uri": path.to_string()}))
                .collect::<Vec<_>>()
        );
    }

    params
}

fn get_initialize_messages(
    workspace_folders: Option<Vec<(&str, Url)>>,
) -> std::vec::Vec<lsp_server::Message> {
    vec![
        Message::from(Request {
            id: RequestId::from(1),
            method: "initialize".to_owned(),
            params: get_initialize_params(workspace_folders),
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
                "uri": Url::from_file_path(&path).unwrap().to_string(),
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
        test_messages: get_initialize_messages(None),
        expected_responses: get_initialize_responses(),
    });
}

fn test_go_to_def(workspace_folders: Option<Vec<(&str, Url)>>) {
    let mut test_messages = get_initialize_messages(workspace_folders);
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
                "uri": Url::from_file_path(root.join("foo.py")).unwrap().to_string()
            },
            "position": {
                "line": 5,
                "character": 16
            }
        }),
    }));

    expected_responses.push(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!({
            "uri": Url::from_file_path(root.join("bar.py")).unwrap().to_string(),
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

#[test]
fn test_go_to_def_single_root() {
    test_go_to_def(Some(vec![(
        "test",
        Url::from_file_path(get_test_files_root()).unwrap(),
    )]));
}

#[test]
fn test_go_to_def_no_root() {
    test_go_to_def(Some(vec![]));
}

#[test]
fn test_go_to_def_no_folder_capability() {
    test_go_to_def(None);
}
