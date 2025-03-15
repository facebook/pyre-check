/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::thread;

use crossbeam_channel::bounded;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use pretty_assertions::assert_eq;

use crate::commands::lsp::run_lsp;
use crate::commands::lsp::Args;

struct TestCase {
    test_messages: Vec<Message>,
    expected_responses: Vec<Response>,
}
fn run_test_lsp(test_case: TestCase) {
    let args = Args {
        include: Vec::new(),
    };
    let (writer_sender, writer_receiver) = bounded::<Message>(0);
    let (reader_sender, reader_receiver) = bounded::<Message>(0);

    // spawn thread to handle writes from language server to client
    let writer: thread::JoinHandle<Result<(), std::io::Error>> = thread::spawn(move || {
        let mut responses = test_case.expected_responses.clone();
        for msg in writer_receiver {
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
                Message::Notification(_) | Message::Request(_) => {
                    panic!("Unexpected message {:?}", msg);
                }
            };
            if responses.is_empty() {
                break;
            }
        }
        Ok(())
    });

    // spawn thread to handle reads of messages from client to language server
    let reader: thread::JoinHandle<Result<(), std::io::Error>> = thread::spawn(move || {
        test_case.test_messages.iter().for_each(|msg| {
            reader_sender.send(msg.clone()).unwrap();
        });
        Ok(())
    });

    let connection = Connection {
        sender: writer_sender,
        receiver: reader_receiver,
    };

    match run_lsp(
        &connection,
        || {
            match reader.join() {
                Ok(r) => r?,
                Err(err) => return Err(anyhow::format_err!("reader panicked: {:?}", err)),
            }
            match writer.join() {
                Ok(r) => r?,
                Err(err) => {
                    return Err(anyhow::format_err!("writer panicked: {:?}", err));
                }
            }
            Ok(())
        },
        args,
    ) {
        Ok(_) => {}
        Err(err) => panic!("run_lsp failed: {:?}", err),
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

#[test]
fn test_initialize() {
    run_test_lsp(TestCase {
        test_messages: get_initialize_messages(),
        expected_responses: get_initialize_responses(),
    });
}
