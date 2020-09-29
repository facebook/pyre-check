/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import React from 'react';
import {Alert, Spin} from 'antd';
import {useQuery, gql} from '@apollo/client';
import {Controlled as CodeMirror} from 'react-codemirror2';

import './Source.css';
require('codemirror/lib/codemirror.css');
require('codemirror/mode/python/python.js');

function Source(
  props: $ReadOnly<{|path: string, location: string|}>,
): React$Node {
  const SourceQuery = gql`
    query Issue($path: String) {
      file(path: $path) {
        edges {
          node {
            contents
          }
        }
      }
    }
  `;
  const {loading, error, data} = useQuery(SourceQuery, {
    variables: {path: props.path},
  });

  var content = <div />;
  if (error) {
    content = (
      <Alert
        message={`Unable to load ${props.path} (${error.toString()})`}
        type="error"
      />
    );
  } else if (loading) {
    content = (
      <div style={{height: '12em', textAlign: 'center', paddingTop: '5em'}}>
        <Spin tip={`Loading ${props.path}...`} />
      </div>
    );
  } else {
    const value = data.file.edges[0].node.contents;

    var range = null;
    var selection = null;
    const split_location = props.location.split('|').map(i => parseInt(i));
    if (split_location.length === 3) {
      range = {
        from: {line: split_location[0] - 1, ch: split_location[1]},
        to: {line: split_location[0] - 1, ch: split_location[2]},
      };
      selection = range.from;
    }

    content = (
      <CodeMirror
        value={value}
        options={{lineNumbers: true, readOnly: 'nocursor'}}
        editorDidMount={editor => {
          if (range === null) {
            return;
          }
          editor.markText(range.from, range.to, {
            className: 'traceSelection',
          });
        }}
        selection={{
          ranges: [{anchor: selection, head: selection}],
          focus: true,
        }}
      />
    );
  }

  return <div style={{border: '1px solid lightgray'}}>{content}</div>;
}

export default Source;
