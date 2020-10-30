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
import {Alert, Button, Spin, Tooltip} from 'antd';
import {SelectOutlined} from '@ant-design/icons';
import {useQuery, gql} from '@apollo/client';
import {Controlled as CodeMirror} from 'react-codemirror2';

import './Source.css';
require('codemirror/lib/codemirror.css');
require('codemirror/mode/python/python.js');

type Location = $ReadOnly<{
  line: number,
  ch: number,
}>;

type Range = $ReadOnly<{
  from: Location,
  to: Location,
}>;

function parseRanges(input: ?string): Array<Range> {
  if (input === undefined || input === null || input === '') {
    return [];
  }

  return input.split(';').map(input => {
    const numbers = input.split('|').map(i => parseInt(i));
    if (numbers.length !== 3) {
      throw new Error(`Invalid Location: ${input}`);
    }
    const line = numbers[0] - 1;
    var begin = numbers[1];
    var end = numbers[2];
    if (end < begin) {
      // TODO(T78595608): remove temporary workaround for Pysa inverting locations.
      [begin, end] = [end, begin];
    }

    return {
      from: {line, ch: begin - 1},
      to: {line, ch: end},
    };
  });
}

function Source(
  props: $ReadOnly<{|path: string, location: string, titos?: string|}>,
): React$Node {
  const range = parseRanges(props.location)[0];
  const line = range.from.line;

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
    const value =
      data.file.edges[0].node.contents + '\n# ' + (props.titos || 'no titos');

    // React codemirror is horribly broken so store a reference to underlying
    // JS implementation.
    var editor = null;

    content = (
      <CodeMirror
        value={value}
        options={{lineNumbers: true, readOnly: 'nocursor'}}
        editorDidMount={nativeEditor => {
          editor = nativeEditor;

          editor.markText(range.from, range.to, {
            className: 'Source-selection',
          });

          parseRanges(props.titos).forEach(range => {
            nativeEditor.markText(range.from, range.to, {
              className: 'Source-tito',
            });
          });

          const offset = editor.heightAtLine(line > 4 ? line - 3 : 1, 'local');
          editor.scrollTo(0, offset);
        }}
      />
    );
  }

  return (
    <>
      <div class="source-menu">
        <Tooltip title="Reset Scroll" placement="bottom">
          <Button
            size="small"
            icon={<SelectOutlined />}
            type="text"
            onClick={() => editor && editor.scrollIntoView({line, ch: 0})}
            disabled={loading || error}
          />
        </Tooltip>
      </div>
      <div class="source">{content}</div>
    </>
  );
}

export default Source;
