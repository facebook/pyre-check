/**
 * (c) Facebook, Inc. and its affiliates. Confidential and proprietary.
 */

import React from 'react';
import './index.css';

function Issue(props) {
  const issueId = props.match.params.issueId;
  return (
    <div>
      SAPP link to issue {issueId}
    </div>);
}

export default Issue;
