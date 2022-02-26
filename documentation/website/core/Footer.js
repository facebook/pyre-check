/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

class Footer extends React.Component {
  docUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    return baseUrl + 'docs/' + (language ? language + '/' : '') + doc;
  }

  pageUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    return baseUrl + (language ? language + '/' : '') + doc;
  }

  render() {
    return (
      <footer className="nav-footer" id="footer">
        <section className="sitemap">
          <a
            href="https://code.facebook.com/projects/"
            target="_blank"
            className="fbOpenSource">
            <img
              src={this.props.config.baseUrl + 'img/oss_logo.png'}
              alt="Facebook Open Source"
              width="170"
              height="45"
            />
          </a>
          <a href="https://code.facebook.com/projects/" target="_blank">
            Open Source Projects
          </a>
          <a href="https://github.com/facebook/" target="_blank">
            Github
          </a>
          <a href="https://twitter.com/fbOpenSource" target="_blank">
            Twitter
          </a>
          <a href={this.props.config.repoUrl} target="_blank">
            Contribute to Pyre
          </a>
        </section>
      </footer>
    );
  }
}

module.exports = Footer;
