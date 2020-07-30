![Pyre](https://raw.githubusercontent.com/facebook/pyre-check/master/logo.png)

Pyre is a performant type checker for Python compliant with [PEP 484](https://www.python.org/dev/peps/pep-0484/). Pyre can analyze codebases with millions of lines of code incrementally – providing instantaneous feedback to developers as they write code.

Pyre ships with **Pysa** Python Static Analyzer, a security focused static analysis tool we've built on top of Pyre that reasons about data flows in Python applications. Please refer to our [documentation](https://pyre-check.org/docs/pysa-basics.html) to get started with our security analysis.

*Read this in other languages: [Español](README.es.md)*

## Getting Started

To install Pyre on your system run `pip install pyre-check` and you should be good to go! Run it on your project with `pyre --source-directory . check`.

For more detailed documentation, see https://pyre-check.org.

## Supported platforms

* **Python**: you need **Python 3.6 or later** to run Pyre.

* **Operating System**:
  * a recent version of Linux (we tested on **Ubuntu 16.04 LTS** and **CentOS 7**);
  * **OSX 10.11** or newer;
  * please note: Windows is not supported.

## Installation

See [INSTALL.md](INSTALL.md) for details on installing Pyre from a packaged version or from source.

## Join the Pyre community

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to help out.

## License

Pyre is licensed under the MIT license.
