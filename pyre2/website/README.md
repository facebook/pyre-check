# Website

The website is a combination of static content (built using
[Docusaurus 2](https://docusaurus.io/)) and a sandbox built with Rust/WASM.

## Development

### On a Devserver

Install the prerequisites:

```bash
sudo dnf install clang
export HTTPS_PROXY=fwdproxy:8080
rustup install nightly && rustup default nightly
cargo install wasm-pack wasm-opt
yarn install
```

Compile it:

```bash
./scripts/start.sh
```

Go to the following URI in your browser:

```
localhost:3000
```

Most changes are reflected live without having to restart the server.

### On a Local Mac

Install dependencies:

```bash
rustup install nightly && rustup default nightly
cargo install wasm-pack wasm-opt
yarn install
```

Run it:

```bash
./scripts/start.sh
```

Go to the following URI in your browser:

```
localhost:3000
```

Most changes are reflected live without having to restart the server.

### Formatter

Please use "Typescript and Javascript Language Features" as your default
formatter to avoid formatting issues.
