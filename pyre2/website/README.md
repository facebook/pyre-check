# Website

This website is built using [Docusaurus 2](https://docusaurus.io/), a modern
static website generator.

## Local Development

IMPORTANT: If you want to test any sandbox functionalities, you should be
developing on your local machine. This is because the sandbox relies on cargo,
which can only be built locally right now. For Meta engineers who are not
working on the sandbox, you can build on your dev machine using the following
instructions.

Install dependencies:

```bash
$ yarn install # yarn dependencies
$ cargo install wasm-pack # wasm-pack for wasm build
$ rustup install nightly && rustup default nightly # the wasm build must be done on the rust nightly build
```

Start a local development server:

```bash
$ ./scripts/start.sh
```

Go to the following uri in your browser:

```
localhost:3000
```

Most changes are reflected live without having to restart the server.
