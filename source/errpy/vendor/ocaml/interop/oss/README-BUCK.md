# Building ocamlrep with Buck2

## Setup

These are things that need to be done once to get going.

### Install Buck2

Buck2 provides prebuilt binaries. For example the following commands will install a prebuilt Buck2 and symlink it into `/usr/local/bin`.
```bash
  wget https://github.com/facebook/buck2/releases/download/latest/buck2-"$PLAT".zst
  zstd -d buck2-"$PLAT".zst -o buck2
  chmod +x buck2
  sudo ln -s "$(pwd)"/buck2 /usr/local/bin/buck2
```
Valid values for `$PLAT` are `x86_64-unknown-linux-gnu` on Linux, `x86_64-apple-darwin` on x86 macOS and `aarch64-apple-darwin` on ARM macOS.

It's also possible to install Buck2 from source into `~/.cargo/bin` like this.
```bash
  cargo +nightly-2023-12-11 install --git https://github.com/facebook/buck2.git buck2
```
*Note: If on aarch64-apple-darwin then be sure install to `brew install protobuf` and for the now it's necessary to add
```bash
  export BUCK2_BUILD_PROTOC=/opt/homebrew/opt/protobuf/bin/protoc
  export BUCK2_BUILD_PROTOC_INCLUDE=/opt/homebrew/opt/protobuf/include
```
to the build environment.*

### Install Reindeer

Install the `reindeer` binary from source into '~/.cargo/bin' like this.
```bash
    cargo +nightly-2023-12-11 install --git https://github.com/facebookincubator/reindeer.git reindeer
```

*Note: Make sure after installing Buck2 and Reindeer to configure your `PATH` environment variable if necessary so they can be found.*

### Install the OCaml package Manager

If you haven't already, install [opam](https://opam.ocaml.org/).

When opam has been installed execute `~/.ocaml-setup.sh` from the root of the distribution. The effect of `ocaml-setup.sh` is to create symlinks in `shim/third/party/ocaml` that point into the local opam installation.

*Note: The script assumes that [`OPAM_SWITCH_PREFIX`](https://opam.ocaml.org/doc/Manual.html#Switches) is set.*

## Vendor sources & generate buck rules for ocamlrep's Rust dependencies

[Reindeer](https://github.com/facebookincubator/reindeer) is a a tool that imports Rust crates from crates.io and generates Buck2 build rules for them. Run it from the root of the ocamlrep repository like this.
```bash
    reindeer --third-party-dir shim/third-party/rust buckify
```

That's it, you're all set.

## Profit!

Run this command from the root of the repository to build all the targets you can.
```
    buck2 build root//...
```

More examples and more detail about building with Buck2 are available on the [Buck2 website](https://buck2.build/)!
