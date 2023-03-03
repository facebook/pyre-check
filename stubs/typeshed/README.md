# Typeshed

Pyre vendors a copy of the stubs from the Python `typeshed` project,
in order to
- pin the typeshed version, which we do because changes to standard library
  stubs don't always work well immediately with Python.
- apply patches to typeshed code that doesn't work out-of-the box with Pyre
- apply patches to extend the standard stubs (for now this is mainly
  so that we can modify `builtins.pyi` to support experimental tensor shape
  type arithmetic).

The typeshed source code can be found at
http://github.com/python/typeshed/, and our vendored copy comes from
pulling zipball of a specific commit from the github API at
`https://api.github.com/repos/python/typeshed/zipball/<sha>`, and applying
patches found in `../scripts/typeshed-patches/`.

We have a copy of the license for typeshed at `./LICENCE`

The code used to pull a zipball and apply patches can be found at
`../../scripts/download_typeshed.py`.
