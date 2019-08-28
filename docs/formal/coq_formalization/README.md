# Work in progress: formalization of a calculus with Pyre features

In this work I am trying to extend the Simply Typed Lambda Calculus with
features present in Pyre to try to model and prove properties of its current
implementation. At the moment you will find in here:
- a formalization of STLC using call by value evaluation
- with simple references to memory cells

Most of this work is inspired by my own formalization [1] of 
"Strong Normalisation in λ-calculi with References" [2]
by Romain Demangeon, Daniel Hirschkoff, and Davide Sangiorgi

In this formalization we only focus on Subject Reduction and Progress.
The main modification to make the proof tractable in this version is the
we remove the hidden dependency from the reduction to the typing judgment,
so we are limited to prove Subject Reduction in the empty context only.

# How to read the Coq files
The Coq files should be red in the following order:
- `ty.v`: definition of the simple types used.
- `term.v`: definition of the untyped terms (and DeBrjuin magic to deal with binders correctly).
- `env.v`: definition of the typing environment for terms.
- `red.v`: definition of the reduction (beta + ref)
- `sr.v`: proof of Subject Reduction and Progress.

# Install
```
# install a recent coq, like 8.8 using opam (see [3])

make
make doc
```

[1] https://github.com/vsiles/STLC_Ref
[2] https://www-apr.lip6.fr/~demangeon/Recherche/impfun2.pdf
[3] https://coq.inria.fr/opam-using.html
