# Contributing

## Installation requirements

There are no installation requirements. `bloomfilter-blocked` builds right out
of the box on Linux, MacOS, and Windows.

## Building

The project is built using `ghc` and `cabal`.

```
cabal update
cabal build all
```

## Testing

Tests are run using `cabal`.

```
cabal build all
cabal test all
```

And using `cabal-docspec`.

```
./scripts/test-cabal-docspec.sh
```

## Code style

There is no strict code style, but try to keep the code style consistent
throughout the repository and favour readability. Code should be well-documented
and well-tested.

## Formatting

We use `stylish-haskell` to format Haskell files, and we use `cabal-fmt` to
format `*.cabal` files. We also use `cabal check` to sanity check our cabal
files. See the helpful scripts in the [scripts folder](./scripts/), and the
[`stylish-haskell` configuration file](./.stylish-haskell.yaml).

To perform a pre-commit code formatting pass, run the following:

```
./format-stylish-haskell.sh
./format-cabal-fmt.sh
./lint-cabal.sh
```

## Documentation

Documentation can be generated locally using a custom script that invokes `cabal haddock-project`.

```
./generate-haddock.sh
```

Documentation is also automatically generated for the latest version of the
`main` branch and hosted on GitHub pages.

## Pull requests

The following are requirements for merging a PR into `main`:
* Each commit should be small and should preferably address one thing. Commit
  messages should be useful.
* Document and test your changes.
* The PR should have a useful description, and it should link issues that it
  resolves (if any).
* Changes introduced by the PR should be recorded in the relevant changelog
  files.
* PRs should not bundle many unrelated changes.
* PRs should be approved by at least 1 developer with write access to the
  repository.
* The PR should pass all CI checks.

## Releases

Releases follow the [Haskell Package Versioning
Policy](https://pvp.haskell.org/). We use version numbers consisting of 4 parts,
like `A.B.C.D`.
* `A.B` is the *major* version number. A bump indicates a breaking change.
* `C` is the *minor* version number. A bump indicates a non-breaking change.
* `D` is the *patch* version number. A bump indicates a small, non-breaking
  patch.
