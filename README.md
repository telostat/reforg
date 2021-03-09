# reforg - Declaratively Organize and Process Files

![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/reforg)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/reforg)
![GitHub](https://img.shields.io/github/license/telostat/reforg)

> **Note:** This application is under development and of prototype quality at
> the moment. Expect significant breaking changes without notification until we
> reach the first major version.

`reforg` is a command line application written in Haskell. It reorganizes and
process files under given directories based on a set of regex-powered,
parameterized and templated declarative rules.

The application is tested on GNU/Linux and there are no specific requirements if
users choose to install the statically linked executable.

## Installation

Download the executable from the [latest
release](https://github.com/telostat/reforg/releases) and put it under your
`PATH`.

## Usage

CLI arguments are as follows:

```
$ reforg --help
reforg - Organize and Process Files in Bulk

Usage: reforg [--version] COMMAND
  Reforg

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  process                  Process files as per given specification file
```

... in particular:

```
$ reforg process --help
Usage: reforg process (-s|--spec SPEC) [-n|--dry-run] [-p|--param KEY=VALUE]
                      [-e|--envar KEY=VALUE] DIR [DIR...]
  Process files as per given specification file

Available options:
  -s,--spec SPEC           Path to reforg specification file (YAML or JSON)
  -n,--dry-run             Dry run
  -p,--param KEY=VALUE     Key/Value parameters
  -e,--envar KEY=VALUE     Environment variables
  DIR [DIR...]             Paths to directories containing files to process
  -h,--help                Show this help text
```

Example:

```
$ reforg process -s example/spec.yaml  -p target_directory_root=example/target example/source
```

## Specification Format

See [./example/spec.yaml](./example/spec.yaml) for an example. Note that we are
using YAML as specification file format.


## License

Copyright Telostat Pte Ltd (c) 2021.

This work is licensed under BSD3. See [LICENSE](./LICENSE).
