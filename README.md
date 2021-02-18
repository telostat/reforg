# reforg - Organize Files Based on Regular Expressions

![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/reforg)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/reforg)
![GitHub](https://img.shields.io/github/license/telostat/reforg)

> **Note:** This application is under development and of prototype quality at
> the moment. Expect significant breaking changes without notification until we
> reach the first major version.

`reforg` is a command line application written in Python(3). It reorganizes
files under given directories based on a set of regex-powered rules.

There are no specific requirements for the application to run other
than `>= Python3.6`.

## Installation

```
curl -o - https://raw.githubusercontent.com/telostat/reforg/main/install.sh | sudo sh -x
```

## Usage

CLI arguments are as follows:

```
$ reforg --help
usage: reforg [-h] --spec SPEC-FILE --root ROOT-DIR [--dry-run] [--metadata]
              [--force]
              DIR [DIR ...]

Organize files based on regular expressions

positional arguments:
  DIR               Directory to list files of to process. Note that special
                    entries in the directory and sub-directories are not
                    listed and therefore not processed.

optional arguments:
  -h, --help        show this help message and exit
  --spec SPEC-FILE  Specification file
  --root ROOT-DIR   Root directory to copy processed files to
  --dry-run         Dry run (do not copy anything)
  --metadata        Preserve metadata while copying
  --force           Force copy if target exists (overwrite existing files)

reforg -- v0.0.1.dev0
```

Example:

```
reforg --spec example/spec.json --root example/target/ --metadata --force --dry-run example/source/
```

## Specification Format

See [./example/spec.json](./example/spec.json) for an example.

Note that we are using JSON as specification file format. A much better file
format would be YAML (or maybe even TOML). However, we want to stick to the idea
of external *no-dependencies* for easier deployment. We may wish to change that
in the future.

For convenience, you may wish to write the specification in YAML (as in
[./example/spec.yaml](./example/spec.yaml)) and then convert to JSON:

```
cd example/
yq . < spec.yaml > spec.json
```

... or pipe converted JSON directly to the command (note the `--spec -`
argument):

```
yq . < example/spec.yaml | reforg --spec - --root example/target/ --metadata --force --dry-run example/source/
```

## License

Copyright Telostat Pte Ltd (c) 2021.

This work is licensed under BSD3. See [LICENSE](./LICENSE).
