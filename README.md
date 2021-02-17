# reforg - Organize Files Based on Regular Expressions

`reforg` is a command line application written in Python(3). It reorganizes
files under given directories based on a set of regex-powered rules.

There are no specific requirements for the application to run other
than `>= Python3.6`.

CLI arguments are as follows:

```
$ ./reforg --help
usage: reforg [-h] --spec SPEC [--ignore IGNORE] [--prefix PREFIX] --root ROOT
              [--dry-run] [--metadata] [--force]
              DIRS [DIRS ...]

positional arguments:
  DIRS

optional arguments:
  -h, --help       show this help message and exit
  --spec SPEC      Specification file
  --ignore IGNORE  Blacklist file (names of files to ignore)
  --prefix PREFIX  Regular expression prefix for all patterns in the
                   specification file
  --root ROOT      Root directory to copy renamed files to
  --dry-run        Dry run
  --metadata       Preserve metadata
  --force          Force copy even if the target exists
```
