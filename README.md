# REMAP DEMAIL File Classifier

`remap-demail-classifer` is a command line application written in
Python3. It classifies fetched DEMAIL attachment files using a regex
specification.

There are no specific requirements for the application to run other
than `>= Python3.6`.

CLI arguments are as follows:

```
./remap-demail-classifer <SPEC-FILE> <PREFIX-REGEX> <IGNORE-FILE> <DEMAIL-DIR>
```

Example:

```
./remap-demail-classifer spec.csv \
	'^(?P<recdate>[0-9]{4}\-[0-9]{2}\-[0-9]{2})T[0-9]{2}:[0-9]{2}:[0-9]{2}Z_[A-Z0-9]{32}_[A-Z0-9]{32}_' \
	tmp/ignore.dat \
	/data/remap/tenants/deployment/demail/downloaded
```
