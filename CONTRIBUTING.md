# Contributing

This is a learning project: a core banking system written in GnuCOBOL with a
lot of help from an AI assistant. It is not production software and has not
been reviewed by a bank, an auditor or a regulator. Issues and pull requests
that make it more correct, clearer or closer to how real banks work are very
welcome.

## Getting started

```bash
sudo apt-get install gnucobol3      # GnuCOBOL 3.x with Berkeley DB indexed files
make                                # builds bin/bankmain, bin/runpgm and one .so per module
make test                           # unit tests, then the end-to-end suite
```

The end-to-end suite (`tests/run_tests.sh`) runs in a throwaway directory and
never touches your `data/`. `KEEP=1` keeps the workspace, `VERBOSE=1` shows the
tail of the output for a failing check.

## House rules for COBOL

* **Fixed format, nothing past column 72.** CI checks every `.cbl` and `.cpy`:
  `awk 'length > 72' src/*.cbl copybooks/*.cpy`.
* **Program names are at most 8 characters** (mainframe load module names).
* **Every program starts with the standard header**: program, purpose, files,
  calls, return codes for batch, and a modification history line per change.
* **Money only moves through `ACCTPOST`.** A new module that debits or credits
  an account calls ACCTPOST with a transaction type; it never rewrites
  `ACCT-BALANCE` itself. If the default contra GL for the type is wrong, pass
  `PP-CONTRA-GL`.
* **Every GL movement is balanced.** Use `GLPOST` (one debit GL, one credit GL).
  A new GL code goes into the chart in `GLPOST` and a name in `GLCODES.cpy`.
* **Batch programs** read their run date from stdin (blank = today), print a
  summary, write a report under `data/`, and return RC 0 (fine), 4 (warnings
  for review), 8 (refused) or 12+ (error). Add them to the RUNPGM allowed list
  with `tools/addbatch.py`, and to `jobs/*.sh` and `jcl/*.jcl` if they belong
  in a job stream.
* **Online menus** stop after 20 blank inputs so piped test input cannot spin
  forever.
* **New records**: put the layout in a copybook with a `LENGTH` line, then run
  `python3 tools/datadict.py` (it cross-checks the length with
  `cobc -tsymbols`) and add the file to `tools/genjcl.py`.

## Tests

* A new feature comes with end-to-end checks in `tests/run_tests.sh` (drive the
  menu or batch with piped input, then `check "what" "regex"` or
  `check_rc "what" N`).
* A new shared subprogram comes with a unit test driver `tests/unit/T*.cbl`
  using `COPY UTWS` / `COPY UTPROC` (and `UTSEEDW` / `UTSEEDP` to seed
  accounts). Each driver runs in its own empty directory.
* Regenerate the docs before committing: `python3 tools/datadict.py`,
  `python3 tools/callgraph.py`, `python3 tools/genjcl.py`. CI fails if they
  are out of date.

## What is not tested

The JCL (`jcl/`) has never been submitted to a real z/OS, and the CICS
programs (`cics/`) are only compiled around their `EXEC CICS` statements by
`cics/check.sh`. The PostgreSQL feed (`sql/`) is tested only when OCESQL and
PostgreSQL are installed. Treat those directories as illustrations.
