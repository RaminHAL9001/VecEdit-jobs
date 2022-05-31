# VecEdit-jobs

This package provides a simple job queue/process manager built on top
of the `VecEdit` and `VecEdit-text` package. `VecEdit` is a small
library that provides some **unabashedly stateful** monadic function
types for editing mutable vectors. The main use case is rapid
prototyping of mutable vector algorithms in a GHCI session.

When launching a process, you can choose if you want to redirect the
child's `STDOUT` or `STDERR`, or else capture them into a text buffer
provided by the `VecEdit-text` package. You can also redirect a text
buffer to the child process' `STDIN`.

## Brief overveiew of useful APIs

### `VecEdit.Jobs`

This is currently the only module.

- `Manager` is a job manager

- `Buffer` is a text buffer for buffering IO between child processes

  - `newBuffer` creates a new `Buffer` in the `Manager` table of
    `Buffer`s.

- `Worker` is a thread used to run the buffering functions

- `Process` is a child process

  - `ProcessConfig` specifies how to launch a child. It wraps a
    `System.Process.CreateProcess` data structure but also includes
    threads for buffering characters flowing over the
    `System.IO.Handle`s to/from the child process.

  - `runInBuffer` launches a child `Process` in the `Manager` table of
    `Process`es, capturing output to a `Buffer`.
