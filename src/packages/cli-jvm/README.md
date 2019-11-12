# cli-jvm

This package provides console interface for Haskell Instant compiler
providing JVM backend.

## Usage

```bash
    $ stack exec instant-cli-jvm -- -f code.ins
```

The command will produce `code.j`, `code.class` and runnable jar `code.jar`
