# cli-llvm

This package provides console interface for Haskell Instant compiler
providing LLVM backend.

## Usage

```bash
    $ stack exec instant-cli-llvm -- -f code.ins
```

The command will produce `code.ll` and `code.bc`
