mkdir -p src/packages/parser/_build/parser
STACK_EXEC=""
if ! [ -x "$(command -v git)" ]; then
  STACK_EXEC="$HOME/Development/bin/stack/stack"
else
  STACK_EXEC="$(which stack)"
fi

ln -f -s "$STACK_EXEC" src/packages/parser/_build/parser/stack

ln -f -s "$STACK_EXEC" src/packages/parser/stack
ln -f -s "$STACK_EXEC" src/packages/core/stack
ln -f -s "$STACK_EXEC" src/packages/cli-jvm/stack
ln -f -s "$STACK_EXEC" src/packages/cli-llvm/stack
ln -f -s "$STACK_EXEC" src/packages/stack
ln -f -s "$STACK_EXEC" src/stack
ln -f -s "$STACK_EXEC" stack

