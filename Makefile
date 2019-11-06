STACK := $(shell echo "$$HOME/Development/bin/stack/stack")

stack:
	./setup.sh
	./links.sh

all: stack
	$(STACK) install --only-dependencies
	$(STACK) install shake
	$(STACK) exec -- shake
