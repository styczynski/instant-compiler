STACK := $(shell echo "$$HOME/Development/bin/stack/stack")

all:
	$(STACK) install --only-dependencies
	$(STACK) install shake
	$(STACK) exec -- shake
