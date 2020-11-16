STACK := $(shell echo "$$HOME/Development/bin/stack/stack")

all: stack
	$(STACK) install --only-dependencies
	$(STACK) install shake
	$(STACK) exec -- shake

build:
	$(STACK) install --only-dependencies
	$(STACK) install shake
	$(STACK) exec -- shake

stack:
	bash ./scripts/setup.sh
	bash ./scripts/links.sh
