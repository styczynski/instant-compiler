STACK := $(shell echo "$$HOME/Development/bin/stack/stack")

all: stack
	$(STACK) install --only-dependencies
	$(STACK) install shake
	$(STACK) exec -- shake

stack:
	bash ./scripts/setup.sh
	./scripts/links.sh
