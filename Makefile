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

build-docker:
	docker build -t styczynski/insc-docker .
	cp ./scripts/run_docker_insc_jvm.sh ./insc_jvm
	cp ./scripts/run_docker_insc_llvm.sh ./insc_llvm
