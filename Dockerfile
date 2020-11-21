FROM fpco/stack-build:latest
RUN apt-get update
COPY ./stack.yaml /opt/waziup/
COPY ./stack.yaml.lock /opt/waziup/
COPY ./package.yaml /opt/waziup/

COPY ./src/packages/cli-jvm/package.yaml /opt/waziup/src/packages/cli-jvm/
COPY ./src/packages/cli-jvm/stack.yaml /opt/waziup/src/packages/cli-jvm/
COPY ./src/packages/cli-jvm/stack.yaml.lock /opt/waziup/src/packages/cli-jvm/

COPY ./src/packages/cli-llvm/package.yaml /opt/waziup/src/packages/cli-llvm/
COPY ./src/packages/cli-llvm/stack.yaml /opt/waziup/src/packages/cli-llvm/
COPY ./src/packages/cli-llvm/stack.yaml.lock /opt/waziup/src/packages/cli-llvm/

COPY ./src/packages/core/package.yaml /opt/waziup/src/packages/core/
COPY ./src/packages/core/stack.yaml /opt/waziup/src/packages/core/
#COPY ./src/packages/core/stack.yaml.lock /opt/waziup/src/packages/core/

COPY ./src/packages/parser/package.yaml /opt/waziup/src/packages/parser/
COPY ./src/packages/parser/stack.yaml /opt/waziup/src/packages/parser/
COPY ./src/packages/parser/stack.yaml.lock /opt/waziup/src/packages/parser/

COPY ./src/packages/test-preprocessor/package.yaml /opt/waziup/src/packages/test-preprocessor/
COPY ./src/packages/test-preprocessor/stack.yaml /opt/waziup/src/packages/test-preprocessor/
#COPY ./src/packages/test-preprocessor/stack.yaml.lock /opt/waziup/src/packages/test-preprocessor/

WORKDIR /opt/waziup
RUN stack setup
RUN stack install --only-dependencies
RUN stack install shake

COPY . /opt/waziup
RUN bash ./scripts/links.sh
RUN stack exec -- shake
