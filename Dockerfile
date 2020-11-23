FROM fpco/stack-build:latest AS builder

RUN apt-get update


RUN sudo apt install -y default-jre
RUN java -version

# Install dependencies
RUN apt-get -qq update; \
    apt-get install -qqy --no-install-recommends \
        ca-certificates \
        autoconf automake cmake dpkg-dev file git make patch \
        libc-dev libc++-dev libgcc-8-dev libstdc++-8-dev  \
        dirmngr gnupg2 lbzip2 wget xz-utils libtinfo5; \
    rm -rf /var/lib/apt/lists/*

# Signing keys
ENV GPG_KEYS 09C4E7007CB2EFFB A2C794A986419D8A B4468DF4E95C63DC D23DD2C20DD88BA2 8F0871F202119294 0FC3042E345AD05D

# Retrieve keys
RUN gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys $GPG_KEYS

# Version info
ENV LLVM_RELEASE 11
ENV LLVM_VERSION 11.0.0

# Install Clang and LLVM
COPY ./scripts/install_llvm.sh .
RUN ./install_llvm.sh

RUN llvm-as --version

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

ADD . /opt/waziup
RUN bash ./scripts/links.sh
RUN ls -a src/packages/cli-llvm/
RUN ls -a .
RUN stack exec -- shake
RUN ls

#FROM alpine:latest
#WORKDIR /root/
#COPY --from=builder /opt/waziup/insc_jvm .
#COPY --from=builder /opt/waziup/insc_llvm .
#COPY --from=builder /opt/waziup/lib .

COPY ./scripts/entrypoint.sh .
ENTRYPOINT ["./entrypoint.sh"]
