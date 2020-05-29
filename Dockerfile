FROM alpine as build

WORKDIR /tmp/build
RUN apk add curl ghc=~8.8.3 musl-dev zlib-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /tmp/build/

RUN stack build --system-ghc --copy-bins

FROM alpine as app
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apk add --no-cache gmp libffi

COPY --from=build /root/.local/bin/mustache-docs .
EXPOSE 7777
CMD []
ENTRYPOINT ["/opt/app/mustache-docs"]