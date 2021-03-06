FROM alpine:3.13 as build

WORKDIR /tmp/build
RUN apk add curl ghc musl-dev zlib-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /tmp/build/

RUN stack build --system-ghc --copy-bins

FROM alpine:3.13 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apk add --no-cache gmp libffi

COPY --from=build /root/.local/bin/must-render .
COPY --from=build /tmp/build/conf ./conf
EXPOSE 7777
CMD []
ENTRYPOINT ["/opt/app/must-render"]
