FROM fpco/stack-build:lts-14.20 as build
RUN mkdir /opt/build
RUN mkdir /opt/app
COPY . /opt/build
RUN cd /opt/build && stack build --local-bin-path /opt/app --system-ghc --copy-bins

FROM amd64/debian:buster-slim
RUN mkdir /opt/app
WORKDIR /opt/app
COPY --from=build /opt/app  .
ENTRYPOINT ["/opt/app/DNSBL-milter-exe"]
