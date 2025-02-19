FROM ocaml/opam:debian-ocaml-4.12 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin master && git reset --hard 01c350d759f8d4e3202596371818e6d997fa5fe2 && opam update
COPY --chown=opam \
	vendor/ocurrent/current_ansi.opam \
	vendor/ocurrent/current_docker.opam \
	vendor/ocurrent/current_github.opam \
	vendor/ocurrent/current_git.opam \
	vendor/ocurrent/current_incr.opam \
	vendor/ocurrent/current.opam \
	vendor/ocurrent/current_rpc.opam \
	vendor/ocurrent/current_slack.opam \
	vendor/ocurrent/current_web.opam \
	/src/vendor/ocurrent/
WORKDIR /src
RUN opam pin add -yn current_ansi.dev "./vendor/ocurrent" && \
    opam pin add -yn current_docker.dev "./vendor/ocurrent" && \
    opam pin add -yn current_github.dev "./vendor/ocurrent" && \
    opam pin add -yn current_git.dev "./vendor/ocurrent" && \
    opam pin add -yn current_incr.dev "./vendor/ocurrent" && \
    opam pin add -yn current.dev "./vendor/ocurrent" && \
    opam pin add -yn current_rpc.dev "./vendor/ocurrent" && \
    opam pin add -yn current_slack.dev "./vendor/ocurrent" && \
    opam pin add -yn current_web.dev "./vendor/ocurrent"
COPY --chown=opam docs-ci.opam /src/
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN --mount=type=cache,target=./_build/,uid=1000,gid=1000 opam config exec -- dune build ./_build/install/default/bin/docs-ci ./_build/install/default/bin/docs-ci-solver && cp ./_build/install/default/bin/docs-ci ./_build/install/default/bin/docs-ci-solver .
FROM debian:10
RUN apt-get update && apt-get install rsync libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase gzip bzip2 xz-utils unzip tar -y --no-install-recommends
RUN git config --global user.name "docs" && git config --global user.email "ci"
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/docs-ci"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/docs-ci /src/docs-ci-solver /usr/local/bin/
