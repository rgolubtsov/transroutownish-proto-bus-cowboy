#
# Dockerfile
# =============================================================================
# Urban bus routing microservice prototype (Erlang/OTP port). Version 0.3.5
# =============================================================================
# An Erlang/OTP application, designed and intended to be run as a microservice,
# implementing a simple urban bus routing prototype.
# =============================================================================
# Copyright (C) 2022-2025 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

# === Stage 1: Build the microservice =========================================
FROM       erlang:alpine AS build
# Installing packages `gcc` and `musl-dev` to build `syslog` as a dependency.
RUN        ["apk", "add", "gcc"     ]
RUN        ["apk", "add", "musl-dev"]
WORKDIR    var/tmp
RUN        ["mkdir", "-p", "bus/apps", "bus/config", "bus/data"]
COPY       apps         bus/apps/
COPY       config       bus/config/
COPY       data         bus/data/
COPY       rebar.config bus/
WORKDIR    bus
# Setting the HOME env var forcing Rebar3 to create ".cache/rebar3/" locally.
ENV        HOME=.
RUN        ["rebar3", "as", "prod", "release"]

# === Stage 2: Run the microservice ===========================================
FROM       alpine:3.18
# Installing packages `ncurses-libs` and `libstdc++` as runtime dependencies
# for Erlang BEAM; `libcrypto1.1` -- for Erlang Crypto app.
RUN        ["apk", "add", "ncurses-libs"]
RUN        ["apk", "add", "libstdc++"   ]
RUN        ["apk", "add", "libcrypto1.1"]
WORKDIR    var/tmp
COPY       --from=build var/tmp/bus/_build/prod/rel ./
ENTRYPOINT ["bus/bin/bus", "foreground"]

# vim:set nu ts=4 sw=4:
