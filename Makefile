#
# Makefile
# =============================================================================
# Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.1
# =============================================================================
# A daemon written in Erlang/OTP, designed and intended to be run
# as a microservice, implementing a simple urban bus routing prototype.
# =============================================================================
# Copyright (C) 2021-2022 Radislav (Radicchio) Golubtsov
#
# (See the LICENSE file at the top of the source tree.)
#

# Profile: "default" or "prod".
PROF = prod

EBIN = _build/$(PROF)/lib/bus/ebin
BEAM = $(EBIN)/bus_app.beam \
       $(EBIN)/bus_sup.beam

APPS = apps/bus/src
SRCS = $(APPS)/bus_app.erl \
       $(APPS)/bus_sup.erl

SERV = _build/$(PROF)/rel/bus/lib

# Specify flags and other vars here.
REBAR3 = rebar3
ECHO   = @echo

# Making the first target (BEAMs).
$(BEAM): $(SRCS)
	$(REBAR3)         compile
	$(REBAR3) as prod compile

# Making the second target (releases).
$(SERV): $(BEAM)
	$(REBAR3)         release
	$(REBAR3) as prod release
	$(ECHO)

.PHONY: all clean

all: $(SERV)

clean:
	$(REBAR3)         clean
	$(REBAR3) as prod clean

# vim:set nu ts=4 sw=4:
