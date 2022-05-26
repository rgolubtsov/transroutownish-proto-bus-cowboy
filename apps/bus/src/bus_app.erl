%
% apps/bus/src/bus_app.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.1
% =============================================================================
% A daemon written in Erlang/OTP, designed and intended to be run
% as a microservice, implementing a simple urban bus routing prototype.
% =============================================================================
% Copyright (C) 2022 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%%%-------------------------------------------------------------------
%% @doc bus public API
%% @end
%%%-------------------------------------------------------------------

-module(bus_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    bus_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

% vim:set nu et ts=4 sw=4:
