%
% apps/bus/src/bus_app.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.1
% =============================================================================
% An Erlang/OTP application, designed and intended to be run as a microservice,
% implementing a simple urban bus routing prototype.
% =============================================================================
% Copyright (C) 2022 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% ----------------------------------------------------------------------------
%% @doc The main module of the application.
%% @end
%% ----------------------------------------------------------------------------
-module(bus_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("bus_helper.hrl").

start(_StartType, _StartArgs) ->
    io:put_chars(?NEW_LINE ++ ?MSG_WORK_IN_PROGRESS ++ ?NEW_LINE ++ ?NEW_LINE),

    bus_sup:start_link().

stop(_State) ->
    ok.

% vim:set nu et ts=4 sw=4:
