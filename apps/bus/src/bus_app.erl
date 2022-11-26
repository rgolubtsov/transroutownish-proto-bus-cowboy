%
% apps/bus/src/bus_app.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.2
% =============================================================================
% An Erlang/OTP application, designed and intended to be run as a microservice,
% implementing a simple urban bus routing prototype.
% =============================================================================
% Copyright (C) 2022 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% ----------------------------------------------------------------------------
%% @doc The callback module of the application.
%%
%% @version 0.0.2
%% @since   0.0.1
%% @end
%% ----------------------------------------------------------------------------
-module(bus_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("bus_helper.hrl").

%% ----------------------------------------------------------------------------
%% @doc The application entry point callback.
%%      Creates the supervision tree by starting the top supervisor.
%%
%% @param _StartType The atom `normal'.
%% @param _StartArgs The list of start arguments.
%%
%% @returns The tuple containing the PID of the top supervisor
%%          and the `State' indicator (defaults to an empty list).
start(_StartType, _StartArgs) ->
    io:put_chars(?NEW_LINE ++ ?MSG_WORK_IN_PROGRESS ++ ?NEW_LINE ++ ?NEW_LINE),

    bus_sup:start_link().

%% ----------------------------------------------------------------------------
%% @doc The application termination callback.
%%      Gets called after the application has been stopped.
%%      Currently does nothing.
%%
%% @param _State The `State' indicator, as returned from the `start' callback.
stop(_State) ->
    ok.

% vim:set nu et ts=4 sw=4:
