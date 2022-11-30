%
% apps/bus/src/bus_app.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.3
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
%% @version 0.0.3
%% @since   0.0.1
%% @end
%% ----------------------------------------------------------------------------
-module(bus_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("bus_helper.hrl").

%% ----------------------------------------------------------------------------
%% @doc The regex pattern for the element to be excluded from a bus stops
%%      sequence: it is an arbitrary identifier of a route,
%%      which is not used in the routes processing anyhow.
-define(ROUTE_ID_REGEX, "^\\d+").

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
%   io:put_chars(?NEW_LINE ?MSG_WORK_IN_PROGRESS ?NEW_LINE ?NEW_LINE),
    io:nl(), io:put_chars(?MSG_WORK_IN_PROGRESS), io:nl(), io:nl(),

    % Getting the application settings.
    Settings = get_settings(),

    ServerPort      = element(1, Settings),
    DebugLogEnabled = element(2, Settings),
    Datastore       = element(3, Settings),

    % Slurping routes from the routes data store.
    Routes = string:split(
        element(2,file:read_file(filename:join(code:priv_dir(bus),Datastore))),
    ?NEW_LINE, all),

    RoutesList = lists:foldl(fun(Route, Routes_) ->
        lists:append([Routes_, [
           re:replace(Route, ?ROUTE_ID_REGEX, ?EMPTY_STRING, [{return, list}])
        ++ ?SPACE]])
    end, [], Routes),

    %% --- Debug output - Begin -----------------------------------------------
    io:put_chars(RoutesList ++ ?V_BAR), io:nl(), io:nl(),
    %% --- Debug output - End -------------------------------------------------

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
