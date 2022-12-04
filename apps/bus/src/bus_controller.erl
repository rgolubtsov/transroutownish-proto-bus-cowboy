%
% apps/bus/src/bus_controller.erl
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
%% @doc The controller module of the application.
%%
%% @version 0.0.3
%% @since   0.0.3
%% @end
%% ----------------------------------------------------------------------------
-module(bus_controller).

-export([startup/1]).

-include("bus_helper.hrl").

%% ----------------------------------------------------------------------------
%% @doc Starts up the web server.
%%
%% @param Args The tuple containing the server port number to listen on,
%%             as the first element.
startup(Args) ->
    ServerPort      = element(1, Args),
    DebugLogEnabled = element(2, Args),
    RoutesList      = element(3, Args),

    %% --- Debug output - Begin -----------------------------------------------
    if (DebugLogEnabled) ->
        logger:debug(RoutesList ++ ?V_BAR); (true) -> false
    end.
    %% --- Debug output - End -------------------------------------------------

%   logger:info(?MSG_SERVER_STARTED ++ ServerPort).

% vim:set nu et ts=4 sw=4:
