%
% apps/bus/src/bus_controller.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.1.0
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
%% @version 0.1.0
%% @since   0.0.3
%% @end
%% ----------------------------------------------------------------------------
-module(bus_controller).

-export([startup/1]).

-include("bus_helper.hrl").

%% ----------------------------------------------------------------------------
%% @doc Starts up the bundled web server.
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
    end,
    %% --- Debug output - End -------------------------------------------------

    % Starting up the Cowboy web server along with all their dependencies.
    {ok, _} = application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
        % Serving the sample routes data store for any request, any path,
        % and any host as an example of using Cowboy's internal special
        % request handler.
        {'_', [
%           {'_', cowboy_static, {priv_file, bus, ?SAMPLE_ROUTES_PATH_DIR
%                                                 ?SAMPLE_ROUTES_FILENAME,
%               [{mimetypes, cow_mimetypes, all}]
%           }},
            {"/", bus_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(bus_listener, [
        {port, ServerPort}
    ], #{
        env => #{dispatch => Dispatch}
    }),

    logger:info(?MSG_SERVER_STARTED ++ integer_to_list(ServerPort)).

% vim:set nu et ts=4 sw=4:
