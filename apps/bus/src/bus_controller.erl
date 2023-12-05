%
% apps/bus/src/bus_controller.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.3.5
% =============================================================================
% An Erlang/OTP application, designed and intended to be run as a microservice,
% implementing a simple urban bus routing prototype.
% =============================================================================
% Copyright (C) 2022-2023 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% ----------------------------------------------------------------------------
%% @doc The controller module of the application.
%%
%% @version 0.3.5
%% @since   0.0.3
%% @end
%% ----------------------------------------------------------------------------
-module(bus_controller).

-export([startup/1]).

-include("bus_helper.hrl").

%% ----------------------------------------------------------------------------
%% @doc Starts up the bundled web server.
%%
%% @param Args A tuple containing the server port number to listen on,
%%             as the first element.
startup(Args) ->
    ServerPort      = element(1, Args),
    DebugLogEnabled = element(2, Args),
    RoutesList      = element(3, Args),
    Syslog          = element(4, Args),

    % Starting up the Cowboy web server along with all their dependencies.
    {ok, _} = application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
        {'_', [
            % Serving the sample routes data store for any request, any path,
            % and any host as an example of using Cowboy's internal special
            % request handler. (Despite being inactive, let it remain.)
%           {'_', cowboy_static, {priv_file, bus, ?SAMPLE_ROUTES_PATH_DIR
%                                                 ?SAMPLE_ROUTES_FILENAME,
%               [{mimetypes, cow_mimetypes, all}]
%           }},
            {
                ?SLASH?REST_PREFIX?SLASH?REST_DIRECT, % <== GET /route/direct
                bus_handler, #{
                    debug_log_enabled => DebugLogEnabled,
                    routes_list       => RoutesList,
                    syslog            => Syslog
                }
            }
        ]}
    ]),

    Status_ = cowboy:start_clear(bus_listener, [
        {port, ServerPort}
    ], #{
        env => #{dispatch => Dispatch}
    }),

    if (element(1, Status_) =:= error) ->
        if (element(2, Status_) =:= eaddrinuse) ->
            logger:critical(?ERR_CANNOT_START_SERVER?ERR_ADDR_ALREADY_IN_USE);
           (true) ->
            logger:critical(?ERR_CANNOT_START_SERVER?ERR_SERV_UNKNOWN_REASON)
        end,

        init:stop(?EXIT_FAILURE);
       (true) ->
        ServerPort_ = integer_to_list(ServerPort),

        logger:info(              ?MSG_SERVER_STARTED ++ ServerPort_),
        syslog:log (Syslog, info, ?MSG_SERVER_STARTED ++ ServerPort_)
    end.

% vim:set nu et ts=4 sw=4:
