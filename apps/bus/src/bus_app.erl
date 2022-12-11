%
% apps/bus/src/bus_app.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.5
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
%% @version 0.0.5
%% @since   0.0.1
%% @end
%% ----------------------------------------------------------------------------
-module(bus_app).

-behavior(application).

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
    logger:info(?MSG_WORK_IN_PROGRESS),

    % Getting the application settings.
    Settings = get_settings_(),

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

    % Starting up the bundled web server.
    bus_controller:startup({
        ServerPort,
        DebugLogEnabled,
        RoutesList
    }),

    bus_sup:start_link().

%% ----------------------------------------------------------------------------
%% @doc The application termination callback.
%%      Gets called after the application has been stopped.
%%      Currently does nothing.
%%
%% @param _State The `State' indicator, as returned from the `start' callback.
stop(_State) ->
    ok.

% -----------------------------------------------------------------------------
% Helper function. Used to get the application settings.
%
% Returns: The tuple containing values of individual settings.
get_settings_() ->
    % Retrieving the port number used to run the server.
    ServerPort_ = application:get_env(server_port),

    ServerPort  = if (ServerPort_ =/= undefined) ->
        ServerPort__ = element(2, ServerPort_),

        if ((ServerPort__ >= ?MIN_PORT)
        and (ServerPort__ =< ?MAX_PORT)) -> ServerPort__;
           (true) ->
            io:put_chars(?ERR_PORT_VALID_MUST_BE_POSITIVE_INT), io:nl(),

            ?DEF_PORT
        end;
       (true) ->
        io:put_chars(?ERR_PORT_VALID_MUST_BE_POSITIVE_INT), io:nl(),

        ?DEF_PORT
    end,

    % Identifying, whether debug logging is enabled.
    DebugLogEnabled_ = application:get_env(logger_debug_enabled),

    DebugLogEnabled  = if (DebugLogEnabled_ =/= undefined) ->
        if (element(2, DebugLogEnabled_) =:= yes) -> true;
           (true) -> false end; (true) -> false end,

    % Retrieving the path and filename of the routes data store.
    DatastorePathPrefix_ = application:get_env(routes_datastore_path_prefix),
    DatastorePathPrefix  = if (DatastorePathPrefix_ =/= undefined) ->
        DatastorePathPrefix0 = element(2, DatastorePathPrefix_),
        DatastorePathPrefix1 = string:is_empty(DatastorePathPrefix0),
        if (not DatastorePathPrefix1) -> DatastorePathPrefix0;
           (true) -> ?SAMPLE_ROUTES_PATH_PREFIX
        end;
       (true) -> ?SAMPLE_ROUTES_PATH_PREFIX
    end,

    DatastorePathDir_ = application:get_env(routes_datastore_path_dir),
    DatastorePathDir  = if (DatastorePathDir_ =/= undefined) ->
        DatastorePathDir0 = element(2, DatastorePathDir_),
        DatastorePathDir1 = string:is_empty(DatastorePathDir0),
        if (not DatastorePathDir1) -> DatastorePathDir0;
           (true) -> ?SAMPLE_ROUTES_PATH_DIR
        end;
       (true) -> ?SAMPLE_ROUTES_PATH_DIR
    end,

    DatastoreFilename_ = application:get_env(routes_datastore_filename),
    DatastoreFilename  = if (DatastoreFilename_ =/= undefined) ->
        DatastoreFilename0 = element(2, DatastoreFilename_),
        DatastoreFilename1 = string:is_empty(DatastoreFilename0),
        if (not DatastoreFilename1) -> DatastoreFilename0;
           (true) -> ?SAMPLE_ROUTES_FILENAME
        end;
       (true) -> ?SAMPLE_ROUTES_FILENAME
    end,

    {
        ServerPort,
        DebugLogEnabled, % <== "true" or "false".
        DatastorePathPrefix
     ++ DatastorePathDir
     ++ DatastoreFilename
    }.

% vim:set nu et ts=4 sw=4:
