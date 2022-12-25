%
% apps/bus/src/bus_app.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.2.0
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
%% @version 0.2.0
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
    % Getting the application settings.
    Settings = get_settings_(),

    ServerPort      = element(1, Settings),
    DebugLogEnabled = element(2, Settings),
    Datastore       = element(3, Settings),

    % Slurping routes from the routes data store.
    Routes_ = file:read_file(filename:join(code:priv_dir(bus), Datastore)),

    if ((element(1, Routes_) =:= error )
    and (element(2, Routes_) =:= enoent)) ->
        logger:critical(?ERR_DATASTORE_NOT_FOUND),

        init:stop(?EXIT_FAILURE);
       (true) -> false
    end,

    Routes = if (element(1, Routes_) =:= ok) ->
        string:split(element(2, Routes_), ?NEW_LINE, all);
       (true) -> []
    end,

    RoutesList = lists:foldl(fun(Route, Routes__) ->
        lists:append([Routes__, [
           re:replace(Route, ?ROUTE_ID_REGEX, ?EMPTY_STRING, [{return, list}])
        ++ ?SPACE]])
    end, [], lists:droplast(Routes)),

    AppName = atom_to_list(element(2, application:get_application())),

    % Opening the system logger.
    % Calling <syslog.h> openlog(NULL, LOG_CONS | LOG_PID, LOG_DAEMON);
    syslog:start(), {ok, Syslog} = syslog:open(AppName, [cons, pid], daemon),

    % Starting up the bundled web server.
    bus_controller:startup({
        ServerPort,
        DebugLogEnabled,
        RoutesList,
        Syslog
    }),

    bus_sup:start_link().

%% ----------------------------------------------------------------------------
%% @doc The application termination callback.
%%      Gets called after the application has been stopped.
%%
%% @param _State The `State' indicator, as returned from the `start' callback.
stop(_State) ->
    logger:info(?MSG_SERVER_STOPPED),

    ok = cowboy:stop_listener(bus_listener).

% -----------------------------------------------------------------------------
% Helper function. Used to get the application settings.
%
% Returns: The tuple containing values of individual settings.
get_settings_() ->
    % Retrieving the port number used to run the server -----------------------
    ServerPort_ = application:get_env(server_port),

    ServerPort  = if (ServerPort_ =/= undefined) ->
        ServerPort__ = element(2, ServerPort_),

        if ((ServerPort__ >= ?MIN_PORT)
        and (ServerPort__ =< ?MAX_PORT)) -> ServerPort__;
           (true) ->
            logger:error(?ERR_PORT_VALID_MUST_BE_POSITIVE_INT),

            ?DEF_PORT
        end;
       (true) ->
        logger:error(?ERR_PORT_VALID_MUST_BE_POSITIVE_INT),

        ?DEF_PORT
    end,

    % Identifying, whether debug logging is enabled ---------------------------
    DebugLogEnabled_ = application:get_env(logger_debug_enabled),

    DebugLogEnabled  = if (DebugLogEnabled_ =/= undefined) ->
        if (element(2, DebugLogEnabled_) =:= yes) -> true;
           (true) -> false end; (true) -> false end,

    % Retrieving the path and filename of the routes data store ---------------
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
