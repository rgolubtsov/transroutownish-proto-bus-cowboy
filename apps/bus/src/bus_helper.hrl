%
% apps/bus/src/bus_helper.hrl
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
%% @doc The helper header file for the application.
%%
%% @version 0.0.3
%% @since   0.0.1
%% @end
%% ----------------------------------------------------------------------------

% Helper constants.
-define(EMPTY_STRING,   "").
-define(SPACE,         " ").
-define(V_BAR,         "|").
-define(NEW_LINE,     "\n").

% Common error messages.
-define(ERR_PORT_VALID_MUST_BE_POSITIVE_INT,
        "Valid server port must be a positive integer value, "
        "in the range 1024 .. 49151. The default value of 8080 "
        "will be used instead.").
-define(ERR_DATASTORE_NOT_FOUND,
        "FATAL: Data store file not found. Quitting...").

% Common notification messages.
-define(MSG_WORK_IN_PROGRESS, "This is a work in progress"
                        " - " "please wait for a while...").

%% ----------------------------------------------------------------------------
%% @doc The minimum port number allowed.
-define(MIN_PORT, 1024).

%% ----------------------------------------------------------------------------
%% @doc The maximum port number allowed.
-define(MAX_PORT, 49151).

%% ----------------------------------------------------------------------------
%% @doc The default server port number.
-define(DEF_PORT, 8080).

% The path and filename of the sample routes data store.
-define(SAMPLE_ROUTES_PATH_PREFIX, "./"        ).
-define(SAMPLE_ROUTES_PATH_DIR,    "data/"     ).
-define(SAMPLE_ROUTES_FILENAME,    "routes.txt").

% -----------------------------------------------------------------------------
% Helper function. Used to get the application settings.
%
% Returns: The tuple containing values of individual settings.
get_settings() ->
    % Retrieving the port number used to run the server.
    ServerPort_ = application:get_env(server_port),

    ServerPort  = integer_to_list(if (ServerPort_ =/= undefined) ->
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
    end),

    % Identifying, whether debug logging is enabled.
    DebugLogEnabled_ = application:get_env(logger_debug_enabled),

    DebugLogEnabled  = atom_to_list(if (DebugLogEnabled_ =/= undefined) ->
        if (element(2, DebugLogEnabled_) =:= yes) -> true;
           (true) -> false end; (true) -> false end),

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
