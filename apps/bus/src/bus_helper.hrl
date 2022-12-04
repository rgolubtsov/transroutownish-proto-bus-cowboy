%
% apps/bus/src/bus_helper.hrl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.4
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
%% @version 0.0.4
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
-define(MSG_SERVER_STARTED, "Server started on port ").
-define(MSG_SERVER_STOPPED, "Server stopped"         ).

%% ----------------------------------------------------------------------------
%% @doc The regex pattern for the element to be excluded from a bus stops
%%      sequence: it is an arbitrary identifier of a route,
%%      which is not used in the routes processing anyhow.
-define(ROUTE_ID_REGEX, "^\\d+").

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

% vim:set nu et ts=4 sw=4:
