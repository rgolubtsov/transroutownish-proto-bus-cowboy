%
% apps/bus/src/bus_helper.hrl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.3.5
% =============================================================================
% An Erlang/OTP application, designed and intended to be run as a microservice,
% implementing a simple urban bus routing prototype.
% =============================================================================
% Copyright (C) 2022-2025 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% ----------------------------------------------------------------------------
%% @doc The helper header file for the application.
%%
%% @version 0.3.5
%% @since   0.0.1
%% @end
%% ----------------------------------------------------------------------------

% Helper constants.
-define(EXIT_FAILURE,    1). %    Failing exit status.
-define(EXIT_SUCCESS,    0). % Successful exit status.
-define(EMPTY_STRING,   "").
-define(SPACE,         " ").
-define(V_BAR,         "|").
-define(SLASH,         "/").
-define(EQUALS,        "=").
-define(NEW_LINE,     "\n").

% Common error messages.
-define(ERR_PORT_VALID_MUST_BE_POSITIVE_INT,
        "Valid server port must be a positive integer value, "
        "in the range 1024 .. 49151. The default value of 8080 "
        "will be used instead.").
-define(ERR_DATASTORE_NOT_FOUND,
        "FATAL: Data store file not found. Quitting...").
-define(ERR_CANNOT_START_SERVER,
        "FATAL: Cannot start server ").
-define(ERR_ADDR_ALREADY_IN_USE,
        "due to address requested already in use. Quitting...").
-define(ERR_SERV_UNKNOWN_REASON,
        "for an unknown reason. Quitting...").
-define(ERR_REQ_PARAMS_MUST_BE_POSITIVE_INTS,
      <<"Request parameters must take positive integer values, "
        "in the range 1 .. 2,147,483,647. Please check your inputs.">>).

% Common notification messages.
-define(MSG_SERVER_STARTED, "Server started on port ").
-define(MSG_SERVER_STOPPED, "Server stopped"         ).

%% ----------------------------------------------------------------------------
%% @doc The regex pattern for the element to be excluded from a bus stops
%%      sequence: it is an arbitrary identifier of a route,
%%      which is not used in the routes processing anyhow.
-define(ROUTE_ID_REGEX, "^\\d+").

%% ----------------------------------------------------------------------------
%% @doc The regex pattern for the leading part of a bus stops sequence,
%%      before the matching element.
-define(SEQ1_REGEX, ".*\\s").

%% ----------------------------------------------------------------------------
%% @doc The regex pattern for the trailing part of a bus stops sequence,
%%      after the matching element.
-define(SEQ2_REGEX, "\\s.*").

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

% REST URI path-related constants.
-define(REST_PREFIX, "route" ).
-define(REST_DIRECT, "direct").

% HTTP response-related constants.
-define(MIME_TYPE,     <<"application">>).
-define(MIME_SUB_TYPE, <<"json">>       ).

% HTTP request parameter names.
-define(FROM, <<"from">>).
-define(TO,   <<"to">>  ).

% HTTP request parameter default values.
-define(ZERO, <<"0">>).

% HTTP response status codes.
-define(HTTP_400_BAD_REQ, 400).

% vim:set nu et ts=4 sw=4:
