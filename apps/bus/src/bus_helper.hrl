%
% apps/bus/src/bus_helper.hrl
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
%% @doc The helper header file for the application.
%%
%% @version 0.0.2
%% @since   0.0.1
%% @end
%% ----------------------------------------------------------------------------

% Helper constants.
-define(EMPTY_STRING,   "").
-define(SPACE,         " ").
-define(V_BAR,         "|").
-define(NEW_LINE,     "\n").

% Common notification messages.
-define(MSG_WORK_IN_PROGRESS, "This is a work in progress"
                        " - " "please wait for a while...").

% -----------------------------------------------------------------------------
% Helper function. Used to get the application settings.
%
% Returns: The tuple containing values of individual settings.
get_settings() ->
    DebugLogEnabled_ = application:get_env(logger_debug_enabled),

    DebugLogEnabled  = if (DebugLogEnabled_ =/= undefined) ->
        if (element(2, DebugLogEnabled_) =:= yes) -> true; (true) -> false end;
                                                           (true) -> false end,

    {
        element(2, application:get_env(server_port)),
        DebugLogEnabled, % <== "true" or "false".

        % The path and filename of the routes data store (as 3rd tuple elem).
        element(2, application:get_env(routes_datastore_path_prefix))
     ++ element(2, application:get_env(routes_datastore_path_dir   ))
     ++ element(2, application:get_env(routes_datastore_filename   ))
    }.

% vim:set nu et ts=4 sw=4:
