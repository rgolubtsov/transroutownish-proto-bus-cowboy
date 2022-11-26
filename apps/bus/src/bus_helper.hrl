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
-define(NEW_LINE, "\n").

% Common notification messages.
-define(MSG_WORK_IN_PROGRESS, "This is a work in progress - "
                           ++ "please wait for a while...").

% vim:set nu et ts=4 sw=4:
