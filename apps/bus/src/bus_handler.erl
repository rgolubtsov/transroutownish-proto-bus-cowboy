%
% apps/bus/src/bus_handler.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.1.5
% =============================================================================
% An Erlang/OTP application, designed and intended to be run as a microservice,
% implementing a simple urban bus routing prototype.
% =============================================================================
% Copyright (C) 2022 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% ----------------------------------------------------------------------------
%% @doc The request handler module of the application.
%%
%% @version 0.1.5
%% @since   0.1.0
%% @end
%% ----------------------------------------------------------------------------
-module(bus_handler).

-export([
    init/2,
    content_types_provided/2,
    to_json/2
]).

-include("bus_helper.hrl").

%% ----------------------------------------------------------------------------
%% @doc The request handler initialization and processing callback.
%%      Used to process the incoming request and send the response.
%%
%% @param Req   The incoming HTTP request object.
%% @param State TODO: Provide the description of the `State' param.
%%
%% @returns The `ok' tuple containing a new request object
%%          along with the state of the request.
init(Req, State) ->
    {cowboy_rest, Req, State}.

%% ----------------------------------------------------------------------------
content_types_provided(Req, State) ->
    {[{{
        ?MIME_TYPE, ?MIME_SUB_TYPE, % <== content-type: application/json
        []                          % <== No any params will be accepted.
    }, to_json}], Req, State}.

%% ----------------------------------------------------------------------------
to_json(Req, State) ->
    {<<"{}">>, Req, State}.

% vim:set nu et ts=4 sw=4:
