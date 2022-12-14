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

-export([init/2]).

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
    Req_ = cowboy_req:reply(?HTTP_200_OK, #{
        ?HDR_CONTENT_TYPE_N => ?HDR_CONTENT_TYPE_V
    }, Req),

    {ok, Req_, State}.

% vim:set nu et ts=4 sw=4:
