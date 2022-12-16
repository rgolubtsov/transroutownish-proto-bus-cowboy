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
%% @returns The `cowboy_rest' tuple containing the request object
%%          along with the state of the request.
%%          The atom `cowboy_rest' indicates that Cowboy will pick
%%          the REST handler behavior to operate on requests.
init(Req, State) ->
    {cowboy_rest, Req, State}.

%% ----------------------------------------------------------------------------
%% @doc The REST-specific callback to respond to the client
%%      when one of the `HEAD', `GET', or `OPTIONS' methods is used.
%%
%% @param Req   The incoming HTTP request object.
%% @param State TODO: Provide the description of the `State' param.
%%
%% @returns The list of media types the microservice provides when responding
%%          to the client. The special callback then will be called for any
%%          appropriate request regarding the corresponding media type:
%%          `application/json' is currently the only used one.
content_types_provided(Req, State) ->
    {[{{
        ?MIME_TYPE, ?MIME_SUB_TYPE, % <== content-type: application/json
        []                          % <== No any params will be accepted.
    }, to_json}], Req, State}.

%% ----------------------------------------------------------------------------
%% @doc The so-called `ProvideCallback', used to return the response body.
%%
%% @param Req   The incoming HTTP request object.
%% @param State TODO: Provide the description of the `State' param.
%%
%% @returns The body of the response in the JSON representation,
%%          containing the following properties:
%%          <ul>
%%          <li><strong>from</strong> &mdash; The starting bus stop point.</li>
%%          <li><strong>to</strong>   &mdash; The ending   bus stop point.</li>
%%          <li><strong>direct</strong> &mdash; The logical indicator
%%          of the presence of a direct route from `from' to `to'.</li>
%%          </ul>
to_json(Req, State) ->
    #{from := From, to := To} = cowboy_req:match_qs([{from,int},{to,int}],Req),

    logger:debug(?FROM?EQUALS ++ integer_to_list(From) ++ ?SPACE?V_BAR?SPACE
                   ?TO?EQUALS ++ integer_to_list(To  )),

    {[
        "{\""?FROM"\":", integer_to_list(From),
          ",\""?TO"\":", integer_to_list(To  ), "}"
    ], Req, State}.

% vim:set nu et ts=4 sw=4:
