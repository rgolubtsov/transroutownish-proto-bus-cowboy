%
% apps/bus/src/bus_handler.erl
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
%% @doc The request handler module of the application.
%%
%% @version 0.3.5
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
%% @param State The so-called "state" of the HTTP request.
%%              This can be any data, payload passed with the request
%%              and used somehow during processing the request.
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
%% @param State The so-called "state" of the HTTP request.
%%              This can be any data, payload passed with the request
%%              and used somehow during processing the request.
%%
%% @returns A list of media types the microservice provides when responding
%%          to the client. A special callback then will be called for any
%%          appropriate request regarding the corresponding media type:
%%          `application/json' is currently the only one used.
content_types_provided(Req, State) ->
    {[{{
        ?MIME_TYPE, ?MIME_SUB_TYPE, % <== content-type: application/json
        []                          % <== No any params needed for this c-type.
    }, to_json}], Req, State}.

%% ----------------------------------------------------------------------------
%% @doc The so-called `ProvideCallback', used to return the response body.
%%
%% @param Req   The incoming HTTP request object.
%% @param State The so-called "state" of the HTTP request.
%%              This can be any data, payload passed with the request
%%              and used somehow during processing the request.
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
    #{
        debug_log_enabled := DebugLogEnabled,
        routes_list       := RoutesList,
        syslog            := Syslog
    } = State,

    % -------------------------------------------------------------------------
    % --- Parsing and validating request params - Begin -----------------------
    % -------------------------------------------------------------------------
    #{from := From_, to := To_} = cowboy_req:match_qs([
        {from, [], ?ZERO},
        {to,   [], ?ZERO}
    ], Req),

    From__ = if (is_boolean(From_)) -> ?ZERO; (true) -> From_ end,
    To__   = if (is_boolean(To_  )) -> ?ZERO; (true) -> To_   end,

    if (DebugLogEnabled) ->
        FROM___ = binary:bin_to_list(?FROM ),
        From___ = binary:bin_to_list(From__),
        TO___   = binary:bin_to_list(?TO   ),
        To___   = binary:bin_to_list(To__  ),

        logger:debug(               FROM___ ++ ?EQUALS ++ From___
           ++ ?SPACE?V_BAR?SPACE ++ TO___   ++ ?EQUALS ++ To___),

        syslog:log  (Syslog, debug, FROM___ ++ ?EQUALS ++ From___
           ++ ?SPACE?V_BAR?SPACE ++ TO___   ++ ?EQUALS ++ To___);
       (true) -> false
    end,

    From = try binary_to_integer(From__) catch error:badarg -> 0 end,
    To   = try binary_to_integer(To__  ) catch error:badarg -> 0 end,

    IsRequestMalformed = if ((From<1) or (To<1)) -> true; (true) -> false end,
    % -------------------------------------------------------------------------
    % --- Parsing and validating request params - End -------------------------
    % -------------------------------------------------------------------------

    if (IsRequestMalformed) ->
        % Not using the malformed_request/2 callback when responding
        % with the HTTP 400 Bad Request status code; instead setting
        % the response body and then sending the response, specifying
        % the status code explicitly. All the required headers are already
        % there, including the content-type, which is set correctly.
        cowboy_req:reply(?HTTP_400_BAD_REQ, cowboy_req:set_resp_body
        (jsx:encode(#{
            error => ?ERR_REQ_PARAMS_MUST_BE_POSITIVE_INTS
        }), Req));
       (true) ->
        % Performing the routes processing to find out the direct route.
        Direct = if (From =:= To) -> false;
           (true) ->
            find_direct_route(DebugLogEnabled, RoutesList, From, To)
        end,

        {jsx:encode(#{
            ?FROM  => From,
            ?TO    => To,
            direct => Direct
        }), Req, State}
    end.

%% ----------------------------------------------------------------------------
%% @doc Performs the routes processing (onto bus stops sequences) to identify
%%      and return whether a particular interval between two bus stop points
%%      given is direct (i.e. contains in any of the routes), or not.
%%
%% @param DebugLogEnabled The debug logging enabler.
%% @param RoutesList      A list containing all available routes.
%% @param From_           The starting bus stop point.
%% @param To_             The ending   bus stop point.
%%
%% @returns `true' if the direct route is found, `false' otherwise.
find_direct_route(DebugLogEnabled, RoutesList, From_, To_) ->
    From = integer_to_list(From_),
    To   = integer_to_list(To_  ),

    try
        lists:foldl(fun(Route, I) ->
            if (DebugLogEnabled) ->
                logger:debug(integer_to_list(I)++?SPACE?EQUALS?SPACE++Route);
               (true) -> false
            end,

            MatchFrom = re:run(Route, ?SEQ1_REGEX ++ From ++ ?SEQ2_REGEX),
            if (element(1, MatchFrom) =:= match) ->
                % Pinning in the starting bus stop point, if it's found.
                % Next, searching for the ending bus stop point
                % on the current route, beginning at the pinned point.
                RouteFrom = string:slice(Route, string:str(Route, From) - 1),

                if (DebugLogEnabled) ->
                    logger:debug(From ++ ?SPACE?V_BAR?SPACE ++ RouteFrom);
                   (true) -> false
                end,

                MatchTo = re:run(RouteFrom, ?SEQ1_REGEX ++ To ++ ?SEQ2_REGEX),
                if (element(1, MatchTo) =:= match) ->
                    throw(true);
                   (true) -> false
                end;
               (true) -> false
            end,

            I + 1
        end, 1, RoutesList), false
    catch
        (true) -> true % <== Like direct = true; break;
    end.

% vim:set nu et ts=4 sw=4:
