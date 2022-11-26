%
% apps/bus/src/bus_sup.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.1
% =============================================================================
% An Erlang/OTP application, designed and intended to be run as a microservice,
% implementing a simple urban bus routing prototype.
% =============================================================================
% Copyright (C) 2022 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%% ----------------------------------------------------------------------------
%% @doc The supervisor module of the application.
%% @end
%% ----------------------------------------------------------------------------

-module(bus_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy  => one_for_all,
        intensity => 0,
        period    => 1
    },

    ChildSpecs = [],

    {ok, {
        SupFlags,
        ChildSpecs
    }}.

% vim:set nu et ts=4 sw=4:
