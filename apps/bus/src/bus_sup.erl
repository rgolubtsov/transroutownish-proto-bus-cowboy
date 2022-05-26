%
% apps/bus/src/bus_sup.erl
% =============================================================================
% Urban bus routing microservice prototype (Erlang/OTP port). Version 0.0.1
% =============================================================================
% A daemon written in Erlang/OTP, designed and intended to be run
% as a microservice, implementing a simple urban bus routing prototype.
% =============================================================================
% Copyright (C) 2022 Radislav (Radicchio) Golubtsov
%
% (See the LICENSE file at the top of the source tree.)
%

%%%-------------------------------------------------------------------
%% @doc bus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bus_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% sup_flags() = #{
%     strategy  => strategy(),        % optional
%     intensity => non_neg_integer(), % optional
%     period    => pos_integer()      % optional
% }
% child_spec() = #{
%     id       => child_id(), % mandatory
%     start    => mfargs(),   % mandatory
%     restart  => restart(),  % optional
%     shutdown => shutdown(), % optional
%     type     => worker(),   % optional
%     modules  => modules()   % optional
% }

init([]) ->
    SupFlags = #{
        strategy  => one_for_all,
        intensity => 0,
        period    => 1
    },
    ChildSpecs = [], {ok, {SupFlags, ChildSpecs}}.

%% internal functions

% vim:set nu et ts=4 sw=4:
