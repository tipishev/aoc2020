%%%-------------------------------------------------------------------
%% @doc aoc2020 public API
%% @end
%%%-------------------------------------------------------------------

-module(aoc2020_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aoc2020_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
