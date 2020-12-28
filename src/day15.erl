-module(day15).

-export([solve_part1/1, solve_part2/1]).

-export([parse/1, init_memory/1]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.


%%% Parse

parse(Input) ->
    [list_to_integer(N) || N <- string:lexemes(Input, ",")].

%%% Solve Part 1

%% @doc returns the number spoken after Timeout.
play(StartingNumbers, Timeout) ->
    todo.

% play(CurrentTurn= length(StartingNumbers) + 1,
%      MostRecentNumber=lists:last(StartingNumbers),
%      Memory,
%      Timeout).

%% @doc Initializes the memory with starting numbers.
init_memory(StartingNumbers) ->
    init_memory(StartingNumbers, #{}, 1).
init_memory([], Acc, TurnNum) ->
    {memory, Acc, turn, TurnNum};
init_memory([H | T], Acc, TurnNum) ->
    init_memory(T, Acc#{H => {once, TurnNum}}, TurnNum + 1).

update_memory(Memory, Number, CurrentTurn) ->
