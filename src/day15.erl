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

play(StartingNumbers) ->
    Memory = init_memory(StartingNumbers).

play(Memory) -> todo.
    

%% @doc Initializes the memory with starting numbers.
init_memory(Starting) ->
    init_memory(Starting, #{}, 1, undefined).
init_memory([], Acc, Turn, Last) ->
    {mem, Acc, last, Last, turn, Turn};
init_memory([H | T], Acc, Turn, _Last) ->
    init_memory(T, Acc#{H => Turn}, Turn + 1, H).

% update_memory(Memory, Number, CurrentTurn) ->
