-module(aoc2020).
-export([solve/1]).

-record(solution, {part1, part2}).
-type solution() :: #solution{}.

-type day() :: day1.

-define(INPUTS_DIR, "inputs").

-spec solve(Day :: day()) -> solution().
solve(day1) ->
    Input = read_newline_separated_integers(day1),
    #solution{part1=day1:solve_part1(Input),
              part2=day1:solve_part2(Input)}.

%%% Helpers

%% reads newline-separated integers from "inputs/Filename"
-spec read_newline_separated_integers(day()) -> list(integer()).

read_newline_separated_integers(Filename) ->
    Fullpath = filename:join([?INPUTS_DIR, Filename]),
    {ok, Data} = file:read_file(Fullpath),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    lists:map(fun erlang:list_to_integer/1, Lines).
