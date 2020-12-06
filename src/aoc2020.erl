-module(aoc2020).
-export([solve/1]).

-record(solution, {part1, part2}).
-type solution() :: #solution{}.

-type day() :: day1.

-define(INPUTS_DIR, "input").

% TODO generalize to all days
-spec solve(Day :: day()) -> solution().
solve(day1) ->
    Input = read_newline_separated_integers(day1),
    #solution{part1=day1:solve_part1(Input),
              part2=day1:solve_part2(Input)};
solve(day2) ->
    Input = read_day2(day2),
    #solution{part1=day2:solve_part1(Input),
              part2=day2:solve_part2(Input)};

solve(day3) ->
    Input = read_newline_separated_strings(day3),
    #solution{part1=day3:solve_part1(Input),
              part2=day3:solve_part2(Input)};

solve(day4) ->
    Input = read(day4),
    #solution{part1=day4:solve_part1(Input),
              part2=day4:solve_part2(Input)};

solve(day5) ->
    Input = read_newline_separated_strings(day5),
    #solution{part1=day5:solve_part1(Input),
              part2=day5:solve_part2(Input)};
solve(day6) ->
    Input = read(day6),
    #solution{part1=day6:solve_part1(Input),
              part2=day6:solve_part2(Input)}.

%%% Helpers


% let the custom parser handle the complex data
read(Filename) ->
    Fullpath = filename:join([?INPUTS_DIR, Filename]),
    {ok, FileData} = file:read_file(Fullpath),
    FileData.

%% reads newline-separated integers from "inputs/Filename"
-spec read_newline_separated_integers(day()) -> list(integer()).

read_newline_separated_integers(Filename) ->
    Fullpath = filename:join([?INPUTS_DIR, Filename]),
    {ok, Data} = file:read_file(Fullpath),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    lists:map(fun erlang:list_to_integer/1, Lines).

read_newline_separated_strings(Filename) ->
    Fullpath = filename:join([?INPUTS_DIR, Filename]),
    {ok, Data} = file:read_file(Fullpath),
    string:tokens(binary_to_list(Data), "\n").

% TODO move to day 2?
read_day2(Filename) ->
    Fullpath = filename:join([?INPUTS_DIR, Filename]),
    {ok, Data} = file:read_file(Fullpath),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    lists:map(fun parse_day2_line/1, Lines).

parse_day2_line(Line) ->
    [FirstNumStr, SecondNumStr, Character, Password] = string:tokens(Line, "- :"),
    {Character, 
     list_to_integer(FirstNumStr),
     list_to_integer(SecondNumStr),
     Password}.
