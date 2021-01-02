-module(day15).

-export([solve_part1/1, solve_part2/1]).

-export([parse/1, spoken/2, enumerate/1]).

%%% solution

solve_part1(Input) ->
    spoken(parse(Input), 2020).

solve_part2(Input) ->
    spoken(parse(Input), 30000000).

%%% Parse

parse(Input) ->
    [list_to_integer(N)
     || N <- string:lexemes(
               string:trim(Input, trailing), ",")].

%%% Solve Part 1

% simple but useless shortcutting
spoken(Starting, MaxTurn) when MaxTurn =< length(Starting)->
    lists:nth(MaxTurn, Starting);
spoken(Starting, MaxTurn) ->
    InitMem = maps:from_list(enumerate(Starting)),
    spoken(_Starting=whatever, MaxTurn,
           _Current=length(Starting) + 1,
           _Mrsn=lists:last(Starting),
           _Mem=InitMem).

spoken(_Starting, MaxTurn, Current, MRSN, _Mem) 
  when Current =:= MaxTurn + 1 ->
    MRSN;
spoken(_Starting, MaxTurn, Current, MRSN, Mem) ->
    {Spoken, NewMem} = case maps:is_key(MRSN, Mem) of
        false ->
            {0, Mem#{MRSN => Current - 1}};
        true ->
            MemVal = maps:get(MRSN, Mem),
            {Current - 1 - MemVal, Mem#{MRSN => Current - 1}}
    end,
    spoken(_Starting, MaxTurn, Current + 1, Spoken, NewMem).

%%% Herlpers

enumerate(List) ->
    lists:zip(List, lists:seq(1, length(List))).
