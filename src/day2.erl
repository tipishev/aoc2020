-module(day2).

% -behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([validate/4, count/2, validate2/4, is_char_at/3]).

%%% solution behavior

solve_part1(Input) ->
    length([Password || {Character, AtLeast, AtMost, Password} <- Input, 
                 validate(Character, AtLeast, AtMost, Password)]).


solve_part2(Input) ->
    length([Password || {Character, AtLeast, AtMost, Password} <- Input, 
                 validate2(Character, AtLeast, AtMost, Password)]).

%%% internals


%%% Part1

-spec validate(Character, AtLeast, AtMost, Password) ->
    Result when
      Character :: string(),
      AtLeast :: pos_integer(),
      AtMost :: pos_integer(),
      Password :: string(),
      Result :: boolean().

validate(Character, AtLeast, AtMost, Password) ->
    Count = count(Character, Password),
    io:format("~p~n", [Count]),
    (Count >= AtLeast) andalso (Count =< AtMost).

-spec count([Character], String) ->
    Count when
      Character :: pos_integer(),
      String :: string(),
      Count :: non_neg_integer().

count([Character], String) ->
    count(Character, String, 0).

-spec count(Character, String, Count) ->
    FinalCount when
      Character :: non_neg_integer(),
      String :: string(),
      Count :: non_neg_integer(),
      FinalCount :: non_neg_integer().

count(_, [], Count) -> Count;
count(Character, [Character|Tail], Count) ->
    count(Character, Tail, Count + 1);
count(Character, [_NotCharacter|Tail], Count) ->
    count(Character, Tail, Count).


%%% Part2


-spec validate2(Character, FirstPos, SecondPos, Password) ->
    Result when
      Character :: [pos_integer()],
      FirstPos :: pos_integer(),
      SecondPos :: pos_integer(),
      Password :: string(),
      Result :: boolean().

validate2(Character, FirstPos, SecondPos, Password) ->
    (is_char_at(Character, FirstPos, Password))
    xor (is_char_at(Character, SecondPos, Password)).

-spec is_char_at(Character, Pos, String) ->
    boolean() when
      Character :: [pos_integer()],
      Pos :: non_neg_integer(),
      String :: string().

is_char_at([Character], Pos, String) ->
    io:format("~p, ~p, ~p~n", [Character, Pos, String]),
    lists:nth(Pos, String) =:= Character.
