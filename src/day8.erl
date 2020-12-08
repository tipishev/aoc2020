-module(day8).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([parse/1]).

%%% solution

solve_part1(_Text) ->
    undefined.

solve_part2(_Input) ->
    undefined.


%%% internals

%%% Parsing Input

%%  Transform the input text into a non-empty list of instructions.

-type operation() :: nop | acc | jmp.
-type argument() :: integer().
-type instruction() :: {operation(), argument()}.

-spec parse(Text) ->
    Instructions when
      Text :: string(),
      Instructions :: [instruction(), ...].

parse(Text) ->
    Lines = string:lexemes(Text, "\n"),
    [parse_line(Line) || Line <- Lines].

%% Convert a single line of input into an instruction

-spec parse_line(Line) ->
    Instruction when
      Line :: string(),
      Instruction :: instruction().

parse_line(Line) ->
    [OperationStr, ArgumentStr] = string:lexemes(Line, " "),
    ArgumentInt = list_to_integer(ArgumentStr),
    case OperationStr of
        "nop" -> {nop, ArgumentInt};
        "acc" -> {acc, ArgumentInt};
        "jmp" -> {jmp, ArgumentInt}
    end.
