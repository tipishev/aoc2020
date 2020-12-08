-module(day8).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([parse/1, detect_loop/1]).

%%% solution

solve_part1(Text) ->
    Instructions = parse(Text),
    detect_loop(Instructions).

solve_part2(_Input) ->
    undefined.

%%% internals

%%% Parsing Input

%%  Transform the input text into a non-empty list of instructions.

-type operation() :: nop | acc | jmp.
-type argument() :: integer().
-type instruction() :: {operation(), argument()}.
-type instructions() :: [instruction()].

-spec parse(Text) ->
    Instructions when
      Text :: string(),
      Instructions :: instructions().

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

%% detects the value of accumulator before running a repeating command

-spec detect_loop(Instructions) ->
    AccumulatorValue when
      Instructions :: instructions(),
      AccumulatorValue :: integer().

detect_loop(Instructions) ->
    detect_loop(Instructions, _CurrentInstruction=1,
                _Visited=sets:new(), _Acc=0).

-spec detect_loop(Instructions, CurrentInstruction, Visited, Acc) ->
    AccBeforeLoop when
      Instructions :: instructions(),
      CurrentInstruction :: pos_integer(),
      Visited :: sets:set(),
      Acc :: integer(),
      AccBeforeLoop :: integer().

detect_loop(Instructions, CurrentInstruction, Visited, Acc) ->
    case sets:is_element(CurrentInstruction, Visited) of
        true -> Acc;
        false ->
            NewVisited = sets:add_element(CurrentInstruction, Visited),
            {Operation, Argument} = lists:nth(CurrentInstruction,
                                              Instructions),
            case Operation of
                nop -> detect_loop(Instructions, CurrentInstruction + 1,
                                   NewVisited, Acc);
                acc -> detect_loop(Instructions, CurrentInstruction + 1,
                                   NewVisited, Acc + Argument);
                jmp -> detect_loop(Instructions,
                                   CurrentInstruction + Argument,
                                   NewVisited, Acc)
            end
    end.
