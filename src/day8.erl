-module(day8).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([parse/1, detect_loop/1, fix/1]).

%%% solution

solve_part1(Text) ->
    Instructions = parse(Text),
    {loops, Acc} = detect_loop(Instructions),
    Acc.

solve_part2(Text) ->
    Instructions = parse(Text),
    fix(Instructions).

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


%%% Part 1

%% detects the value of accumulator before running a repeating command
-type behaviour() :: halts | loops.
-spec detect_loop(Instructions) ->
    DetectionResult when
      Instructions :: instructions(),
      DetectionResult :: {behaviour(), integer()}.

detect_loop(Instructions) ->
    detect_loop(Instructions, _CurrentInstruction=1,
                _Visited=sets:new(), _Acc=0).

-spec detect_loop(Instructions, CurrentInstruction, Visited, Acc) ->
    DetectionResult when
      Instructions :: instructions(),
      CurrentInstruction :: pos_integer(),
      Visited :: sets:set(),
      Acc :: integer(),
      DetectionResult :: {behaviour(), integer()}.

detect_loop(Instructions, CurrentInstruction, _, Acc)
  when  CurrentInstruction =:= length(Instructions) + 1 ->
    {halts, Acc};
detect_loop(Instructions, CurrentInstruction, Visited, Acc) ->
    case sets:is_element(CurrentInstruction, Visited) of
        true -> {loops, Acc};
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

%%% Part 2

%% Changes a nop to jmp, or the other way around to make
%% the program terminate, outputs the value of Acc after the last instruction

-spec fix(Instructions) ->
    Acc when
      Instructions :: instructions(),
      Acc :: integer().

fix(Instructions) ->
    fix(Instructions, _Current=1).

-spec fix(Instructions, Current) ->
    Acc when
      Instructions :: instructions(),
      Current :: pos_integer(),
      Acc :: integer().

fix(Instructions, Current) ->
    Mutated = replace(Instructions, Current),
    case detect_loop(Mutated) of 
        {halts, Val} -> Val;
        {loops, _Val} -> fix(Instructions, Current + 1)
    end.

replace(Instructions, Index) ->
    Instruction = lists:nth(Index, Instructions),
    Replacement = case Instruction of
        {nop, Arg} -> {jmp, Arg}; 
        {jmp, Arg} -> {nop, Arg}; 
        Any -> Any
    end,
    lists:sublist(Instructions, Index - 1) ++ [Replacement] ++ lists:nthtail(Index, Instructions).
