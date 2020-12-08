-module(day8).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([parse/1, detect_loop/1, fix/1, mutate/1]).

%%% solution

solve_part1(Text) ->
    Instructions = parse(Text),
    {loops, Acc} = detect_loop(Instructions),
    Acc.

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

fix(_) ->
    8.

%% Creates a list of altenative instructions with exactly
%% one nop/jmp swapped to jmp/nop respectively
-spec mutate(Instructions) ->
    AlternativeInstructions when
      Instructions :: instructions(),
      AlternativeInstructions :: [instructions()].


mutate(Instructions) ->
    mutate(_Seen=[], _Unseen=Instructions, _Mutations=[]).

-spec mutate(Seen, Unseen, Mutations) ->
    FinalMutations when
      Seen :: instructions(),
      Unseen :: instructions(),
      Mutations :: [instructions()],

      FinalMutations :: [instructions()].

mutate(_Seen, _Unseen=[], Mutations) -> Mutations;
mutate(Seen, [Current={nop, Arg} | Tail], Mutations) ->
    Mutation = Seen ++ [{jmp, Arg} | Tail],
    mutate(Seen ++ [Current], Tail, [Mutation | Mutations]);
mutate(Seen, [Current={jmp, Arg} | Tail], Mutations) ->
    Mutation = Seen ++ [{nop, Arg} | Tail],
    mutate(Seen ++ [Current], Tail, [Mutation | Mutations]);
mutate(Seen, [Current | Tail], Mutations) ->
    mutate(Seen ++ [Current], Tail, Mutations).
