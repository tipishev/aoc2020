-module(day16).

-export([solve_part1/1, solve_part2/1]).
-export([parse/1]).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

%%% parse
parse(Input) ->
    [Fields, Your, Nearby] = string:split(Input, "\n\n", all),
    {fields, parse(fields, Fields),
     your, parse(your, Your),
     nearby, parse(nearby, Nearby)}.

parse(fields, Fields) ->
    maps:from_list([parse(field, Field)
     || Field <- string:lexemes(Fields, "\n")]);
parse(field, Field) ->
    [Name, Ranges] = string:lexemes(Field, ":"),
    RangesList = [parse(range, Range)
                  || Range <- string:lexemes(Ranges, " or")],
    % witness me, I don't fear creating atoms >:(P)
    {list_to_atom(Name), RangesList};
parse(range, Range) ->
    list_to_tuple(lists:map(fun list_to_integer/1,
                            string:lexemes(Range, "-")));
parse(your, Your) ->
    [_Label, CsvInts] = string:lexemes(Your, "\n"),
    parse(csv_ints, CsvInts);
parse(csv_ints, CsvInts) ->
    [list_to_integer(S) || S <- string:lexemes(CsvInts, ",")];
parse(nearby, Nearby) ->
    [_Label | CsvIntsList] = string:lexemes(Nearby, "\n"),
    [parse(csv_ints, CsvInts) || CsvInts <- CsvIntsList].
