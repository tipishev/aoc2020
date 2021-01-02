-module(day16).

-export([solve_part1/1, solve_part2/1]).
-export([parse/1, to_valid_values/1, filter/2, deduce/1, transpose/1]).

%%% solution

solve_part1(Input) ->
    solve(part1, parse(Input)).

solve_part2(_Input) ->
    undefined.

%%% part 2

deduce(_) ->
    todo.

filter(Tickets, ValidValues) ->
    [Ticket  || Ticket <- Tickets,
                 validate(Ticket, ValidValues)].
validate(Ticket, ValidValues) ->
    lists:all(fun(Val) -> lists:member(Val, ValidValues) end, Ticket).

%%% part 1

solve(part1, {fields, Fields, your, _Your, nearby, Nearby}) ->
    ValidValues = to_valid_values(Fields),
    lists:sum([Value
               || Value <- lists:flatten(Nearby),
                  not lists:member(Value, ValidValues)]).


to_valid_values(Fields) ->
    FlatRanges = lists:flatten(maps:values(Fields)),
    lists:sort(
      deduplicate(
        lists:flatten(
          [lists:seq(From, To) || {From, To} <- FlatRanges]
         )
       )
     ).

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

%%% herlpers
deduplicate(List) ->
    sets:to_list(sets:from_list(List)).

transpose(Matrix) ->
    transpose(Matrix, []).

%% @doc transpose a matrix
transpose([], Result) ->
    [lists:reverse(Row) || Row <- Result];
transpose([Row | TailRows], Result) ->
    transpose(TailRows, add_column(_Column=Row, Result)).

add_column(Column, []) ->
    [[V] || V <- Column];
add_column(Column, Columns) ->
    [[V | Vs] || {V, Vs} <- lists:zip(Column, Columns)].
