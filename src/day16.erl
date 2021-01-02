-module(day16).

-export([solve_part1/1, solve_part2/1]).
-export([parse/1, to_valid_values/1, filter/2, deduce/1, transpose/1,
         possible_fields/2, as_set/1, as_sets/1
        ]).

%%% solution

solve_part1(Input) ->
    solve(part1, parse(Input)).

solve_part2(Input) ->
    Deduction = solve(part2, parse(Input)),
    DepartureValues = [Value || {Label, Value} <- Deduction,
              lists:member(Label, [
                                   'departure location'
                                   ,'departure station'
                                   ,'departure platform'
                                   ,'departure track'
                                   ,'departure date'
                                   ,'departure time'
                                  ])
    ],
    lists:foldl(fun(Elem, Acc) -> Elem * Acc end, 1, DepartureValues).

%%% part 2

deduce({fields, Fields, your, Your, nearby, Nearby}) ->
    ValidValues = to_valid_values(Fields),
    ValidTickets = filter(Nearby, ValidValues),
    Columns = transpose(ValidTickets),
    Idx2Possible = lists:sort([{length(possible_fields(Column, Fields)),
                     possible_fields(Column, Fields),
                     Idx}
     || {Idx, Column} <- enumerate(Columns)
    ]),
    {Result, _UsedLabels} = lists:foldl(fun deduce_folder/2,
                                        {[], []}, Idx2Possible),
    Labels = [Label || {_Idx, Label} <- lists:sort(Result)],
    lists:zip(Labels, Your).

deduce_folder({_, PossibleValues, Idx}, {Result, UsedLabels}) ->
    [Only] = sets:to_list(sets:subtract(sets:from_list(PossibleValues),
                          sets:from_list(UsedLabels))),
    {[{Idx, Only} | Result], [Only | UsedLabels]}.
       

possible_fields(Column, Fields) ->
    FieldSets = as_sets(Fields),
    ColumnSet = sets:from_list(Column),
    [Key
     || Key <- maps:keys(FieldSets),
        sets:is_subset(ColumnSet, maps:get(Key, FieldSets))
    ].

filter(Tickets, ValidValues) ->
    [Ticket  || Ticket <- Tickets,
                 validate(Ticket, ValidValues)].
validate(Ticket, ValidValues) ->
    lists:all(fun(Val) -> lists:member(Val, ValidValues) end, Ticket).

as_sets(Fields) ->
    maps:map(fun(_Key, Ranges) -> as_set(Ranges) end, Fields).

as_set(Ranges) ->
    sets:from_list(
      lists:flatten(
        [lists:seq(From, To)
         || {From, To} <- Ranges]
       )).

%%% part 1

solve(part1, {fields, Fields, your, _Your, nearby, Nearby}) ->
    ValidValues = to_valid_values(Fields),
    lists:sum([Value
               || Value <- lists:flatten(Nearby),
                  not lists:member(Value, ValidValues)]);
solve(part2, ParsedInput) ->
    deduce(ParsedInput).


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

enumerate(List) ->
     lists:zip(lists:seq(1, length(List)), List).

%% @doc transpose a matrix
transpose(Matrix) ->
    transpose(Matrix, []).
transpose([], Result) ->
    [lists:reverse(Row) || Row <- Result];
transpose([Row | TailRows], Result) ->
    transpose(TailRows, add_column(_Column=Row, Result)).

add_column(Column, []) ->
    [[V] || V <- Column];
add_column(Column, Columns) ->
    [[V | Vs] || {V, Vs} <- lists:zip(Column, Columns)].
