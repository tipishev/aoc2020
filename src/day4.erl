-module(day4).

-export([solve_part1/1, solve_part2/1]).
-export([parse/1, validate/1]).

% LYSE suggests to keep records private to modules, no header files
-record(passport, {byr, iyr, eyr, hgt, hcl, ecl, pid, cid}).

%%% solution

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

%%% internals

parse(InputData) ->
    % InputData.
    Csv = re:replace(InputData, "\n\n", ",", [global, {return, list}]),
    WithoutNewlines = re:replace(Csv, "\n", " ", [global, {return, list}]),
    Lines = string:lexemes(WithoutNewlines, ","),
    lists:map(fun line_to_passport/1, Lines).

line_to_passport(Line) ->
    ColonSeps = string:lexemes(Line, " "),
    Tuples = lists:map(fun colon_sep_to_tuple/1, ColonSeps),
    tuples_to_passport(Tuples).

colon_sep_to_tuple(Lexeme) ->
    list_to_tuple(string:split(Lexeme, ":")).

tuples_to_passport(Tuples) ->
    tuples_to_passport(Tuples, #passport{}).

tuples_to_passport([], Passport) -> Passport;
tuples_to_passport([{Key, Value} | Tail], Passport) ->
    NewPassport = case list_to_existing_atom(Key) of
        byr -> Passport#passport{byr=Value};
        iyr -> Passport#passport{iyr=Value};
        eyr -> Passport#passport{eyr=Value};
        hgt -> Passport#passport{hgt=Value};
        hcl -> Passport#passport{hcl=Value};
        ecl -> Passport#passport{ecl=Value};
        pid -> Passport#passport{pid=Value};
        cid -> Passport#passport{cid=Value}
    end,
    tuples_to_passport(Tail, NewPassport).

%%% part1

validate(#passport{byr=Byr, iyr=Iyr, eyr=Eyr, hgt=Hgt,
                   hcl=Hcl, ecl=Ecl, pid=Pid, cid=_})  when Byr =/= undefined,
                                                            Iyr =/= undefined,
                                                            Eyr =/= undefined,
                                                            Hgt =/= undefined,
                                                            Hcl =/= undefined,
                                                            Ecl =/= undefined,
                                                            Pid =/= undefined ->
    true;
validate(_) -> false.
