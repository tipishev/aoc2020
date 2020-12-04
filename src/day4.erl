-module(day4).

-export([solve_part1/1, solve_part2/1]).

% for tests
-export([parse/1, validate/1, parse_height/1, validate2/1]).

% LYSE suggests to keep records private to modules, no header files
-record(passport, {byr, iyr, eyr, hgt, hcl, ecl, pid, cid}).

%%% solution

solve_part1(Input) ->
    Passports = parse(Input),
    length(lists:filter(fun validate/1, Passports)).

solve_part2(Input) ->
    Passports = parse(Input),
    ValidPassports = lists:filter(fun validate2/1, Passports),
    length(ValidPassports).

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

%%% part2

validate2(#passport{byr=Byr, iyr=Iyr, eyr=Eyr, hgt=Hgt,
                    hcl=Hcl, ecl=Ecl, pid=Pid, cid=_})  when Byr =/= undefined,
                                                             Iyr =/= undefined,
                                                             Eyr =/= undefined,
                                                             Hgt =/= undefined,
                                                             Hcl =/= undefined,
                                                             Ecl =/= undefined,
                                                             Pid =/= undefined ->

    % prepare data
    BirthYear = list_to_integer(Byr),
    IssueYear = list_to_integer(Iyr),
    ExpirationYear = list_to_integer(Eyr),
    Height = parse_height(Hgt),

    Validations = [
                   validate_one(within(BirthYear, 1920, 2002), wrong_birth_year),
                   validate_one(within(IssueYear, 2010, 2020), wrong_issue_year),
                   validate_one(within(ExpirationYear, 2020, 2030), wrong_expiration_year),
                   validate_one(validate_height(Height), wrong_height),
                   validate_one(validate_hair_color(Hcl), wrong_hair_color),
                   validate_one(validate_eye_color(Ecl), wrong_eye_color),
                   validate_one(validate_passport_id(Pid), wrong_passport_id)
                  ],
    % io:format("~p~n", [Validations]),  % for debugging
    lists:all(fun(IsValid) -> IsValid =:= true end, Validations);
validate2(_) ->
    false.

validate_one(ValidationResult, Error) ->
    case ValidationResult of
        true -> true;
        false -> Error
    end.

within(Number, Min, Max) when Number >= Min, Number =< Max -> true;
within(_, _, _) -> false.

validate_eye_color(EyeColor) ->
    lists:member(EyeColor, ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]).

validate_passport_id(Pid) when length(Pid) =:= 9 ->
    ValidChars = sets:from_list("0123456789"),
    GivenChars = sets:from_list(Pid),
    sets:is_subset(GivenChars, ValidChars);
validate_passport_id(_) -> false.

validate_hair_color([$# | MaybeHex]) when length(MaybeHex) =:= 6 ->
    ValidChars = sets:from_list("0123456789abcdef"),
    GivenChars = sets:from_list(MaybeHex),
    sets:is_subset(GivenChars, ValidChars);
validate_hair_color(_) -> false.

validate_height({cm, HeightValue}) -> within(HeightValue, 150, 193);
validate_height({in, HeightValue}) -> within(HeightValue, 59, 76);
validate_height(_) -> false.

parse_height(HeightStr) when is_list(HeightStr), length(HeightStr) > 3 ->
    Last2Chars = string:sub_string(HeightStr, length(HeightStr) - 1),
    case Last2Chars of
        "cm" ->
            {cm, extract_number(HeightStr)};
        "in" ->
            {in, extract_number(HeightStr)};
        _ -> not_a_height
    end;
parse_height(_) -> not_a_height.

extract_number(HeightStr) ->
    list_to_integer(string:sub_string(HeightStr, 1, length(HeightStr) - 2)).
