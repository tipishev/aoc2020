-module(day4_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PASSPORTS,
"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in").

-define(INVALID_PASSPORTS, 
"eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007").

-define(VALID_PASSPORTS,
"pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
).

parse_test_() ->
    [

     {"Simple parse",
      ?_assertEqual(
         [
          {passport,"1937","2017","2020","183cm","#fffffd","gry",
           "860033327","147"},
          {passport,"1929","2013","2023",undefined,"#cfa07d","amb",
           "028048884","350"},
          {passport,"1931","2013","2024","179cm","#ae17e1","brn",
           "760753108",undefined},
          {passport,undefined,"2011","2025","59in","#cfa07d","brn",
           "166559648",undefined}
         ]
         , day4:parse(?PASSPORTS))}
    ].

parse_height_test_() ->
    [

     {"Centimeters",
      ?_assertEqual({cm, 123} , day4:parse_height("123cm"))},

     {"Inches",
      ?_assertEqual({in, 69} , day4:parse_height("69in"))},

     {"Not a height",
      ?_assertEqual(not_a_height , day4:parse_height("123"))},

     {"Not a height without number",
      ?_assertEqual(not_a_height , day4:parse_height("cm"))}

    ].

validate_test_() ->
    [

     {"The first passport is valid - all eight fields are present.",
      ?_assert(day4:validate(
                 {passport,"1937","2017","2020","183cm",
                  "#fffffd","gry", "860033327","147"}))},

     {"The second passport is invalid - it is missing hgt (the Height field).",
      ?_assertNot(day4:validate(
                 {passport,"1929","2013","2023",undefined,
                  "#cfa07d","amb", "028048884","350"}))},

     {"The third passport is interesting; the only missing field is cid,
        so it looks like data from North Pole Credentials,
        not a passport at all! Surely, nobody would mind if you 
        made the system temporarily ignore missing cid fields.
        Treat this 'passport' as valid.",
      ?_assert(day4:validate(
                 {passport,"1931","2013","2024","179cm",
                  "#ae17e1","brn","760753108",undefined}))},

     {"The fourth passport is missing two fields, cid and byr.
        Missing cid is fine, but missing any other field is not,
        so this passport is invalid.",
      ?_assertNot(day4:validate({passport,undefined,"2011","2025","59in","#cfa07d","brn",
                                "166559648",undefined}))}
    ].

solve_part2_test_() ->
    [

     {"Invalid Passports",
      ?_assertEqual(0, day4:solve_part2(?INVALID_PASSPORTS))},

     {"Valid Passports",
      ?_assertEqual(4, day4:solve_part2(?VALID_PASSPORTS))}

    ].
