-module(day7).
-behaviour(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, p1/1, p2/1]).

input_type() -> numbers.

p1(Input) ->
    Min = lists:min(Input),
    Max = lists:max(Input),
    lists:min([
        lists:sum([abs(Crab - N) || Crab <- Input])
        ||
        N <- lists:seq(Min, Max)
    ]).


-ifdef(EUNIT).

p1_test() ->
    ?assertEqual(37, p1(example())).

-endif.

p2(Input) ->
    Min = lists:min(Input),
    Max = lists:max(Input),
    lists:min([
        lists:sum([fuel(C, N) || C <- Input])
        ||
        N <- lists:seq(Min, Max)
    ]).


fuel(A, B) ->
    (1 + abs(B - A)) / 2 * abs(B - A).

-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(168, p1(example())).

-endif.

%%% Helpers

example() -> [16,1,2,0,4,2,7,1,2,14].
