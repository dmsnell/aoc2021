-module(day7).
-behaviour(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, p1/1, p2/1]).

input_type() -> numbers.

p1(Input) ->
    lists:min([
        lists:sum([abs(Crab - N) || Crab <- Input])
        ||
        N <- unique(Input)
    ]).


-ifdef(EUNIT).

p1_test() ->
    ?assertEqual(37, p1(example())).

-endif.

p2(Input) ->
    lists:min([
        lists:sum([fuel(C, N) || C <- Input])
        ||
        N <- unique(Input)
    ]).


fuel(A, B) ->
    Delta = abs(B - A),
    round((1 + Delta) / 2 * Delta).

-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(168, p2(example())).

-endif.

%%% Helpers

example() -> [16,1,2,0,4,2,7,1,2,14].

unique(L) ->
    sets:to_list(sets:from_list(L)).