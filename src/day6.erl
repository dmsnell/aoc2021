-module(day6).
-behavior(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, p1/1, p1/2, p2/1]).

input_type() -> numbers.


p1(Ages) ->
    p1(Ages, 80).

p1(Ages, Days) ->
    Map = build(Days),
    length(Ages) + lists:sum([maps:get(On - 9, Map) || On <- Ages]).


build(End) ->
    build(#{}, End - 1, End - 1).

build(Map, -9, _End) -> Map;
build(Map, Day, End) when Day > End - 9 ->
    build(Map#{Day => 0}, Day - 1, End);
build(Map, Day, End) ->
    SpawnOn = lists:seq(Day + 9, End, 7),
    build(
        Map#{Day => length(SpawnOn) + lists:sum([maps:get(On, Map) || On <- SpawnOn])},
        Day - 1,
        End
    ).


-ifdef(EUNIT).

p1_test() ->
    ?assertEqual(5934, p1(example())).

-endif.

p2(Ages) ->
    p1(Ages, 256).


-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(26984457539, p2(example())).

-endif.

%%% Helpers

example() -> [3,4,3,1,2].
