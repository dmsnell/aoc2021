-module(day7).
-behaviour(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, p1/1, p2/1]).

input_type() -> numbers.

p1(Input) ->
    {Min, Max} = extrema(Input),
    lists:min([
        lists:sum([abs(Crab - N) || Crab <- Input])
        ||
        N <- lists:seq(Min, Max)
    ]).


fuel_until(Xs, Max, Cost) ->
    fuel_until(Xs, Max, Cost, 0).

fuel_until(_Xs, Max, _Cost, Fuel) when Fuel > Max ->
    Max + 1;
fuel_until([], Max, _Cost, Fuel) ->
    min(Max, Fuel);
fuel_Until([X | Xs], Max, Cost, Fuel) ->
    fuel_until(Xs, Max, Cost, Fuel + Cost(X)).


-ifdef(EUNIT).

p1_test() ->
    ?assertEqual(37, p1(example())).

-endif.

p2(Input) ->
    {Min, Max} = extrema(Input),
    lists:min([
        lists:sum([fuel(C, N) || C <- Input])
        ||
        N <- lists:seq(Min, Max)
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


extrema([First | List]) ->
    extrema(List, First, First).

extrema([], Max, Min) -> {Min, Max};
extrema([A | List], Max, Min) when A > Max ->
    extrema(List, A, Min);
extrema([A | List], Max, Min) when A < Min ->
    extrema(List, Max, A);
extrema([_ | List], Max, Min) ->
    extrema(List, Max, Min).
