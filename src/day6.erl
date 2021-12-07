-module(day6).
-behavior(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, p1/1, p1/2, p2/1]).

input_type() -> numbers.


p1(Ages) ->
    p1(Ages, 80).

p1(Ages, Days) ->
    descendants(build(Days), [Age - 9 || Age <- Ages]).


%% Map out how many children would be born to
%% a fish if that fish were born on each day.
%% 
%% By working backwards we get auto-memoization
%% through the data structure and we can compute
%% this in a single pass.
%% 
%% The ending day is funny because of 0/1 indexing.
build(End) ->
    build(#{}, End - 1, End - 1).

% Starting fish are born "before time" to avoid
% having to special-case them for the first
% round of child-bearing.
build(Map, -9, _End) ->
    Map;

% Fish born within the initial spawn time of
% the last day never have children, so they get 0.
build(Map, Day, End) when Day > End - 9 ->
    build(Map#{Day => 0}, Day - 1, End);

% Every other fish spawns a computable number of
% direct children, and we add the number of fish
% descendants that those children spawn, which we
% have thankfully already computed by now.
build(Map, Day, End) ->
    build(Map#{Day => descendants(Map, lists:seq(Day + 9, End, 7))}, Day - 1, End).

descendants(Map, SpawnedOn) ->
    length(SpawnedOn) + lists:sum([maps:get(Day, Map) || Day <- SpawnedOn]).

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
