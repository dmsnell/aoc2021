-module(day2).
-behavior(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Lines) ->
    lists:map(fun parse_step/1, Lines).


%% @doc
%% Dive!
%% 
%% Now, you need to figure out how to pilot this thing.
%% 
%% It seems like the submarine can take a series
%% of commands like forward 1, down 2, or up 3:
%% 
%%     - forward X increases the horizontal position by X units.
%%     - down X increases the depth by X units.
%%     - up X decreases the depth by X units.
%% 
%% Note that since you're on a submarine, down and up affect
%% your depth, and so they have the opposite result of what
%% you might expect.
%% 
%% The submarine seems to already have a planned course
%% (your puzzle input). You should probably figure out
%% where it's going. For example:
%% 
%%    forward 5
%%    down 5
%%    forward 8
%%    up 3
%%    down 8
%%    forward 2
%% 
%% Your horizontal position and depth both start at 0.
%% The steps above would then modify them as follows:
%% 
%%    - forward 5 adds 5 to your horizontal position, a total of 5.
%%    - down 5 adds 5 to your depth, resulting in a value of 5.
%%    - forward 8 adds 8 to your horizontal position, a total of 13.
%%    - up 3 decreases your depth by 3, resulting in a value of 2.
%%    - down 8 adds 8 to your depth, resulting in a value of 10.
%%    - forward 2 adds 2 to your horizontal position, a total of 15.
%% 
%% After following these instructions, you would have a
%% horizontal position of 15 and a depth of 10.
%% (Multiplying these together produces 150.)
%% 
%% Calculate the horizontal position and depth you would have
%% after following the planned course. What do you get if you
%% multiply your final horizontal position by your final depth?
p1(Steps) ->
    {Position, Depth} = lists:foldl(fun run/2, {0, 0}, Steps),
    Position * Depth.


run({forward, S}, {P, D}) -> {P + S, D};
run({up,      S}, {P, D}) -> {P, D - S};
run({down,    S}, {P, D}) -> {P, D + S}.


-ifdef(EUNIT).

example() -> [
    {forward, 5},
    {down,    5},
    {forward, 8},
    {up,      3},
    {down,    8},
    {forward, 2}
].

p1_test() ->
    ?assertEqual(150, p1(example())).

-endif.


%% @doc
%% Based on your calculations, the planned course doesn't seem to
%% make any sense. You find the submarine manual and discover
%% that the process is actually slightly more complicated.
%% 
%% In addition to horizontal position and depth, you'll also need to track a
%% third value, aim, which also starts at 0. The commands also mean something
%% entirely different than you first thought:
%% 
%%     - down X increases your aim by X units.
%%     - up X decreases your aim by X units.
%%     - forward X does two things:
%%         - It increases your horizontal position by X units.
%%         - It increases your depth by your aim multiplied by X.
%% 
%% Again note that since you're on a submarine, down and up do the opposite
%% of what you might expect: "down" means aiming in the positive direction.
%% 
%% Now, the above example does something different:
%% 
%%     - forward 5 adds 5 to your horizontal position, a total of 5.
%%       Because your aim is 0, your depth does not change.
%%     - down 5 adds 5 to your aim, resulting in a value of 5.
%%     - forward 8 adds 8 to your horizontal position, a total of 13.
%%       Because your aim is 5, your depth increases by 8*5=40.
%%     - up 3 decreases your aim by 3, resulting in a value of 2.
%%     - down 8 adds 8 to your aim, resulting in a value of 10.
%%     - forward 2 adds 2 to your horizontal position, a total of 15.
%%       Because your aim is 10, your depth increases by 2*10=20 to a total of 60.
%% 
%% After following these new instructions, you would have a horizontal
%% position of 15 and a depth of 60. (Multiplying these produces 900.)
%% 
%% Using this new interpretation of the commands, calculate the horizontal
%% position and depth you would have after following the planned course.
%% What do you get if you multiply your final horizontal position by your final depth?
p2(Steps) ->
    {Position, Depth, _Aim} = lists:foldl(fun run2/2, {0, 0, 0}, Steps),
    Position * Depth.


run2({forward, S}, {P, D, A}) -> {P + S,  D + S * A,  A    };
run2({up,      S}, {P, D, A}) -> {P,              D,  A - S};
run2({down,    S}, {P, D, A}) -> {P,              D,  A + S}.


-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(900, p2(example())).

-endif.

%%% Helpers

parse_step(<<"forward ", Scale/binary>>) -> {forward, binary_to_integer(Scale)};
parse_step(<<"up ",      Scale/binary>>) -> {up,      binary_to_integer(Scale)};
parse_step(<<"down ",    Scale/binary>>) -> {down,    binary_to_integer(Scale)}.
