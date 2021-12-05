-module(day5).
-behaviour(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Input) ->
    [make_line(parse_line(Line)) || Line <- Input].


p1(AllLines) ->
    Lines = ortho_lines(AllLines),
    Stack = stack_lines(Lines),
    map_size(maps:filter(fun (_, Count) -> Count > 1 end, Stack)).


-ifdef(EUNIT).

p1_test() ->
    ?assertEqual(5, p1(example())).

-endif.

p2(Lines) ->
    Stack = stack_lines(Lines),
    map_size(maps:filter(fun (_, Count) -> Count > 1 end, Stack)).

-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(12, p2(example())).

-endif.

%%% Helpers

stack_lines(Lines) ->
    lists:foldl(
        fun (#{path := Path}, Stack) ->
            lists:foldl(
                fun (Point, InnerStack) -> maps:update_with(Point, fun inc/1, 1, InnerStack) end,
                Stack,
                Path
            )
        end,
        #{},
        Lines
    ).


ortho_lines(Lines) ->
    [
        Line
        ||
        #{ from := {X1, Y1}, to := {X2, Y2} } = Line <- Lines, 
        X1 == X2 orelse Y1 == Y2
    ].

make_line({From, To}) ->
    #{
        from => From,
        to   => To,
        path => line_points(From, To)
    }.


line_points({X1, Y1}, {X2, Y2}) ->
    case {abs(X2 - X1), abs(Y2 - Y1)} of
        {DX, DY} when DX >= DY -> line_points(horizontal, {X1, Y1}, {X2, Y2});
        {DX, DY} when DX  < DY -> line_points(vertical, {X1, Y1}, {X2, Y2})
    end.

line_points(horizontal, {X1, Y1}, {X2, Y2}) ->
    Slope = float(Y2 - Y1) / float(X2 - X1),
    [{X, Y1 + round(Slope * (X - X1))} || X <- lists:seq(X1, X2, sign(X2 - X1))];

line_points(vertical, {X1, Y1}, {X2, Y2}) ->
    Slope = float(X2 - X1) / float(Y2 - Y1),
    [{X1 + round(Slope * (Y - Y1)), Y} || Y <- lists:seq(Y1, Y2, sign(Y2 - Y1))].


inc(V) -> V + 1.


sign(V) when V  > 0 ->  1;
sign(V) when V  < 0 -> -1;
sign(V) when V == 0 ->  0.

%%% Parsers

parse_line(Line) ->
    [From, To] = binary:split(Line, <<" -> ">>),
    {parse_coords(From), parse_coords(To)}.


parse_coords(Coords) ->
    [X, Y] = binary:split(Coords, <<",">>),
    {binary_to_integer(X), binary_to_integer(Y)}.

%%% Helpers

example() -> parse_input(binary:split(list_to_binary(
    """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""), <<"\n">>, [global])).