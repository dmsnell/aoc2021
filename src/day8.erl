-module(day8).
-behaviour(aoc).

-include_lib("eunit/include/eunit.hrl").
-import(utils, [split_by/2]).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Lines) ->
    lists:map(fun parse_line/1, Lines).


p1(Input) ->
    lists:sum([
        lists:sum([1 || #{count := Count} <- Output, is_unique(Count)])
        || #{output := Output} <- Input
    ]).


is_unique(2) -> true;
is_unique(3) -> true;
is_unique(4) -> true;
is_unique(7) -> true;
is_unique(_) -> false.


-ifdef(EUNIT).

p1_test() ->
    ?assertEqual(26, p1(example())).

-endif.


p2(Input) ->
    lists:sum([
        begin
            Segments = infer(Line),
            list_to_integer(lists:flatten([integer_to_list(to_digit(Segments, Digit)) || #{segments := Digit} <- Output]))
        end
        ||
        #{output := Output} = Line <- Input
    ]).


to_digit(#{} = Segments, Digits) ->
    to_digit(output, maps:from_keys([maps:get(Digit, Segments) || Digit <- maps:keys(Digits)], on));

to_digit(output, #{a := on, b := on, c := on, d := on, e := on, f := on, g := on}) -> 8;
to_digit(output, #{a := on, b := on, c := on, d := on, f := on, g := on}) -> 9;
to_digit(output, #{a := on, b := on, d := on, e := on, f := on, g := on}) -> 6;
to_digit(output, #{a := on, b := on, c := on, e := on, f := on, g := on}) -> 0;
to_digit(output, #{a := on, b := on, d := on, f := on, g := on}) -> 5;
to_digit(output, #{a := on, c := on, d := on, e := on, g := on}) -> 2;
to_digit(output, #{a := on, c := on, d := on, f := on, g := on}) -> 3;
to_digit(output, #{b := on, c := on, d := on, f := on}) -> 4;
to_digit(output, #{a := on, c := on, f := on}) -> 7;
to_digit(output, #{c := on, f := on}) -> 1.


infer(#{digits := Digits, output := _Output}) ->
    [One] = counting(Digits, 2),
    [Four] = counting(Digits, 4),
    [Seven] = counting(Digits, 3),
    [Eight] = counting(Digits, 7),
    Fives = counting(Digits, 5),
    A = segment_a(One, Seven),
    {D, G} = segments_dg(A, Four, Fives),
    B = segment_b(One, Four, D),
    F = segment_f(maps:from_keys([A, B, D, G], on), Fives),
    C = segment_c(maps:from_keys([A, B, D, F, G], on), Fives),
    E = segment_e([A, B, C, D, F, G], Eight),
    #{A => a, B => b, C => c, D => d, E => e, F => f, G => g}.


counting(Digits, N) ->
    lists:filter(fun (#{count := Count}) -> Count == N end, Digits).


segment_a(#{segments := One}, #{segments := Seven}) ->
    [Segment] = maps:keys(maps:without(maps:keys(One), Seven)),
    Segment.


segment_b(#{segments := One}, #{segments := Four}, D) ->
    BD = maps:without(maps:keys(One), Four),
    [B] = maps:keys(maps:without([D], BD)),
    B.


segment_c(ABDFG, Fives) ->
    [#{segments := Three}] = lists:filter(fun (#{segments := S}) -> map_size(maps:intersect(ABDFG, S)) == 4 end, Fives),
    [C] = maps:keys(maps:without(maps:keys(ABDFG), Three)),
    C.


segment_e(ABCDFG, #{segments := Eight}) ->
    [E] = maps:keys(maps:without(ABCDFG, Eight)),
    E.


segments_dg(SegA, #{segments := Four}, [#{segments := A}, #{segments := B}, #{segments := C}]) ->
    DG = maps:without([SegA], maps:intersect(A, maps:intersect(B, C))),
    [D] = maps:keys(maps:intersect(Four, DG)),
    [G] = maps:keys(maps:without([D], DG)),
    {D, G}.


segment_f(ABDG, Fives) ->
    [#{segments := Five}] = lists:filter(fun (#{segments := S}) -> map_size(maps:intersect(ABDG, S)) == 4 end, Fives),
    [F] = maps:keys(maps:without(maps:keys(ABDG), Five)),
    F.


-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(61229, p2(example())).

-endif.

%%% Parsers

parse_line(Line) ->
    [SignalPattern, OutputValue] = split_by(Line, <<" | ">>),
    Digits = split_by(SignalPattern, <<" ">>),
    OutputDigits = split_by(OutputValue, <<" ">>),
    #{
        digits => lists:map(fun parse_digit/1, Digits),
        output => lists:map(fun parse_digit/1, OutputDigits)
    }.


parse_digit(Segments) ->
    Count = byte_size(Segments),
    SegmentMap = maps:from_keys([segment_name(S) || S <- binary_to_list(Segments)], on),
    #{
        segments => SegmentMap,
        count    => Count
    }.

segment_name($a) -> a;
segment_name($b) -> b;
segment_name($c) -> c;
segment_name($d) -> d;
segment_name($e) -> e;
segment_name($f) -> f;
segment_name($g) -> g.


%%% Example data


example() ->
    parse_input(split_by(example_input(), <<"\n">>)).


example_input() -> list_to_binary("""be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""").