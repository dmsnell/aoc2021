-module(day4).
-behaviour(aoc).

-include_lib("eunit/include/eunit.hrl").

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input([DrawData | BoardData]) ->
    Draws = parse_draws(DrawData),
    Boards = parse_boards(BoardData),
    {Draws, Boards}.


%%% @doc
%% You're already almost 1.5km (almost a mile) below the surface of the ocean,
%% already so deep that you can't see any sunlight. What you can see, however, is a
%% giant squid that has attached itself to the outside of your submarine.
%% 
%% Maybe it wants to play bingo?
%% 
%% Bingo is played on a set of boards each consisting of a 5x5 grid of numbers.
%% Numbers are chosen at random, and the chosen number is marked on all boards on
%% which it appears. (Numbers may not appear on all boards.) If all numbers in any
%% row or any column of a board are marked, that board wins. (Diagonals don't
%% count.)
%% 
%% The submarine has a bingo subsystem to help passengers (currently, you and the
%% giant squid) pass the time. It automatically generates a random order in which
%% to draw numbers and a random set of boards (your puzzle input). For example:
%% 
%%     7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
%% 
%%     22 13 17 11  0
%%     8  2 23  4 24
%%     21  9 14 16  7
%%     6 10  3 18  5
%%     1 12 20 15 19
%% 
%%     3 15  0  2 22
%%     9 18 13 17  5
%%     19  8  7 25 23
%%     20 11 10 24  4
%%     14 21 16 12  6
%% 
%%     14 21 17 24  4
%%     10 16 15  9 19
%%     18  8 23 26 20
%%     22 11 13  6  5
%%     2  0 12  3  7
%% 
%% After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no
%% winners, but the boards are marked as follows (shown here adjacent to each
%% other to save space):
%% 
%%     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%     8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%%     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%     6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%     1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
%% 
%% After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still
%% no winners:
%% 
%%     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%     8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%%     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%     6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%     1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
%% 
%% Finally, 24 is drawn:
%% 
%%     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%     8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%%     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%     6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%     1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
%% 
%% At this point, the third board wins because it has at least one complete row or
%% column of marked numbers (in this case, the entire top row is marked: 14 21 17
%% 24 4).
%% 
%% The score of the winning board can now be calculated. Start by finding the sum
%% of all unmarked numbers on that board; in this case, the sum is 188. Then,
%% multiply that sum by the number that was just called when the board won, 24, to
%% get the final score, 188 * 24 = 4512.
%% 
%% To guarantee victory against the giant squid, figure out which board will win
%% first. What will your final score be if you choose that board?
p1({Draws, StartingBoards}) ->
    {LastTurn, {winner, WinningBoard}} = lists:foldl(
        % Passing the winner through to skip processing.
        fun (_Draw, {Turn, {winner, _Winner} = Winner}) ->
            {Turn, Winner}
        % Compute next round and look for a winner.
        ;   (Draw, {Turn, Boards}) -> 
            Next = [call(Turn, Draw, Board) || Board <- Boards],
            case lists:filter(fun (#{wins := Winners}) -> length(Winners) > 0 end, Next) of
                % Assume for p1 that we will only get a single winning
                % board on the turn where we get our first winner
                [Winner] -> {Turn, {winner, Winner}};
                []       -> {Turn + 1, Next}
            end
        end,
        {1, StartingBoards},
        Draws
    ),
    tally(lists:sublist(Draws, LastTurn), WinningBoard).


%% @doc Given a list of draws and a board compute the "score."
tally(Draws, #{play := Play, turn := LastTurn}) ->
    Unplayed = maps:keys(maps:filter(fun (N, _) -> not lists:member(N, Draws) end, Play)),
    [Draw] = maps:keys(maps:filter(fun (_, Turn) -> Turn == LastTurn end, Play)),
    Draw * lists:sum(Unplayed).


% Board stays the same if it doesn't have the currently-called draw.
call(_Turn, N, #{play := Play} = Board) when not is_map_key(N, Play) ->
    Board;
% If we've already won, don't update the board either.
call(_Turn, _N, #{wins := [_|_]} = Board) ->
    Board;
% If we haven't won yet though and have a match, mark the draw and check for wins.
call(Turn, N, #{grid := Grid, play := PlaySoFar} = Board) when is_map_key(N, PlaySoFar) ->
    [{Row, Col}] = maps:keys(maps:filter(fun (_, M) -> M == N end, Grid)),
    Play = PlaySoFar#{ N => Turn },
    Board#{
        play => Play,
        turn => Turn,
        wins => row_win(Grid, Row, Play) ++ col_win(Grid, Col, Play)
    }.


row_win(Grid, RowN, Play) ->
    Row = maps:filter(fun ({R, _C}, N) -> R == RowN andalso maps:get(N, Play, 0) > 0 end, Grid),
    case map_size(Row) of
        5 -> [{row, RowN, maps:values(Row)}];
        _ -> []
    end.

col_win(Grid, ColN, Play) ->
    Col = maps:filter(fun ({_R, C}, N) -> C == ColN andalso maps:get(N, Play, 0) > 0 end, Grid),
    case map_size(Col) of
        5 -> [{col, ColN, maps:values(Col)}];
        _ -> []
    end.


-ifdef(EUNIT).

p1_test() ->
    ?assertEqual(4512, p1(parse_input(example_raw()))).

-endif.

%% @doc
%% On the other hand, it might be wise to try a different strategy: let the giant squid win.
%% 
%% You aren't sure how many bingo boards a giant squid could play at once, so
%% rather than waste time counting its arms, the safe thing to do is to figure out
%% which board will win last and choose that one. That way, no matter which boards
%% it picks, it will win for sure.
%% 
%% In the above example, the second board is the last to win, which happens after
%% 13 is eventually called and its middle column is completely marked. If you were
%% to keep playing until this point, the second board would have a sum of unmarked
%% numbers equal to 148 for a final score of 148 * 13 = 1924.
%% 
%% Figure out which board will win last. Once it wins, what would its final score be?
p2({Draws, StartingBoards}) ->
    % Play out all draws to find the last state of every board
    {_, FinalBoards} = lists:foldl(
        fun (Draw, {Turn, Boards}) -> {Turn + 1, [call(Turn, Draw, Board) || Board <- Boards]} end,
        {1, StartingBoards},
        Draws
    ),
    % Find whichever board finished last.
    #{turn := MaxTurn} = LastWinner = lists:foldl(
        fun (#{wins := []}, PrevWinner)                                             -> PrevWinner
        ;   (#{turn := Turn}, #{turn := MaxTurn} = PrevWinner) when Turn =< MaxTurn -> PrevWinner
        ;   (#{turn := Turn} = NextWinner, #{turn := MaxTurn}) when Turn  > MaxTurn -> NextWinner
        end,
        #{turn => 0},
        FinalBoards
    ),
    tally(lists:sublist(Draws, MaxTurn), LastWinner).

-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(1924, p2(parse_input(example_raw()))).

-endif.


%%% Parsers

parse_draws(DrawData) ->
    [binary_to_integer(N) || N <- binary:split(DrawData, <<",">>, [global])].

parse_boards(BoardData) ->
    parse_boards(BoardData, []).

parse_boards([<<>>, A, B, C, D, E | BoardData], Boards) ->
    parse_boards(BoardData, [parse_board(A, B, C, D, E) | Boards]);
parse_boards(_NoMatchingBoardData, Boards) ->
    lists:reverse(Boards).

parse_board(A, B, C, D, E) ->
    Rows = [
        [binary_to_integer(N) || N <- binary:split(Row, <<" ">>, [global, trim_all])]
        ||
        Row <- [A, B, C, D, E]
    ],
    Grid = [
        [{{RowN, ColN}, N} 
        || {ColN, N} <- lists:zip(lists:seq(1, length(Row)), Row)]
        || {RowN, Row} <- lists:zip(lists:seq(1, length(Rows)), Rows)
    ],
    #{
        grid => maps:from_list(lists:flatten(Grid)),
        play => maps:from_keys(lists:flatten(Rows), 0),
        turn => 1,
        wins => []
    }.


%%% Helpers


example_raw() -> binary:split(list_to_binary("""7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""), <<"\n">>, [global]).