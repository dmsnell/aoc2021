Advent of Code 2021 Solutions
=====

Running the solutions
-----

To run a particular solution call `aoc:solve/2` with the day
and the part specified. The day is an atom like `day1` and the
part is either `p1` for part one or `p2` for part two.

The output from each solution identifies which day and part ran,
the output for that problem, and the time it took to run.

To run all the solutions call `aoc:solve_all()`.

Start by cloning the repo and running `rebar3 shell` for a REPL.

```erlang
aoc:solve_all() = [
    {day1,p1,1688,{0.028,ms}},
    {day1,p2,1728,{0.057,ms}},
    {day2,p1,1714680,{0.025,ms}},
    {day2,p2,1963088820,{0.027,ms}},
    {day3,p1,3895776,{0.394,ms}},
    {day3,p2,7928162,{1.080,ms}},
    {day4,p1,5685,{3.030,ms}},
    {day4,p2,21070,{5.172,ms}},
    {day5,p1,5294,{79.632,ms}},
    {day5,p2,21698,{150.635,ms}},
    {day6,p1,358214,{0.125,ms}},
    {day6,p2,1622533344325,{0.330,ms}},
    {day7,p1,364898,{35.685,ms}},
    {day7,p2,104149091.0,{68.945,ms}}
].

aoc:solve(day1, p1) = {day1,p1,1688,{0.184,ms}}.

{Day, Part, Answer, Runtime} = aoc:solve(day1, p1),
Day = day1,
Part = p1,
Answer = 1688,
Runtime = {0.184,ms}.
```

To develop with an open REPL call `r3:compile()` from within
the REPL to reload code with updates from your editor.

```erlang
% Reload code and re-run all solutions
r3:compile(), aoc:solve_all().
```