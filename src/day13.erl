-module(day13).
-moduledoc """
Started at 10:00pm
Part 1 solved at 11:16pm (piddling around).
Part 2 solved at 4:17am (first intuition, line intercept, was right, and I should have stuck with it).
Solved in 6h 18m. Yikes.

@author Dennis Snell <dmsnell@xkq.io>
@copyright (C) 2024, Dennis Snell <dmsnell@xkq.io>
Created : 12. Dec 2024 8:02 PM
""".
-author("Dennis Snell <dmsnell@xkq.io>").

%% API
-export([config/0]).

config() -> #{
    p1 => {fun p1_submitted/1, lines},
    p2 => {fun p2_intercept2/1, lines},

    p1_using_p2 => {fun p1_using_p2/1, lines}
}.

p1_using_p2(Lines) ->
    play5(Lines, 0, fun (N, M) -> N =< 100 andalso M =< 100 end, 0).

-doc """
A <- 3 tokens
B <- 1 token
A <= 100, B <= 100

Pxy = n·Axy + m·Bxy, minimize 3·n + m

Find the point of intersection on the two lines formed
by vectors A and B. There is only one way to press the
buttons to reach P, and that will be when A intersects
line B at some offset C.

a(x) = ax
b(x) = bx + c
p(x) = a(x) + b(x) = ax + bx + c = x·(a + b) + c
  py = px · (ay/ax + by/bx) + c
   c = py - px · (ay/ax + by/bx)

Find intersections of a(x) and b(x).

a(x) = a(x) + b(x)
Ax = Ax + Bx + C
 0 = Bx + C
Bx = -C
x = -C / B

Simplify

C = Py - Px · (Ay/Ax + By/Bx) = -(By/Bx) · x
x = -[ Py - Px · (Ay/Ax + By/Bx) ] / (By/Bx)
  = -[ Py - Px · (Ay/Ax + By/Bx) ] Bx / By
  = -[ Bx·Py - Bx·Px·(Ay/Ax + By/Bx) ] / By
  = -[ Bx·Py - Bx·Px·Ay/Ax -Px·By ] / By

Or if the A has a higher slope it’ll be different...

a(x) = ax + c
b(x) = bx
p(x) = a(x) + b(x) = ax + bx + c = x·(a + b) + c
  py = px · (ay/ax + by/bx) + c
   c = py - px · (ay/ax + by/bx)

Find intersections of a(x) and b(x).

b(x) = a(x) + b(x)
Bx = Ax + Bx + C
 0 = Ax + C
Ax = -C
x = -C / A

Simplify

C = Py - Px · (Ay/Ax + By/Bx) = -(Ay/Ax) · x
x = -[ Py - Px · (Ay/Ax + By/Bx) ] / (Ay/Ax)
  = -[ Py - Px · (Ay/Ax + By/Bx) ] Ax / Ay
  = -[ Ax·Py - Ax·Px·(Ay/Ax + By/Bx) ] / Ay
  = -[ Ax·Py - Px·Ay - Ax·Px·By/Bx ] / Ay

""".
p1_submitted(Lines) ->
    play(Lines, 0).

play([], Sum) ->
    Sum;
play([<<>> | Lines], Sum) ->
%%    io:format("Skipping blank line.\n"),
    play(Lines, Sum);
play([A, B, P | Lines], Sum) ->
%%    io:format("Processing ~ts.\n", [P]),
    {Ax, Ay} = button($A, A),
    {Bx, By} = button($B, B),
    {Px, Py} = prize(P),
    Matches = [
        3 * NA + NB
        ||
        NA <- lists:seq(0, 100),
        NB <- lists:seq(0, 100),
        NA * Ax + NB * Bx == Px,
        NA * Ay + NB * By == Py
    ],
    Tokens = case Matches of
        [] ->
%%            io:format("\e[31mNo matching win\e[m\n"),
            0;
        [_|_] ->
            Lowest = lists:min(Matches),
%%            io:format("\e[32mLowest is \e[33m~p\e[m\n", [Lowest]),
            Lowest
    end,
    play(Lines, Sum + Tokens).

button(Label, <<"Button ", Label, ": X+", Buffer/binary>>) ->
    {{X, _}, <<", Y+", Rest/binary>>} = day1:int(Buffer, 0, 0),
    {{Y, _}, <<>>} = day1:int(Rest, 0, 0),
    {X, Y}.

prize(<<"Prize: X=", Buffer/binary>>) ->
    {{X, _}, <<", Y=", Rest/binary>>} = day1:int(Buffer, 0, 0),
    {{Y, _}, <<>>} = day1:int(Rest, 0, 0),
    {X, Y}.


-doc """
These big numbers; are they multiples of an integer slope?

```
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=10000000008400, Y=10000000005400
    Slope = 10000000005400 / 10000000008400
          = (no they are not easily divisible by integer multiples)
```
""".
p2_submitted(Lines) ->
    play2(Lines, 0, 10000000000000).

play2([], Sum, _Delta) ->
    Sum;
play2([<<>> | Lines], Sum, Delta) ->
    play2(Lines, Sum, Delta);
play2([A, B, P | Lines], Sum, Delta) ->
    {Ax, Ay} = button($A, A),
    {Bx, By} = button($B, B),
    {RPx, RPy} = prize(P),
    Px = RPx + Delta,
    Py = RPy + Delta,
    % This is the intercept X between the A button line and the B button line.
    % There is rounding error.
    X1 = -(Bx * Py - Bx * Px * Ay / Ax - Px * By) / By,
    N1 = floor(X1 / Ax),
    X2 = -(Bx * Py - Px * Ay - Ax * Px * By / Bx) / Ay,
    N2 = floor(X2 / Ax),
%%    io:format("X1 = ~p, N1 = ~p~n", [X1, N1]),
%%    io:format("X2 = ~p, N2 = ~p~n", [X2, N2]),
    play2_match(Ax, Ay, Bx, By, Px, Py, [N || N <- [N1, N2], N >= 0, N * Ax =< Px], Sum, Delta, Lines).

play2_match(_, _, _, _, _, _, [], Sum, Delta, Lines) ->
    play2(Lines, Sum, Delta);

play2_match(Ax, Ay, Bx, By, Px, Py, [N | Ns], Sum, Delta, Lines) ->
    SearchDistance = 100,
    Matches = lists:foldl(
        fun
            (NA, Ms) when NA * Ax > Px; NA * Ay > Py ->
                Ms;

            (NA, Ms) when 0 =/= (Px - NA * Ax) rem Bx; 0 =/= (Py - NA * Ay) rem By ->
                Ms;

            (NA, Ms) ->
                NB = (Px - NA * Ax) div Bx,
                [3 * NA + NB | Ms]
        end,
        [],
        lists:seq(max(0, N - SearchDistance), N + SearchDistance)
    ),
    _Tokens = case Matches of
                 [] ->
%%                     io:format("\e[31mNo matching win\e[m\n"),
                     0;
                 [_|_] ->
                     Lowest = lists:min(Matches),
%%                     io:format("\e[32mLowest is \e[33m~p\e[m\n", [Lowest]),
                     Lowest
             end,
    play2_match(Ax, Ay, Bx, By, Px, Py, Ns, Sum, Delta, Lines).

p2_1again(Lines) ->
    play3(Lines, 0, 10000000000000).
%%    play3(Lines, 0, 0).

play3([], Sum, _Delta) ->
    Sum;
play3([<<>> | Lines], Sum, Delta) ->
%%    io:format("Skipping blank line.\n"),
    play3(Lines, Sum, Delta);
play3([A, B, P | Lines], Sum, Delta) ->
%%    io:format("Processing ~ts.\n", [P]),
    {Ax, Ay} = button($A, A),
    {Bx, By} = button($B, B),
    {RPx, RPy} = prize(P),
    Px_ = RPx + Delta,
    Py_ = RPy + Delta,
    GCD = gcd(Px_, Py_),
    Px  = Px_ div GCD,
    Py  = Py_ div GCD,
    MaxA = floor(min(Px / Ax, Py / Ay)),
    NextCost = fun
        (_Loop, NA, Min) when NA < 0 ->
            Min;

        (Loop, NA, Min) ->
            if
                NA rem 10000000 == 0 ->
                    io:format("\e[90mOn \e[33m~p\e[90m -> \e[3;34m~p\e[2m%\e[m\n", [MaxA-NA, (MaxA - NA)/MaxA]);
                true ->
                    ok
            end,
            Xa = NA * Ax,
            Ya = NA * Ay,
            Xb = Px - Xa,
            Yb = Py - Ya,
            NB = Xb div Bx,
            Next = if
                Xb rem Bx == 0,
                Yb rem By == 0,
                NB * Bx == Xb,
                NB * By == Yb ->
                    Cost = 3 * NA + NB,
                    case Min of
                        nil  -> Cost;
                        Prev -> min(Prev, Cost)
                    end;

                true ->
                    Min
            end,
            Loop(Loop, NA - 1, Next)
    end,
    MinCost = NextCost(NextCost, MaxA, nil),
    play3(Lines, case MinCost of nil -> Sum; Cost -> Sum + GCD * Cost end, Delta).

gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

p2_binary_search(Lines) ->
%%    play4(Lines, 0, 10000000000000).
    Self = self(),
    Pid = spawn(fun () -> Self ! play4(Lines, 0, 0) end),
    receive
        Result ->
            Result

    after 5000 ->
        exit(Pid, timeout),
        timeout
    end.

play4([], Sum, _Delta) ->
    Sum;
play4([<<>> | Lines], Sum, Delta) ->
%%    io:format("Skipping blank line.\n"),
    play4(Lines, Sum, Delta);
play4([A, B, P | Lines], Sum, Delta) ->
%%    io:format("Processing ~ts.\n", [P]),
    {Ax, Ay} = button($A, A),
    {Bx, By} = button($B, B),
    {RPx, RPy} = prize(P),
    Px_ = RPx + Delta,
    Py_ = RPy + Delta,
%%    GCD = gcd(Px_, Py_),
    GCD = 1,
    Px  = Px_ div GCD,
    Py  = Py_ div GCD,
    MaxA = floor(max(Px / Ax, Py / Ay)),
    io:format("Looking for ~p / ~p (max is ~p)~n", [Px, Py, MaxA]),
    Cost = hunt({Ax, Ay, Ay / Ax, Bx, By, By / Bx, Px, Py}, 0, MaxA, MaxA div 2),
    play4(Lines, Sum + GCD * Cost, Delta).

hunt(_Params, Min, Max, NA) when NA < Min; NA > Max ->
    0;
hunt({Ax, Ay, As, Bx, By, Bs, Px, Py} = Params, Min, Max, NA) ->
    NB = (Px - NA * Ax) div Bx,
    X = NA * Ax + NB * Bx,
    Y = NA * Ay + NB * By,
    io:format("\e[90mHunting at NA = \e[33m~p\e[m\n", [NA]),
    io:format("\e[90m            X = \e[33m~p\e[m\n", [X]),
    io:format("\e[90m            Y = \e[33m~p\e[m\n", [Y]),
    LessOfA = fun () -> hunt(Params, NA, Max, NA + max(1, (Max - NA) div 2)) end,
    MoreOfA = fun () -> hunt(Params, Min, NA, NA - max(1, (NA - Min) div 2)) end,
    if
        NA * Ax > Px ->
            LessOfA();

        X == Px, Y == Py ->
            3 * NA + NB;

        Y < Py ->
            case As > Bs of true -> MoreOfA(); false -> LessOfA() end;

        Y > Py ->
            case As > Bs of true -> MoreOfA(); false -> LessOfA() end
    end.


p2_intercept2(Lines) ->
    play5(Lines, 0, fun (_, _) -> true end, 10000000000000).

play5([], Sum, _IsValid, _Delta) ->
    Sum;
play5([<<>> | Lines], Sum, IsValid, Delta) ->
%%    io:format("Skipping blank line.\n"),
    play5(Lines, Sum, IsValid, Delta);
play5([A, B, P | Lines], Sum, IsValid, Delta) ->
%%    io:format("Processing ~ts.\n", [P]),
    {Ax, Ay} = button($A, A),
    {Bx, By} = button($B, B),
    {RPx, RPy} = prize(P),
    Px_ = RPx + Delta,
    Py_ = RPy + Delta,
%%    GCD = gcd(Px_, Py_),
    GCD = 1,
    Px  = Px_ div GCD,
    Py  = Py_ div GCD,
    {NA, NB} = hunt2({Ax, Ay, Bx, By, Px, Py}),
    Cost = case IsValid(NA, NB) of
        true -> 3 * NA + NB;
       false -> 0
    end,
    play5(Lines, Sum + Cost, IsValid, Delta).

hunt2({Ax, Ay, Bx, By, Px, Py} = Params) ->
    if
        % All A
        0 == Px rem Ax, 0 == Py rem (Px div Ax) * Ay ->
            {Px div Ax, 0};

        % All B
        0 == Px rem Bx, 0 == Py rem (Px div Bx) * By ->
            {0, Px div Bx};

        % Equally A and B
        0 == Px rem (Ax + Bx), 0 == Py rem (Px div (Ax + Bx) * (Ay + By)) ->
            {Px div (Ax + Bx), Px div (Ax + Bx)};

        % Use M.A.T.H.S.
        true ->
            hunt2(slopes, Params)
    end.

hunt2(slopes, {Ax, Ay, Bx, By, Px, Py} = Params) ->
    Ps = Py / Px,
    case {Ay / Ax, By / Bx} of
        {As, Bs} when As > Ps, Bs > Ps ->
            {0, 0};

        {As, Bs} when As < Ps, Bs < Ps ->
            {0, 0};

        {As, Bs} when As < Bs ->
            hunt2(steeperB, Params);

        {As, Bs} when Bs < As ->
            hunt2(steeperA, Params)
    end;

hunt2(steeperB, {Ax, Ay, Bx, By, Px, Py} = _Params) ->
    Ac = Py - Px * Ay / Ax,
    % Ya(x) = Ma x + Ac
    % Yb(x) = Mb x
    % Ma x + Ac = Mb x
    % x (Mb - Ma) = Ac
    X = Ac / (By / Bx - Ay / Ax),
    NB = round(X / Bx),
    NA = round((Px - X) / Ax),
    case {NA * Ax + NB * Bx, NA * Ay + NB * By} of
        {Px, Py} ->
            {NA, NB};

        {_Ox, _Oy} ->
            {0, 0}
    end;

hunt2(steeperA, {Ax, Ay, Bx, By, Px, Py} = _Params) ->
    Bc = Py - Px * By / Bx,
    % Ya(x) = Ma x
    % Yb(x) = Mb x + Bc
    % Mb x + Bc = Ma x
    % x (Ma - Mb) = Bc
    X = Bc / (Ay / Ax - By / Bx),
    NA = round(X / Ax),
    NB = round((Px - X) / Bx),
    case {NA * Ax + NB * Bx, NA * Ay + NB * By} of
        {Px, Py} ->
            {NA, NB};

        {_Ox, _Oy} ->
            {0, 0}
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test_() -> [
    {"Example 1", ?_assertEqual(480, aoc:test(day13, p1, "day13_a"))}
].

-endif.
