-module(bowling).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

score(Rolls) -> score(Rolls,0,{0,1},{0,0}).

score([ThisBall|T],Result,Frame,Bonus) -> score(T,Result+points(ThisBall,Bonus),next(ThisBall,Frame),bonus(ThisBall,Frame,Bonus));
score([], Result,_Frame,_Bonus) -> Result.

next(ThisBall,{FrameScore,_}) when ThisBall + FrameScore == 10 -> {0,1};
next(_ThisBall,{_FrameScore,2}) -> {0,1};
next(ThisBall,{FrameScore,1}) -> {FrameScore+ThisBall,2}.

points(ThisBall,{Bonus1,Bonus2}) -> ThisBall+apply_bonus(Bonus1,ThisBall)+apply_bonus(Bonus2,ThisBall).

apply_bonus(0,_) -> 0;
apply_bonus(_,ThisBall) -> ThisBall.

bonus(ThisBall,{FrameScore,FrameBall},{_Bonus1,Bonus2}) when ThisBall + FrameScore == 10 -> {3-FrameBall,Bonus2};
bonus(_,_,{Bonus1,_Bonus2}) -> {0,decr(Bonus1)}.

decr(0) -> 0;
decr(N) -> N-1.

-ifdef(EUNIT).

gutter_game_test() -> 0 = score([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
one_point_game_test() -> 1 = score([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]).
spare_game_test() -> 16 = score([1,9,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
strike_game_test() -> 22 = score([10,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).

-endif.
