-module(bowling).

-include_lib("eunit/include/eunit.hrl").

score(Pins) -> score(Pins,0,{1,0,0},{0,0}).

score([],Score,_Frame,_Bonus) -> Score;
score([Pin|TheRest],Score,Frame,Bonus) ->
	score(TheRest,increment_score(Score,Pin,Frame,Bonus),frame(Frame,Pin),bonus(Bonus,Frame,Pin)).

increment_score(CurrentScore,NewPins,Frame,{ThisFrameBonus,_NextFrameBonus}) -> CurrentScore + count_pins(Frame,NewPins) + (NewPins * ThisFrameBonus).

count_pins({FrameNumber,_PinsInThisFrame,_ThrowsInThisFrame},NewPins) when FrameNumber =< 10 -> NewPins;
count_pins(_,_) -> 0.

frame({FrameNumber,0,0},10) -> {FrameNumber+1,0,0};
frame({FrameNumber,_PinsInThisFrame,1},_NewPins) -> {FrameNumber+1,0,0};
frame({FrameNumber,_PinsInThisFrame,ThrowsInThisFrame},NewPins) -> {FrameNumber,NewPins,ThrowsInThisFrame+1}.  

bonus({_ThisFrameBonus,NextFrameBonus},{FrameNumber,0,0},10) when FrameNumber =< 10 -> {1+NextFrameBonus,1};
bonus(_OldBonus,{FrameNumber,PinsInThisFrame,_ThrowsInThisFrame},NewPins) when PinsInThisFrame + NewPins == 10, FrameNumber =< 10 -> {1,0};
bonus({_ThisFrameBonus,NextFrameBonus},_,_) -> {NextFrameBonus,0}.


-ifdef(EUNIT).
%%Tests

gutter_ball_game_test() -> 0 = score([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
one_pin_game_test() -> 1 = score([1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
two_pin_game_test() -> 2 = score([1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
all_open_frame_test() -> 70 = score([3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4]).
game_with_a_spare_test() -> 20 = score([5,5,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
game_with_a_spare_every_frame_test() -> 150 = score([5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]).
game_with_a_strike_test() -> 20 = score([10,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
game_with_a_strike_and_a_real_second_ball_test() -> 30 = score([10,5,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
game_with_seqential_strikes_test() -> 60 = score([10,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
perfect_game_test() -> 300 = score([10,10,10,10,10,10,10,10,10,10,10,10]).

frame_test() -> {1,0,1} = frame({1,0,0},0).
second_throw_frame_test() -> {2,0,0} = frame({1,0,1},0).
score_in_frame_test() -> {1,1,1} = frame({1,0,0},1).
score_in_second_throw_frame_test() -> {2,0,0} = frame({1,0,1},1).

spare_bonus_test() -> {1,0} = bonus({0,0},{1,5,1},5).
strike_bonus_test() -> {1,1} = bonus({0,0},{1,0,0},10).
strike_after_strike_bonus_test() -> {2,1} = bonus({1,1},{1,0,0},10).
bonus_rolls_off_test() -> {1,0} = bonus({1,1},{1,0,0},0).

increment_score_test() -> 1 = increment_score(0,1,{1,0,0},{0,0}). 
increment_score_with_bonus_test() -> 20 = increment_score(10,5,{2,0,0},{1,0}).

count_pins_frame_less_than_ten_test() -> 3 = count_pins({3,2,1},3).
count_pins_equal_to_ten_test() -> 3 = count_pins({10,2,1},3).
count_pins_greater_than_ten_test() -> 0 = count_pins({11,2,1},3).

-endif.
