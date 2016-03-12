-module(bowling).
-export([score/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([verbose/0]).
verbose() -> eunit:test(?MODULE,[verbose]).
-endif.

-type score() :: 0..300.
-type pin() :: 0..10.
-type pins() :: list(pin()).
-type frame_number() :: 0..11.
-type throws() :: 0 | 1.
-type bonus() :: 0 | 1 | 2.
-type bonus_rec() :: {bonus,bonus(),bonus()}.
-type frame_rec() :: {frame,frame_number(),score(),throws()}.

-record(frame,{number=1 :: frame_number(),score=0 :: score(),throws=0 :: throws()}).
-record(bonus,{this=0 :: bonus(),next=0 :: bonus()}).

-spec score(pins()) -> score().
score(Pins) -> score(Pins,0,#frame{},#bonus{}).

-spec score(pins(),score(),frame_rec(),bonus_rec()) -> score().
score([],Score,_Frame,_Bonus) -> Score;
score([Pin|TheRest],Score,#frame{}=Frame,#bonus{}=Bonus) ->
	score(TheRest,increment_score(Score,Pin,Frame,Bonus),frame(Frame,Pin),bonus(Bonus,Frame,Pin)).

increment_score(CurrentScore,NewPins,#frame{}=Frame,#bonus{this=ThisFrameBonus}) -> CurrentScore + count_pins(Frame,NewPins) + (NewPins * ThisFrameBonus).

count_pins(#frame{number=FrameNumber},NewPins) when FrameNumber =< 10 -> NewPins;
count_pins(_,_) -> 0.

frame(#frame{number=FrameNumber,score=0,throws=0},10) -> #frame{number=FrameNumber+1};
frame(#frame{number=FrameNumber,throws=1},_NewPins) -> #frame{number=FrameNumber+1};
frame(#frame{number=FrameNumber,throws=ThrowsInThisFrame},NewPins) -> #frame{number=FrameNumber,score=NewPins,throws=ThrowsInThisFrame+1}.

bonus(#bonus{next=NextFrameBonus},#frame{number=FrameNumber,score=0,throws=0},10) when FrameNumber =< 10 -> #bonus{this=1+NextFrameBonus,next=1};
bonus(_OldBonus,#frame{number=FrameNumber,score=PinsInThisFrame},NewPins) when PinsInThisFrame + NewPins == 10, FrameNumber =< 10 -> #bonus{this=1};
bonus(#bonus{next=NextFrameBonus},_,_) -> #bonus{this=NextFrameBonus}.


-ifdef(EUNIT).
%%Tests

frame_test() -> #frame{throws=1} = frame(#frame{},0).
second_throw_frame_test() -> #frame{number=2} = frame(#frame{throws=1},0).
score_in_frame_test() -> #frame{score=1,throws=1} = frame(#frame{},1).
score_in_second_throw_frame_test() -> #frame{number=2} = frame(#frame{throws=1},1).

spare_bonus_test() -> #bonus{this=1} = bonus(#bonus{},#frame{score=5,throws=1},5).
strike_bonus_test() -> #bonus{this=1,next=1} = bonus(#bonus{},#frame{},10).
strike_after_strike_bonus_test() -> #bonus{this=2,next=1} = bonus(#bonus{this=1,next=1},#frame{},10).
bonus_rolls_off_test() -> #bonus{this=1} = bonus(#bonus{this=1,next=1},#frame{},0).

increment_score_test() -> 1 = increment_score(0,1,#frame{},#bonus{}).
increment_score_with_bonus_test() -> 20 = increment_score(10,5,#frame{number=2},#bonus{this=1}).

count_pins_frame_less_than_ten_test() -> 3 = count_pins(#frame{number=3,score=2,throws=1},3).
count_pins_equal_to_ten_test() -> 3 = count_pins(#frame{number=10,score=2,throws=1},3).
count_pins_greater_than_ten_test() -> 0 = count_pins(#frame{number=11,score=2,throws=1},3).

-endif.
