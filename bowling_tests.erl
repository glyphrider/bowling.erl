-module(bowling_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(EUNIT).
-import(bowling,[score/1]).

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

-endif.
