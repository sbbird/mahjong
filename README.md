mahjong engine for Shirai-mahjong

usage:

compile:
./rebar compile


run

$ erl -pa ebin

> application:start(mahjong).

> mahjong_ser:new_game({}).

> mahjong_ser:get_state().