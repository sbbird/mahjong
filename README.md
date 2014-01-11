mahjong engine for Shirai-mahjong

usage:

  compile:

$ ./rebar compile


  run:

$ erl -pa ebin

1> application:start(mahjong).

2> mahjong_ser:new_game({}).

3> mahjong_ser:get_state().
