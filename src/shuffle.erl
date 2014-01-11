%%%-------------------------------------------------------------------
%%% @author sbbird.zhu <sbbirdzhu@MacBook-Pro.local>
%%% @copyright (C) 2014, sbbird.zhu
%%% @doc
%%%
%%% @end
%%% Created :  1 Jan 2014 by sbbird.zhu <sbbirdzhu@MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(shuffle).

-export([shuffle_v1/1]).

shuffle_v1(L) ->            
    List1 = [{random:uniform(), X} || X <- L],
    List2 = lists:keysort(1, List1),
        [E || {_, E} <- List2]. 


