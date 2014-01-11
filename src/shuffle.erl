%%%-------------------------------------------------------------------
%%% @author 朱 軼青 <sbbirdzhu@ake-no-MacBook-Pro.local>
%%% @copyright (C) 2014, 朱 軼青
%%% @doc
%%%
%%% @end
%%% Created :  1 Jan 2014 by 朱 軼青 <sbbirdzhu@ake-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(shuffle).

-export([shuffle_v1/1]).

shuffle_v1(L) ->            
    List1 = [{random:uniform(), X} || X <- L],
    List2 = lists:keysort(1, List1),
        [E || {_, E} <- List2]. 


