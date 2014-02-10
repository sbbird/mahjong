%%%-------------------------------------------------------------------
%%% @author Chen Wei <acee06.weichen@gmail.com>
%%% @copyright (C) 2014, Chen Wei
%%% @doc
%%%  This module calculate the shanten number of the Tehai. 
%%% @end
%%% Created : 19 Jan 2014 by Chen Wei <acee06.weichen@gmail.com>
%%%-------------------------------------------------------------------
-module (shanten).
-include ("shanten_table.hrl").

%% API
-export ([shanten/1, get_candidate/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

shanten(Tehai) -> 
	%% TODO: aka dora should be changed before calculating the shanten.
	Dict = tehai_dict(Tehai, dict:new()),
	Y9Dict = tehai_y9dict(Tehai, dict:new()),
	C = shanten_chitui(Tehai, Dict),
	K = shanten_kokushi(Tehai, Y9Dict),
	G = shanten_general(Tehai, Dict),
	{shanten, C, K, G}.

get_candidate(Key) -> 
	case lists:keyfind(Key, 1, ?SHANTEN_TABLE) of
		{_, Mentsu, Candidate} ->
			{ok, Mentsu, Candidate};
		false -> key_is_illegal
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 	6 - Number of Tuitsy + (7 - Number of tile kinds {max = 7})
%% @end
%%--------------------------------------------------------------------
shanten_chitui(Tehai, Dict) ->
	Kinds = length(lists:usort(Tehai)),
	D2 = dict:filter(fun (K, V) -> V > 1 end, Dict),
	Ntuitsy = dict:size(D2),
	6 - Ntuitsy + (7 - if Kinds > 7 -> 7; true -> Kinds end).

%%--------------------------------------------------------------------
%% @doc
%%  13 - Number of Y9 tile kinds - Number of Y9 Tuitsy {max = 1}
%% @end
%%--------------------------------------------------------------------
shanten_kokushi(Tehai, Y9Dict) ->
	Y9Kinds = dict:size(Y9Dict),
	YD2 = dict:filter(fun (K, V) -> V > 1 end, Y9Dict),
	NY9tuitsy = dict:size(YD2),
	13 - Y9Kinds - if NY9tuitsy > 1 -> 1; true -> NY9tuitsy end.

shanten_general(Tehai, Dict) ->
	%% todo: calculate the general style shanten number. 
	calc_key_msp(Dict).

%%--------------------------------------------------------------------
%% @doc
%%  count the number of the same hai, and store a hai as the key, and 
%%  the number of the hai as the value to the dict.
%% @end
%%--------------------------------------------------------------------
tehai_dict([], D) ->
	D;
tehai_dict([F | RTehai], D) -> 
	tehai_dict(RTehai, dict:update_counter(F, 1, D)).

tehai_y9dict([], YD) ->
	YD;
tehai_y9dict([F | RTehai], YD) ->
	case F of
		T when T =:= 16#01; T =:= 16#09; T =:= 16#11; T =:= 16#19; 
			T =:= 16#21; T =:= 16#29; T >= 16#31, T =< 16#37
			-> tehai_y9dict(RTehai, dict:update_counter(F, 1, YD));
		_ -> tehai_y9dict(RTehai, YD)
	end.

calc_key_msp(Dict) ->
	Mkey = calc_key(Dict, 16#01, 0, 0),
	Skey = calc_key(Dict, 16#11, 0, 0),
	Pkey = calc_key(Dict, 16#21, 0, 0),
	{Mkey, Skey, Pkey}.

calc_key(D, Start, Key, 9) ->
	Key;
calc_key(D, Start, Key, Pos) ->
	P = Start + Pos,
	K2 = Key bsl 3,
	case dict:find(P, D) of
		{ok, Count} ->
			K3 = K2 bor Count;
		error -> K3 = K2
	end,
	calc_key(D, Start, K3, Pos + 1).
