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
	D2 = dict:filter(fun (_K, V) -> V > 1 end, Dict),
	Ntuitsy = dict:size(D2),
	6 - Ntuitsy + (7 - if Kinds > 7 -> 7; true -> Kinds end).

%%--------------------------------------------------------------------
%% @doc
%%  13 - Number of Y9 tile kinds - Number of Y9 Tuitsy {max = 1}
%% @end
%%--------------------------------------------------------------------
shanten_kokushi(_Tehai, Y9Dict) ->
	Y9Kinds = dict:size(Y9Dict),
	YD2 = dict:filter(fun (_K, V) -> V > 1 end, Y9Dict),
	NY9tuitsy = dict:size(YD2),
	13 - Y9Kinds - if NY9tuitsy > 1 -> 1; true -> NY9tuitsy end.

%%--------------------------------------------------------------------
%% @doc
%%  8 - (Number of Mentsu * 2 + Number of Candidate + HasHead{0 or 1})
%% @end
%%--------------------------------------------------------------------
shanten_general(_Tehai, Dict) ->
	%% calculate the general style shanten number. 
	{HasHead, Mentsu, Candidate} = count_mentsu_candidate(Dict),
	8 - (Mentsu * 2 + Candidate + HasHead).

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

calc_key(_D, _Start, Key, 9) ->
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

%% ignore isolated tiles
ignore([], D) ->
	D;
ignore([K | RKeys], D) ->
	case K of 
		Var when Var =:= 16#01; Var =:= 16#11; Var =:= 16#21 ->
			case (dict:is_key(Var + 1, D) or dict:is_key(Var + 2, D)) of
				false -> ignore(RKeys, dict:erase(Var, D));
				true -> ignore(RKeys, D)
			end;
		Var when Var =:= 16#09; Var =:= 16#19; Var =:= 16#29 ->
			case (dict:is_key(Var - 1, D) or dict:is_key(Var - 2, D)) of
				false -> ignore(RKeys, dict:erase(Var, D));
				true -> ignore(RKeys, D)
			end;
		Var when Var =:= 16#02; Var =:= 16#12; Var =:= 16#22 ->
			case (dict:is_key(Var - 1, D) or dict:is_key(Var + 1, D) or dict:is_key(Var + 2, D)) of
				false -> ignore(RKeys, dict:erase(Var, D));
				true -> ignore(RKeys, D)
			end;
		Var when Var =:= 16#08; Var =:= 16#18; Var =:= 16#28 ->
			case (dict:is_key(Var + 1, D) or dict:is_key(Var - 1, D) or dict:is_key(Var - 2, D)) of
				false -> ignore(RKeys, dict:erase(Var, D));
				true -> ignore(RKeys, D)
			end;
		Var when Var < 16#30 ->
			case (dict:is_key(Var - 1, D) or dict:is_key(Var - 2, D) 
				or dict:is_key(Var + 1, D) or dict:is_key(Var + 2, D)) of
				false -> ignore(RKeys, dict:erase(Var, D));
				true -> ignore(RKeys, D)
			end
	end.

ignore_isolated(Dict) -> 
	Pred = fun (K, V) -> (K < 16#30) and (V =:= 1) end,
	IsolatedDict = dict:filter(Pred, Dict),
	IsolatedKeys = dict:fetch_keys(IsolatedDict),
	ignore(IsolatedKeys, Dict).

%% main function to count the mentsu and candidate.
count_mentsu_candidate(D) -> 
	Pred = fun (_K, V) -> V >= 2 end,
	FDict = dict:filter(Pred, D),
	Keys = dict:fetch_keys(FDict),
	MinInt = -1 bsl 32,
	% Step 1: has head case.
	{MaxM, MaxC, MaxMC} = helper_f1(Keys, D, MinInt, MinInt, MinInt),
	% Step 2: no head case.
	{NoheadM, NoheadC} = helper_f2(D),
	NewMaxMC = NoheadM * 2 + NoheadC,
	if 
		NewMaxMC > MaxMC -> {0, NoheadM, NoheadC};
		true -> {1, MaxM, MaxC}
	end.

helper_f1([], _D, MaxM, MaxC, MaxMC) ->
	{MaxM, MaxC, MaxMC};
helper_f1([Key | RKeys], D, MaxM, MaxC, MaxMC) -> 
	HasHead = 1,
	NewValue = dict:fetch(Key, D) - 2,
	NewDict = dict:store(Key, NewValue, D),
	{M, C} = helper_f2(NewDict),
	NewMaxMC = M * 2 + C + HasHead,
	if 
		NewMaxMC > MaxMC -> helper_f1(RKeys, D, M, C, NewMaxMC);
		true -> helper_f1(RKeys, D, MaxM, MaxC, MaxMC)
	end.

helper_f2(D) ->
	RealDict = ignore_isolated(D),
	{Mkey, Skey, Pkey} = calc_key_msp(RealDict),
	{ok, ManMentsu, ManCandidate} = get_candidate(Mkey),
	{ok, SouMentsu, SouCandidate} = get_candidate(Skey),
	{ok, PinMentsu, PinCandidate} = get_candidate(Pkey),
	{ok, ZiMentsu, ZiCandidate} = calc_zi(lists:seq(16#31, 16#37), RealDict, 0, 0),
	Mentsu = ManMentsu + SouMentsu + PinMentsu + ZiMentsu,
	Candidate = ManCandidate + SouCandidate + PinCandidate + ZiCandidate,
	if 
		Mentsu + Candidate > 4 -> {Mentsu, 4 - Mentsu};
		true -> {Mentsu, Candidate}
	end.

calc_zi([], _D, M, C) ->
	{ok, M, C};
calc_zi([ZiKey | ZiRest], D, M, C) ->
	case dict:find(ZiKey, D) of
		{ok, Count} ->
			case Count of
				Var when Var >= 3 -> calc_zi(ZiRest, D, M + 1, C);
				2 -> calc_zi(ZiRest, D, M, C + 1);
				_ -> calc_zi(ZiRest, D, M ,C)
			end;
		_ -> calc_zi(ZiRest, D, M, C)
	end.
