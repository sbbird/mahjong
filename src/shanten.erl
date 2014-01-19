%%%-------------------------------------------------------------------
%%% @author Chen Wei <acee06.weichen@gmail.com>
%%% @copyright (C) 2014, Chen Wei
%%% @doc
%%%
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
	Dict = tehai_dict(Tehai, dict:new()),
	C = shanten_chitui(Tehai, Dict),
	K = shanten_kokushi(Tehai, Dict),
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
shanten_kokushi(Tehai, Dict) ->
	
	ok.

shanten_general(Tehai, Dict) ->
	ok.

tehai_dict([], D) ->
	D;
tehai_dict([F | RTehai], D) -> 
	tehai_dict(RTehai, dict:update_counter(F, 1, D)).
