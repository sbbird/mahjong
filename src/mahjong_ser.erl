%%%-------------------------------------------------------------------
%%% @author sbbird.zhu <sbbirdzhu@MacBook-Pro.local>
%%% @copyright (C) 2014, sbbird.zhu
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2014 by sbbird.zhu <sbbirdzhu@MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mahjong_ser).
-include("game.hrl").
-behaviour(gen_server).

%% API
-export([start_link/4]).

-export([print_info/0, 
	 get_info/0,
	 get_player_state/1,
	 new_game/1,
	 new_round/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(_Sock1, _Sock2, _Sock3, _Sock4) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [_Sock1, _Sock2, _Sock3, _Sock4], []).


print_info() ->
    gen_server:cast(?SERVER, print).

get_info()->
    gen_server:call(?SERVER, get_info).

new_game({})->
    gen_server:call(?SERVER, new_game).

new_round()->
    gen_server:call(?SERVER, new_round).

get_player_state(ID)->
    gen_server:call(?SERVER, {get_player_state, ID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([_Sock1, _Sock2, _Sock3, _Sock4]) ->
    io:format("here mahjong_sup, ~p ~p ~p ~p~n", [_Sock1, _Sock2, _Sock3, _Sock4]),
    Kawa = #kawa{},

    Player1State = #player_state{player_id=1, kawa=Kawa},
    Player2State = #player_state{player_id=2, kawa=Kawa},
    Player3State = #player_state{player_id=3, kawa=Kawa},
    Player4State = #player_state{player_id=4, kawa=Kawa},
    Yama = #yama{},
    RoundState = #round_state{round=ea, yama=Yama},
    {ok, #game_info{gameid=1,
		     round_state=RoundState,
		     player1=Player1State,
		     player2=Player2State,
		     player3=Player3State,
		     player4=Player4State,
		     other={}
		    }
    }.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_info, _From, State) ->
    Reply = {ok, State},
    {reply, Reply, State};
handle_call(new_game, _From, _State) ->    
    
    YamaList = shuffle(?INIT_YAMA),
    
    {Tehai1, Tehai2, Tehai3, Tehai4} = haipai(YamaList),
    Yama = #yama{yama_list=YamaList, head=13*4},
    
    Player1State = new_player(1, ?EAST, Tehai1),
    Player2State = new_player(2, ?SOUTH, Tehai2),
    Player3State = new_player(3, ?WEST, Tehai3),
    Player4State = new_player(4, ?NORTH, Tehai4),

    RoundState = #round_state{round=e0, yama=Yama},
    {reply,ok ,#game_info{gameid=1,
		     round_state=RoundState,
		     player1=Player1State,
		     player2=Player2State,
		     player3=Player3State,
		     player4=Player4State,
		     other={}
		    }
    };
handle_call({get_player_state, 1}, _From, State) -> 
    Reply = {ok, State#game_info.player1},
    {reply, Reply, State};
handle_call({get_player_state, 2}, _From, State) -> 
    Reply = {ok, State#game_info.player2},
    {reply, Reply, State};
handle_call({get_player_state, 3}, _From, State) -> 
    Reply = {ok, State#game_info.player3},
    {reply, Reply, State};
handle_call({get_player_state, 4}, _From, State) -> 
    Reply = {ok, State#game_info.player4},
    {reply, Reply, State};
handle_call(new_round,  _From, State) ->
    {reply, do_nothing, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(print, State) ->
    %display(State),
    io:format("The value is: ~p.", [State]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
shuffle(L) ->            
%    List1 = [{random:uniform(), X} || X <- L],
%    List2 = lists:keysort(1, List1),
%        [E || {_, E} <- List2]. 
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])].
haipai(L) ->
    {List1, _Rest} = lists:split(13, L), 
    {List2, _Rest1} = lists:split(13, _Rest),
    {List3, _Rest2} = lists:split(13, _Rest1),
    {List4, _Rest3} = lists:split(13, _Rest2),
    {List1, List2, List3, List4}.
    

new_player(ID, Pos, Tehai) ->
    Kawa = #kawa{},
    SortedTehai = lists:sort(Tehai),
    #player_state{player_id=ID, position=Pos, kawa=Kawa, tehai=SortedTehai}.
