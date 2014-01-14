%%%-------------------------------------------------------------------
%%% @author Chen Wei <acee06.weichen@gmail.com>
%%% @copyright (C) 2014, Chen Wei
%%% @doc
%%%
%%% @end
%%% Created : 13 Jan 2014 by Chen Wei <acee06.weichen@gmail.com>
%%%-------------------------------------------------------------------
-module (ti_server).

-behaviour (gen_server).

-export ([start_link/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-record (state, {lsock}).

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
start_link(LSock) ->
	gen_server:start_link(?MODULE, [LSock], []).

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
init([LSock]) ->
	{ok, #state{lsock = LSock}, 0}.

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
handle_call(Msg, _From, State) ->
	{reply, {ok, Msg}, State}.

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
handle_info({tcp, Socket, RawData}, State) ->
	NewState = handle_data(Socket, RawData, State),
	{noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
	{ok, _Sock1} = gen_tcp:accept(LSock),
	{ok, _Sock2} = gen_tcp:accept(LSock),
	{ok, _Sock3} = gen_tcp:accept(LSock),
	{ok, _Sock4} = gen_tcp:accept(LSock),
	io:format("1: ~p, 2: ~p, 3: ~p, 4: ~p~n", [_Sock1, _Sock2, _Sock3, _Sock4]),
	mahjong_sup:start_link(_Sock1, _Sock2, _Sock3, _Sock4),
	mahjong_ser:new_game({}),
	Reply = mahjong_ser:get_state(),
	io:format("~p~n", [Reply]),
	ti_sup:start_child(),
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
handle_data(Socket, RawData, State) ->
	gen_tcp:send(Socket, RawData),
	State.
