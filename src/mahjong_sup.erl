%%%-------------------------------------------------------------------
%%% @author sbbird.zhu <sbbirdzhu@MacBook-Pro.local>
%%% @copyright (C) 2014, sbbird.zhu
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2014 by sbbird.zhu <sbbirdzhu@MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mahjong_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(_Sock1, _Sock2, _Sock3, _Sock4) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [_Sock1, _Sock2, _Sock3, _Sock4]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([_Sock1, _Sock2, _Sock3, _Sock4]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {mahjong_ser, {mahjong_ser, start_link, [_Sock1, _Sock2, _Sock3, _Sock4]},
	      Restart, Shutdown, Type, [mahjong_ser]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
