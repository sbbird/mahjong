%%%-------------------------------------------------------------------
%%% @author Chen Wei <acee06.weichen@gmail.com>
%%% @copyright (C) 2014, Chen Wei
%%% @doc
%%%
%%% @end
%%% Created : 13 Jan 2014 by Chen Wei <acee06.weichen@gmail.com>
%%%-------------------------------------------------------------------
-module(ti_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, [LSock]}, temporary, brutal_kill, Type, [I]}).
-define (SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(LSock) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]).

start_child() ->
	supervisor:start_child(?SERVER, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([LSock]) ->
	Server = ?CHILD(ti_server, worker),
	Children = [Server],
	RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, { RestartStrategy, Children } }.

