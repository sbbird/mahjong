%%%-------------------------------------------------------------------
%%% @author Chen Wei <acee06.weichen@gmail.com>
%%% @copyright (C) 2014, Chen Wei
%%% @doc
%%%
%%% @end
%%% Created : 13 Jan 2014 by Chen Wei <acee06.weichen@gmail.com>
%%%-------------------------------------------------------------------
-module(ti_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define (DEFAULT_PORT, 1155).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Port = case application:get_env(tcp_interface, port) of
			{ok, P} -> P;
			undefined -> ?DEFAULT_PORT
		end,
	{ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    case ti_sup:start_link(LSock) of
    	{ok, Pid} ->
    		ti_sup:start_child(),
    		{ok, Pid};
    	Other ->
    		{error, Other}
    end.

stop(_State) ->
    ok.
