%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@ghoti-dev.smcqueen.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2010 by Stan McQueen <smcqueen@ghoti-dev.smcqueen.com>
%%%-------------------------------------------------------------------
-module(test).

%% API
-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

test1() ->
    J="{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42, 23], \"id\": 1}",
    httpc:request(post, {"http://localhost:8080/rpc", [], "application/x-www-form-urlencoded", J}, [], []).

