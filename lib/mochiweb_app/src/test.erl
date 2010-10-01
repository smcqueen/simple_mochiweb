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
    executeRequest(J).

test2() ->
    J="{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42, 23, 2], \"id\": 1}",
    executeRequest(J).
    
test3() ->
    J="{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42], \"id\": 1}",
    executeRequest(J).
    
test4() ->
    J="{\"jsonrpc\": \"2.0\", \"method\": \"nosubtract\", \"params\": [42, 23], \"id\": 1}",
    executeRequest(J).
    
executeRequest(J) ->
    case httpc:request(post, {"http://localhost:8080/rpc", [], "application/x-www-form-urlencoded", J}, [], []) of
        {_StatusLine, _Headers, Body} ->
%            io:format("StatusLine = ~p~n, Headers = ~p~n, Body = ~p~n",
%                      [StatusLine, Headers, Body]),
            {_,_, Response} = Body,
            io:format("Response = ~p~n", [Response]);
        {_StatusCode, Body} ->
%            io:format("StatusCode = ~p~n, Body = ~p~n", [StatusCode, Body]),
            {_,_,Response} = Body,
            io:format("Response = ~p~n", [Response]);
        RequestId ->
            io:format("RequestId = ~p~n", [RequestId])
    end.
    


