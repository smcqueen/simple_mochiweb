%%%-------------------------------------------------------------------
%%% @author Stan McQueen <smcqueen@ghoti-dev.smcqueen.com>
%%% @copyright (C) 2010, Stan McQueen
%%% @doc
%%%  the index for our application
%%% @end
%%% Created : 29 Sep 2010 by Stan McQueen <smcqueen@ghoti-dev.smcqueen.com>
%%%-------------------------------------------------------------------
-module(mwa_index).

%% API
-export([handle/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This handles http requests
%% @spec (Req) -> void()
%% @end
%%--------------------------------------------------------------------
handle(Req) ->
    Req:respond({200, [{"content_type", "text/plain"}], "Hello, Contentwatch User"}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
