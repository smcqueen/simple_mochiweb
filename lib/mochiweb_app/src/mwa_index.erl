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
%    ServiceDescriptor = rfc4627_jsonrpc:service(
%                          {function, fun my_handler/3},
%                          "my_handler",
%                          "1",
%                          "1",
%                          [{"arglist", [{"arg1", num}, {"arg2", num}]}]),
    Path = Req:get(path),
    QueryObj = {obj, [{K, list_to_binary(V)} || {K,V} <- Req:parse_qs()]},
    HeaderObj = {obj, [{normalize(K), list_to_binary(V)}
		       || {K,V} <- mochiweb_headers:to_list(Req:get(headers))]},
    RequestInfo = {obj, [{"http_method", list_to_binary(atom_to_list(Req:get(method)))},
			 {"http_query_parameters", QueryObj},
			 {"http_headers", HeaderObj},
			 {"remote_peername", list_to_binary(Req:get(peer))},
			 {"scheme", <<"http">>}]},
    Body = Req:recv_body(),
    case invoke_service_method(Path, RequestInfo, Body) of
        no_match ->
            handle_non_jsonrpc_request(Req);

        {ok, ResultEnc, ResponseInfo} ->
            handle_non_jsonrpc_request(Req)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_non_jsonrpc_request(Req) ->
    Req:respond({200, [{"content_type", "text/plain"}], "Hello, Contentwatch User"}).

normalize(X) when is_atom(X) ->
    string:to_lower(atom_to_list(X));
normalize(X) when is_binary(X) ->
    string:to_lower(binary_to_list(X));
normalize(X) when is_list(X) ->
    string:to_lower(X).

extract_object_and_method(Path) ->
    Length = 0, %string:len(Path),
    ObjectMethod = string:substr(Path, Length + 1),
    case lists:reverse(string:tokens(ObjectMethod, "/")) of
		[] -> {<<>>, <<>>};
		[Object] -> {list_to_binary(Object), <<>>};
		[Method1, Object | _] -> {list_to_binary(Object), list_to_binary(Method1)}
    end.

parse_jsonrpc(Path, HttpMethod, QueryParametersObj, Body) ->
    case extract_object_and_method(Path) of
        no_match ->
            no_match;
        {Object, PathMethod} ->
            case HttpMethod of
                <<"POST">> ->
                    {ok, RequestObject, _} = rfc4627:decode(Body),
                    {post,
                     rfc4627:get_field(RequestObject, "id", null),
                     Object,
                     rfc4627:get_field(RequestObject, "method", undefined),
                     rfc4627:get_field(RequestObject, "params", undefined),
                     rfc4627:get_field(RequestObject, "jsonrpc", undefined)};
                _ ->
                    %% GET, presumably. We don't really care, here.
                    {get,
                     null,
                     Object,
                     PathMethod,
                     QueryParametersObj}
            end
    end.

invoke_service_method(default, Path, RequestInfo, Body) ->
    invoke_service_method("/jsonrpc", Path, RequestInfo, Body).

invoke_service_method(Path, RequestInfo, Body) ->
    case parse_jsonrpc(Path,
		       rfc4627:get_field(RequestInfo, "http_method", <<"GET">>),
		       rfc4627:get_field(RequestInfo, "http_query_parameters", {obj, []}),
		       Body) of
        no_match ->
            no_match;
        {PostOrGet, Id, Service, Method, Args, JsonRpcVsn} ->
            io:format("GetOrPost: ~p, Id: ~p, Service: ~p, Method: ~p, Args: ~p, JsonRPCVersion: ~p~n", [PostOrGet, Id, Service, Method, Args, JsonRpcVsn]),
            Fun = make_fun(my_handler),
            Fun()
    end.

service_address(RequestInfo, ServiceName) ->
    HttpHeaders = rfc4627:get_field(RequestInfo, "http_headers", {obj, []}),
    Host = case rfc4627:get_field(HttpHeaders, "host", undefined) of
	       undefined -> "";
	       Name -> "//" ++ binary_to_list(Name)
	   end,
    Scheme = case rfc4627:get_field(RequestInfo, "scheme", undefined) of
		 undefined -> "";
		 S -> binary_to_list(S) ++ ":"
	     end,
    list_to_binary(Scheme ++ Host ++ "/" ++ binary_to_list(ServiceName)).

extract_timeout_header(HeadersJsonObj) ->
    case rfc4627:get_field(HeadersJsonObj, "x-json-rpc-timeout", <<"default">>) of
	<<"default">> ->
	    default;
	<<"infinity">> ->
	    infinity;
	Other ->
	    list_to_integer(binary_to_list(Other))
    end.

make_fun(my_handler) ->
    fun my_handler/0.

my_handler() ->
    io:format("my_handler()~n").
