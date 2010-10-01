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

-record(state, {rcp_services}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This handles http requests
%% @spec (Req) -> void()
%% @end
%%--------------------------------------------------------------------
handle(Req) ->
    State = #state{rcp_services = dict:new()},
    {ok, NewState} = add_functions(State),
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
    case invoke_service_method(Path, RequestInfo, Body, NewState) of
        no_match ->
            no_match;
        {ok, ResultEnc, ResponseInfo} ->
            {obj, ResponseHeaderFields} =
                rfc4627:get_field(ResponseInfo, "http_headers", {obj, []}),
            Headers = [{K, binary_to_list(V)} || {K,V} <- ResponseHeaderFields],
            {ok, {200, Headers ++ [{"Content-type", "text/plain"}], ResultEnc}}
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

%invoke_service_method(default, Path, RequestInfo, Body) ->
%    invoke_service_method("/jsonrpc", Path, RequestInfo, Body).

invoke_service_method(Path, RequestInfo, Body, State) ->
    case parse_jsonrpc(Path,
		       rfc4627:get_field(RequestInfo, "http_method", <<"GET">>),
		       rfc4627:get_field(RequestInfo, "http_query_parameters", {obj, []}),
		       Body) of
        no_match ->
            no_match;
        {PostOrGet, Id, Service, Method, Args, JsonRpcVsn} ->
            io:format("GetOrPost: ~p, Id: ~p, Service: ~p, Method: ~p, Args: ~p, JsonRPCVersion: ~p~n", [PostOrGet, Id, Service, Method, Args, JsonRpcVsn]),
            {ResultOrError, Result, ResponseInfo} =
                case dict:find(Method, State#state.rcp_services) of
                    error ->
                        error_response(404, "Undefined procedure", Method);
                    {ok, Fun} ->
                        HttpHeaders = rfc4627:get_field(RequestInfo, "http_headers", {obj, []}),
                        Timeout = extract_timeout_header(HttpHeaders),
                        EndpointAddress = service_address(RequestInfo, Service),
                        invoke_service_method(
                          Fun, Id, PostOrGet, RequestInfo,
                          EndpointAddress, Method, Args, Timeout)
                end,
            ResultEnc = lists:flatten(rfc4627:encode(Result)),
            {ok, ResultEnc, ResponseInfo}
    end.

invoke_service_method(Fun, RequestId, PostOrGet, RequestInfo,
                      EndpointAddress, Method, Args, Timeout) ->
    expand_jsonrpc_reply(
      RequestId,
      case catch Fun(Args) of
        {'EXIT', {{function_clause, _}, _}} ->
            error_response(404, "Undefined procedure", Method);
        {'EXIT', Reason} ->
            error_response(500, "Internal error", list_to_binary(io_lib:format("~p", [Reason])));
        Response ->
            Response
      end
    ).

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

%coerce_args(_Params, Args) when is_list(Args) ->
%    Args;
%coerce_args(Params, {obj, Fields}) ->
%    [case lists:keysearch(binary_to_list(Name), 1, Fields) of
%	 {value, {_, Value}} -> coerce_value(Value, Type);
%	 false -> null
%     end || #service_proc_param{name = Name, type = Type} <- Params].

%coerce_value(Value, _Type) when not(is_binary(Value)) ->
%    Value;
%coerce_value(<<"true">>, <<"bit">>) -> true;
%coerce_value(_, <<"bit">>) -> false;
%coerce_value(V, <<"num">>) -> list_to_integer(binary_to_list(V));
%coerce_value(V, <<"str">>) -> V;
%coerce_value(V, <<"arr">>) -> rfc4627:decode(V);
%coerce_value(V, <<"obj">>) -> rfc4627:decode(V);
%coerce_value(V, <<"any">>) -> V;
%coerce_value(_, <<"nil">>) -> null;
%coerce_value(V, _) -> V.

add_functions(State) ->
    FunctionMap = State#state.rcp_services,
    NewFunctionMap = dict:store(<<"subtract">>, fun subtract/1, FunctionMap),
    {ok, State#state{rcp_services = NewFunctionMap}}.

subtract([N1, N2]) ->
    Diff = N1 - N2,
    io:format("subtract(~p, ~p) = ~p~n", [N1, N2, Diff]),
    {ok, Diff}.

error_response(Code, ErrorValue) when is_integer(Code) ->
    error_response(Code, "Error "++integer_to_list(Code), ErrorValue);
error_response(Message, ErrorValue) when is_list(Message) ->
    error_response(500, list_to_binary(Message), ErrorValue);
error_response(Message, ErrorValue) when is_binary(Message) ->
    error_response(500, Message, ErrorValue).

error_response(Code, Message, ErrorValue) when is_list(Message) ->
    error_response(Code, list_to_binary(Message), ErrorValue);
error_response(Code, Message, ErrorValue) ->
    {error, {obj, [{"name", <<"JSONRPCError">>},
		   {"code", Code},
		   {"message", Message},
		   {"error", ErrorValue}]}}.

build_jsonrpc_response(Id, ResultField) ->
    {obj, [{version, <<"1.1">>},
	   {id, Id},
	   ResultField]}.

expand_jsonrpc_reply(RequestId, {ResultOrError, Value}) ->
    {ResultOrError, build_jsonrpc_response(RequestId, {ResultOrError, Value}), {obj, []}};
expand_jsonrpc_reply(RequestId, {ResultOrError, Value, ResponseInfo}) ->
    {ResultOrError, build_jsonrpc_response(RequestId, {ResultOrError, Value}), ResponseInfo}.
