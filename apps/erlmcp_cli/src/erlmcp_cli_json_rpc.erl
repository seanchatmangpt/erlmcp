%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_json_rpc - JSON-RPC 2.0 Handler
%%%
%%% Implements JSON-RPC 2.0 protocol for CLI commands with proper
%%% request ID correlation and error handling.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_json_rpc).

-behaviour(gen_server).

%% API
-export([start_link/0, handle_json_rpc/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(request_state,
        {pending_requests :: map(),  % Request ID -> from pid
         metrics :: map()}).

-define(SERVER, ?MODULE).
-define(DEFAULT_STATE,
        #{pending_requests => #{},
          metrics =>
              #{"requests.total" => 0,
                "requests.success" => 0,
                "requests.failed" => 0,
                "errors.parse" => 0,
                "errors.invalid_request" => 0,
                "errors.method_not_found" => 0,
                "errors.invalid_params" => 0,
                "errors.internal" => 0}}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the JSON-RPC server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Handle JSON-RPC request
-spec handle_json_rpc(binary(), map(), binary()) -> {ok, map()} | {error, term()}.
handle_json_rpc(JsonData, Headers, SessionId) ->
    gen_server:call(?SERVER, {handle_json_rpc, JsonData, Headers, SessionId}, 30000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the server
-spec init(term()) -> {ok, #request_state{}}.
init( _Args ) -> erlmcp_otel : with_span( "cli.json_rpc.init" , #{ } , fun ( ) -> ok end ) , State = #request_state{ pending_requests = #{ } , metrics = ?DEFAULT_STATE #{ metrics } } , erlmcp_metrics : record( "cli.json_rpc.initialized" , 1 ) , { ok , State } .
    %% Create OTEL span for JSON-RPC server initialization
                                                                                                                                                                                                                                                                          %% Initialize state
    %% Start metrics collection

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #request_state{}) -> {reply, term(), #request_state{}}.
handle_call({handle_json_rpc, JsonData, Headers, SessionId}, From, State) ->
    %% Create OTEL span for JSON-RPC processing
    SpanCtx =
        erlmcp_otel:inject_rpc_span(<<"cli.json_rpc.handle">>,
                                    make_request_id(),
                                    #{<<"session">> => SessionId,
                                      <<"headers.size">> => map_size(Headers),
                                      <<"payload.size">> => size(JsonData)},
                                    undefined),

    %% Parse JSON-RPC request
    case parse_json_rpc(JsonData, SpanCtx) of
        {ok, Request} ->
            %% Process request
            process_json_rpc_request(Request, Headers, SessionId, SpanCtx, From, State);
        {error, ParseError} ->
            %% Send JSON-RPC error response
            Response = make_error_response(ParseError),
            erlmcp_otel:record_error(SpanCtx, {parse_error, ParseError}),
            erlmcp_metrics:record("cli.json_rpc.errors.parse", 1),

            %% Update state metrics
            UpdatedMetrics =
                maps:update_with("errors.parse", fun(V) -> V + 1 end, State#request_state.metrics),
            UpdatedState = State#request_state{metrics = UpdatedMetrics},

            {reply, {ok, Response}, UpdatedState}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #request_state{}) -> {noreply, #request_state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #request_state{}) -> {noreply, #request_state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #request_state{}) -> ok.
terminate(_Reason, State) ->
    %% Cancel all pending requests
    lists:foreach(fun({RequestId, From}) -> gen_server:reply(From, {error, server_shutdown}) end,
                  maps:to_list(State#request_state.pending_requests)),

    %% Send final metrics
    erlmcp_metrics:record("cli.json_rpc.terminated", 1),

    ok.

%% @doc Handle code changes
-spec code_change(term(), #request_state{}, term()) -> {ok, #request_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Parse JSON-RPC request
-spec parse_json_rpc(binary(), term()) -> {ok, map()} | {error, term()}.
parse_json_rpc(JsonData, SpanCtx) ->
    try
        %% Parse JSON
        JsonDataDecoded = jsx:decode(JsonData, [{labels, binary}, return_maps]),

        %% Validate JSON-RPC 2.0 structure
        case validate_json_rpc_request(JsonDataDecoded) of
            {ok, Request} ->
                %% Add parsed request to span
                erlmcp_otel:add_event(SpanCtx,
                                      <<"json_rpc.parsed">>,
                                      #{<<"method">> => maps:get(<<"method">>, Request, undefined),
                                        <<"params">> => maps:get(<<"params">>, Request, undefined),
                                        <<"id">> => maps:get(<<"id">>, Request, undefined)}),

                {ok, Request};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Error:Reason:Stacktrace ->
            erlmcp_otel:record_error(SpanCtx, {json_parse_error, Error, Reason, Stacktrace}),
            {error, {parse_failed, Reason}}
    end.

%% @doc Validate JSON-RPC 2.0 request
-spec validate_json_rpc_request(map()) -> {ok, map()} | {error, term()}.
validate_json_rpc_request(Request) ->
    %% Check required fields
    case maps:find(<<"jsonrpc">>, Request) of
        {ok, <<"2.0">>} ->
            %% JSON-RPC version is correct
            case maps:find(<<"method">>, Request) of
                {ok, Method} when is_binary(Method) ->
                    %% Method is valid
                    Id = maps:get(<<"id">>, Request, null),
                    Params = maps:get(<<"params">>, Request, null),

                    {ok,
                     #{jsonrpc => <<"2.0">>,
                       method => Method,
                       params => Params,
                       id => Id}};
                {ok, _} ->
                    {error, {invalid_request, invalid_method}};
                error ->
                    {error, {invalid_request, missing_method}}
            end;
        {ok, _} ->
            {error, {invalid_request, invalid_jsonrpc_version}};
        error ->
            {error, {invalid_request, missing_jsonrpc_version}}
    end.

%% @doc Process JSON-RPC request
-spec process_json_rpc_request(map(), map(), binary(), term(), {pid(), term()}, #request_state{}) ->
                                  {reply, term(), #request_state{}}.
process_json_rpc_request(Request, Headers, SessionId, SpanCtx, From, State) ->
    Method = maps:get(<<"method">>, Request),
    Params = maps:get(<<"params">>, Request),
    RequestId = maps:get(<<"id">>, Request),

    %% Store pending request
    PendingRequests = maps:put(RequestId, From, State#request_state.pending_requests),

    %% Update metrics
    Metrics = maps:update_with("requests.total", fun(V) -> V + 1 end, State#request_state.metrics),

    %% Create span for method execution
    MethodSpanCtx =
        erlmcp_otel:with_span("cli.json_rpc.method",
                              #{<<"method">> => Method,
                                <<"session">> => SessionId,
                                <<"request_id">> => RequestId},
                              fun() ->
                                 execute_json_rpc_method(Method,
                                                         Params,
                                                         Headers,
                                                         SessionId,
                                                         RequestId,
                                                         SpanCtx)
                              end),

    %% Update state with pending request
    UpdatedState = State#request_state{pending_requests = PendingRequests, metrics = Metrics},

    {noreply, UpdatedState}.

%% @doc Execute JSON-RPC method
-spec execute_json_rpc_method(binary(), term(), map(), binary(), binary(), term()) -> ok.
execute_json_rpc_method(Method, Params, Headers, SessionId, RequestId, SpanCtx) ->
    try
        %% Parse parameters
        Args =
            case Params of
                null ->
                    [];
                Array when is_list(Array) ->
                    Array;
                Object when is_map(Object) ->
                    [Object];
                _ ->
                    [Params]
            end,

        %% Add headers to args if needed
        case maps:get(<<"include-headers">>, Headers, false) of
            true ->
                ArgsWithHeaders = Args ++ [Headers],
                execute_cli_method(Method, ArgsWithHeaders, SessionId, RequestId, SpanCtx);
            false ->
                execute_cli_method(Method, Args, SessionId, RequestId, SpanCtx)
        end
    catch
        Class:Reason:Stacktrace ->
            erlmcp_otel:record_error(SpanCtx, {method_execution_failed, Class, Reason, Stacktrace}),

            %% Send error response
            Response = make_error_response({invalid_params, {Class, Reason}}),
            gen_server:cast(?SERVER, {send_response, RequestId, Response}),

            ok
    end.

%% @doc Execute CLI method
-spec execute_cli_method(binary(), list(), binary(), binary(), term()) -> ok.
execute_cli_method( Method , Args , SessionId , RequestId , SpanCtx ) -> case erlmcp_cli_registry : lookup_command( Method ) of { ok , CommandInfo } -> Result = CommandInfo #command_info .
module : execute( Args ) , Response = make_success_response( RequestId , Result ) , gen_server : cast( ?SERVER , { send_response , RequestId , Response } ) , erlmcp_metrics : record( "cli.json_rpc.requests.success" , 1 ) ; { error , not_found } -> Response = make_error_response( { method_not_found , Method } ) , gen_server : cast( ?SERVER , { send_response , RequestId , Response } ) , erlmcp_otel : record_error( SpanCtx , { method_not_found , Method } ) , erlmcp_metrics : record( "cli.json_rpc.errors.method_not_found" , 1 ) ; { error , Reason } -> Response = make_error_response( { internal_error , Reason } ) , gen_server : cast( ?SERVER , { send_response , RequestId , Response } ) , erlmcp_otel : record_error( SpanCtx , { internal_error , Reason } ) , erlmcp_metrics : record( "cli.json_rpc.errors.internal" , 1 ) end .
    %% Look up command in registry
            %% Execute command

            %% Send success response

            %% Record success metrics

            %% Send method not found error

            %% Send internal error

%% @doc Send JSON-RPC response
-spec send_response(binary(), map()) -> ok.
send_response(RequestId, Response) ->
    %% Find pending request
    case erlang:get({pending_request, RequestId}) of
        undefined ->
            lager:warning("No pending request for ID: ~p", [RequestId]);
        From ->
            gen_server:reply(From, {ok, Response}),
            %% Remove from pending requests
            erlang:erase({pending_request, RequestId})
    end.

%% @doc Make success response
-spec make_success_response(binary(), term()) -> map().
make_success_response(RequestId, Result) ->
    #{<<"jsonrpc">> => <<"2.0">>,
      <<"result">> => Result,
      <<"id">> => RequestId}.

%% @doc Make error response
-spec make_error_response(term()) -> map().
make_error_response( { parse_error , Reason } ) -> #{ << "jsonrpc" >> => << "2.0" >> , << "error" >> => #{ << "code" >> => - 32700 , / / Parse error << "message" >> => << "Parse error" >> , << "data" >> => format_error( Reason ) } , << "id" >> => null } ; make_error_response( { invalid_request , Reason } ) -> #{ << "jsonrpc" >> => << "2.0" >> , << "error" >> => #{ << "code" >> => - 32600 , / / Invalid Request << "message" >> => << "Invalid Request" >> , << "data" >> => format_error( Reason ) } , << "id" >> => null } ; make_error_response( { method_not_found , Method } ) -> #{ << "jsonrpc" >> => << "2.0" >> , << "error" >> => #{ << "code" >> => - 32601 , / / Method not found << "message" >> => << "Method not found" >> , << "data" >> => #{ << "method" >> => Method } } , << "id" >> => null } ; make_error_response( { invalid_params , Reason } ) -> #{ << "jsonrpc" >> => << "2.0" >> , << "error" >> => #{ << "code" >> => - 32602 , / / Invalid params << "message" >> => << "Invalid params" >> , << "data" >> => format_error( Reason ) } , << "id" >> => null } ; make_error_response( { internal_error , Reason } ) -> #{ << "jsonrpc" >> => << "2.0" >> , << "error" >> => #{ << "code" >> => - 32603 , / / Internal error << "message" >> => << "Internal error" >> , << "data" >> => format_error( Reason ) } , << "id" >> => null } .

%% @doc Format error
-spec format_error(term()) -> binary().
format_error(Error) ->
    list_to_binary(io_lib:format("~p", [Error])).

%% @doc Make request ID
-spec make_request_id() -> binary().
make_request_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).
