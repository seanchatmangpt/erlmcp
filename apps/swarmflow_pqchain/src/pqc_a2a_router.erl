%%%-------------------------------------------------------------------
%%% @doc A2A HTTP Router - HTTP Face over Case Kernel
%%%
%%% Provides HTTP endpoints for A2A protocol, routing to the
%%% pqc_projection_a2a module which projects onto Case reality.
%%%
%%% Endpoints:
%%% - POST /message:send -> send_message/1
%%% - POST /message:stream -> streaming version with SSE
%%% - GET /tasks -> list_tasks/1
%%% - GET /tasks/{id} -> get_task/1
%%% - POST /tasks/{id}:cancel -> cancel_task/1
%%% - POST /tasks/{id}:subscribe -> subscribe (WebSocket/SSE)
%%% - GET /.well-known/agent.json -> agent card with PQC keys
%%%
%%% Architecture:
%%% - Stateless HTTP handler
%%% - Uses jsx for JSON encoding/decoding
%%% - Supports SSE for streaming task updates
%%% - Agent card includes PQC public keys (ML-DSA, ML-KEM)
%%% - Can integrate with Cowboy or any HTTP server
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_a2a_router).

-include("pqchain.hrl").

%% API exports
-export([
    handle_request/3,
    dispatch/0,
    cowboy_init/2,
    cowboy_info/3,
    cowboy_terminate/3
]).

%% SSE handler exports
-export([
    sse_init/2,
    sse_info/3,
    sse_terminate/3
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type http_method() :: binary().
-type path() :: binary().
-type body() :: binary().
-type status_code() :: 100..599.
-type headers() :: [{binary(), binary()}].
-type response_body() :: binary().

-export_type([http_method/0, path/0, status_code/0, headers/0, response_body/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Handle HTTP request - main entry point
%%
%% Parses method, path, and body, routes to appropriate handler,
%% and returns HTTP response tuple.
%%
%% @end
-spec handle_request(Method :: http_method(), Path :: path(), Body :: body()) ->
    {StatusCode :: status_code(), Headers :: headers(), ResponseBody :: response_body()}.
handle_request(<<"POST">>, <<"/message:send">>, Body) ->
    handle_send_message(Body);

handle_request(<<"POST">>, <<"/message:stream">>, Body) ->
    handle_streaming_message(Body);

handle_request(<<"GET">>, <<"/tasks">>, _Body) ->
    handle_list_tasks();

handle_request(<<"GET">>, Path, _Body) ->
    %% Match /tasks/{id}
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, <<"tasks">>, TaskId] ->
            handle_get_task(TaskId);
        _ ->
            not_found_response()
    end;

handle_request(<<"POST">>, Path, _Body) ->
    %% Match /tasks/{id}:cancel or /tasks/{id}:subscribe
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, <<"tasks">>, TaskAction] ->
            case binary:split(TaskAction, <<":">>) of
                [TaskId, <<"cancel">>] ->
                    handle_cancel_task(TaskId);
                [TaskId, <<"subscribe">>] ->
                    handle_subscribe_task(TaskId);
                _ ->
                    not_found_response()
            end;
        _ ->
            not_found_response()
    end;

handle_request(<<"GET">>, <<"/.well-known/agent.json">>, _Body) ->
    handle_agent_card();

handle_request(_Method, _Path, _Body) ->
    not_found_response().

%% @doc Return Cowboy-compatible dispatch rules
-spec dispatch() -> cowboy_router:dispatch_rules().
dispatch() ->
    cowboy_router:compile([
        {'_', [
            {"/message:send", ?MODULE, #{handler => send_message}},
            {"/message:stream", pqc_a2a_sse_handler, #{}},
            {"/tasks", ?MODULE, #{handler => list_tasks}},
            {"/tasks/:task_id", ?MODULE, #{handler => get_task}},
            {"/tasks/:task_id/:action", ?MODULE, #{handler => task_action}},
            {"/.well-known/agent.json", ?MODULE, #{handler => agent_card}}
        ]}
    ]).

%%====================================================================
%% Cowboy Handler Callbacks
%%====================================================================

%% @doc Cowboy init callback
cowboy_init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    %% Read body
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    %% Handle request
    {StatusCode, Headers, ResponseBody} = handle_request(Method, Path, Body),

    %% Send response
    Req3 = cowboy_req:reply(StatusCode, maps:from_list(Headers), ResponseBody, Req2),

    {ok, Req3, State}.

%% @doc Cowboy info callback (for async messages)
cowboy_info(_Info, Req, State) ->
    {ok, Req, State}.

%% @doc Cowboy terminate callback
cowboy_terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% SSE Handler for Streaming
%%====================================================================

%% @doc SSE handler init
sse_init(Req, _State) ->
    %% Set SSE headers
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>
    },

    Req2 = cowboy_req:stream_reply(200, Headers, Req),

    %% Extract task ID from query params
    TaskId = cowboy_req:binding(task_id, Req2, undefined),

    case TaskId of
        undefined ->
            {ok, Req2, #{}};
        _ ->
            %% Subscribe to task updates
            ok = pqc_projection_a2a:subscribe_task(TaskId, self()),
            {cowboy_loop, Req2, #{task_id => TaskId}}
    end.

%% @doc SSE handler info - handles Case events
sse_info({pqc_case_event, _CaseId, Event}, Req, State) ->
    %% Convert Case event to A2A message
    A2AMessage = pqc_projection_a2a:case_to_a2a_message(Event),

    %% Encode as SSE event
    EventData = jsx:encode(A2AMessage),
    SSEData = [
        <<"event: message\n">>,
        <<"data: ">>, EventData, <<"\n\n">>
    ],

    %% Stream to client
    cowboy_req:stream_body(SSEData, nofin, Req),

    {ok, Req, State};

sse_info(_Info, Req, State) ->
    {ok, Req, State}.

%% @doc SSE handler terminate
sse_terminate(_Reason, _Req, #{task_id := TaskId}) ->
    %% Unsubscribe from task
    pqc_projection_a2a:unsubscribe_task(TaskId, self()),
    ok;
sse_terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Request Handlers
%%====================================================================

%% @private Handle POST /message:send
handle_send_message(Body) ->
    try
        %% Parse JSON body
        ReqMap = jsx:decode(Body, [return_maps]),

        %% Call projection
        case pqc_projection_a2a:send_message(ReqMap) of
            {ok, TaskMap} ->
                %% Return A2A task
                ResponseBody = jsx:encode(#{<<"task">> => TaskMap}),
                {200, json_headers(), ResponseBody};
            {error, Reason} ->
                error_response(400, Reason)
        end
    catch
        error:badarg ->
            error_response(400, invalid_json);
        error:Reason:_Stack ->
            error_response(500, Reason)
    end.

%% @private Handle POST /message:stream
handle_streaming_message(Body) ->
    %% Return SSE stream URL
    try
        ReqMap = jsx:decode(Body, [return_maps]),
        Message = maps:get(<<"message">>, ReqMap),
        TaskId = maps:get(<<"taskId">>, Message, undefined),

        %% Create task first
        case pqc_projection_a2a:send_message(ReqMap) of
            {ok, TaskMap} ->
                ActualTaskId = maps:get(<<"id">>, TaskMap),
                StreamURL = <<"/tasks/", ActualTaskId/binary, "/stream">>,

                ResponseBody = jsx:encode(#{
                    <<"task">> => TaskMap,
                    <<"streamUrl">> => StreamURL
                }),
                {200, json_headers(), ResponseBody};
            {error, Reason} ->
                error_response(400, Reason)
        end
    catch
        error:badarg ->
            error_response(400, invalid_json);
        error:Reason ->
            error_response(500, Reason)
    end.

%% @private Handle GET /tasks
handle_list_tasks() ->
    try
        %% TODO: Parse query parameters
        Params = #{},

        case pqc_projection_a2a:list_tasks(Params) of
            {ok, ResultMap} ->
                ResponseBody = jsx:encode(ResultMap),
                {200, json_headers(), ResponseBody};
            {error, Reason} ->
                error_response(500, Reason)
        end
    catch
        error:Reason ->
            error_response(500, Reason)
    end.

%% @private Handle GET /tasks/{id}
handle_get_task(TaskId) ->
    try
        case pqc_projection_a2a:get_task(TaskId) of
            {ok, TaskMap} ->
                ResponseBody = jsx:encode(TaskMap),
                {200, json_headers(), ResponseBody};
            {error, task_not_found} ->
                error_response(404, task_not_found);
            {error, Reason} ->
                error_response(500, Reason)
        end
    catch
        error:Reason ->
            error_response(500, Reason)
    end.

%% @private Handle POST /tasks/{id}:cancel
handle_cancel_task(TaskId) ->
    try
        case pqc_projection_a2a:cancel_task(TaskId) of
            ok ->
                ResponseBody = jsx:encode(#{<<"status">> => <<"canceled">>}),
                {200, json_headers(), ResponseBody};
            {error, task_not_found} ->
                error_response(404, task_not_found);
            {error, Reason} ->
                error_response(500, Reason)
        end
    catch
        error:Reason ->
            error_response(500, Reason)
    end.

%% @private Handle POST /tasks/{id}:subscribe
handle_subscribe_task(TaskId) ->
    try
        %% Return SSE stream URL
        StreamURL = <<"/tasks/", TaskId/binary, "/stream">>,
        ResponseBody = jsx:encode(#{
            <<"streamUrl">> => StreamURL,
            <<"protocol">> => <<"SSE">>
        }),
        {200, json_headers(), ResponseBody}
    catch
        error:Reason ->
            error_response(500, Reason)
    end.

%% @private Handle GET /.well-known/agent.json
handle_agent_card() ->
    try
        %% Get PQC public keys
        AgentCard = create_agent_card(),
        ResponseBody = jsx:encode(AgentCard),
        {200, json_headers(), ResponseBody}
    catch
        error:Reason ->
            error_response(500, Reason)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Create agent card with PQC public keys
create_agent_card() ->
    %% Get PQC keys from configuration or generate
    {MLDSAPublicKey, MLKEMPublicKey} = get_pqc_keys(),

    #{
        <<"name">> => <<"PQChain A2A Agent">>,
        <<"description">> => <<"Post-quantum blockchain agent with Case kernel">>,
        <<"version">> => <<"1.0.0">>,
        <<"supportedInterfaces">> => [
            #{
                <<"url">> => <<"https://localhost:8080">>,
                <<"protocolBinding">> => <<"HTTP+JSON">>,
                <<"protocolVersion">> => <<"0.3">>
            }
        ],
        <<"capabilities">> => #{
            <<"streaming">> => true,
            <<"pushNotifications">> => false,
            <<"extendedAgentCard">> => false
        },
        <<"defaultInputModes">> => [
            <<"text/plain">>,
            <<"application/json">>
        ],
        <<"defaultOutputModes">> => [
            <<"text/plain">>,
            <<"application/json">>
        ],
        <<"skills">> => [
            #{
                <<"id">> => <<"message_processing">>,
                <<"name">> => <<"Message Processing">>,
                <<"description">> => <<"Process messages with Case kernel">>,
                <<"tags">> => [<<"workflow">>, <<"post-quantum">>, <<"blockchain">>],
                <<"examples">> => [
                    <<"Send a message to process">>
                ]
            }
        ],
        <<"pqcKeys">> => #{
            <<"mlDsa">> => #{
                <<"algorithm">> => <<"ML-DSA-65">>,
                <<"publicKey">> => base64:encode(MLDSAPublicKey)
            },
            <<"mlKem">> => #{
                <<"algorithm">> => <<"ML-KEM-768">>,
                <<"publicKey">> => base64:encode(MLKEMPublicKey)
            }
        },
        <<"metadata">> => #{
            <<"runtime">> => <<"SwarmFlow OS">>,
            <<"kernel">> => <<"Case">>,
            <<"protocol">> => <<"A2A 0.3">>
        }
    }.

%% @private Get PQC public keys
get_pqc_keys() ->
    %% TODO: Get from configuration or key management service
    %% For now, generate placeholder keys
    MLDSAKey = crypto:strong_rand_bytes(32),
    MLKEMKey = crypto:strong_rand_bytes(32),
    {MLDSAKey, MLKEMKey}.

%% @private JSON response headers
json_headers() ->
    [
        {<<"content-type">>, <<"application/json">>},
        {<<"cache-control">>, <<"no-cache">>}
    ].

%% @private 404 Not Found response
not_found_response() ->
    ErrorBody = jsx:encode(#{
        <<"error">> => <<"not_found">>,
        <<"message">> => <<"Endpoint not found">>
    }),
    {404, json_headers(), ErrorBody}.

%% @private Error response
error_response(StatusCode, Reason) ->
    ErrorBody = jsx:encode(#{
        <<"error">> => format_error(Reason),
        <<"message">> => format_error_message(Reason)
    }),
    {StatusCode, json_headers(), ErrorBody}.

%% @private Format error reason
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error({Type, _Details}) when is_atom(Type) ->
    atom_to_binary(Type, utf8);
format_error(_Reason) ->
    <<"internal_error">>.

%% @private Format error message
format_error_message(invalid_json) ->
    <<"Invalid JSON in request body">>;
format_error_message(task_not_found) ->
    <<"Task not found">>;
format_error_message(invalid_request) ->
    <<"Invalid request format">>;
format_error_message({case_creation_failed, Reason}) ->
    iolist_to_binary(io_lib:format("Case creation failed: ~p", [Reason]));
format_error_message(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error_message(_Reason) ->
    <<"Internal server error">>.
