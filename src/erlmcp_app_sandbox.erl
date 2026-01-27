%%%-------------------------------------------------------------------
%% @doc MCP App Sandbox Manager - Security Isolation and Resource Control
%%
%% This module manages the sandbox environment for MCP Apps,
%% providing security isolation, resource limits, and restricted access
%% to server capabilities.
%%
%% Features:
%% - Iframe-based sandbox isolation
%% - Content Security Policy (CSP) headers
%% - Resource isolation and quotas
%% - Permission-based access control
%% - Communication via postMessage API
%% - State isolation between apps
%%
%% @author ErlMCP Development Team
%% @since 0.8.0
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_app_sandbox).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    create_sandbox/2,
    destroy_sandbox/1,
    get_sandbox/1,
    list_sandboxes/0,
    send_message/3,
    receive_message/2,
    update_sandbox_state/2,
    check_resource_access/2,
    get_csp_headers/1,
    validate_origin/2,
    isolate_permissions/2,
    generate_sandbox_id/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Type definitions (before records)
-type sandbox_id() :: binary().
-type app_id() :: binary().
-type sandbox_state() :: #{atom() => term()}.
-type message() :: map().
-type resource_quota() :: pos_integer().

%% Sandbox record
-record(sandbox, {
    id :: sandbox_id(),                          % Unique sandbox ID
    app_id :: app_id(),                          % Associated app ID
    origin :: binary(),                          % Sandbox origin URL
    created_at :: integer(),                     % Creation timestamp
    last_activity :: integer(),                  % Last activity timestamp
    message_queue = [] :: [message()],           % Outgoing messages
    state = #{} :: sandbox_state(),              % Sandbox-specific state
    memory_quota :: resource_quota(),            % Memory limit in MB
    cpu_limit :: resource_quota(),               % CPU time limit in ms
    storage_quota :: resource_quota(),           % Storage limit in MB
    permissions = sets:new() :: sets:set(binary()),  % Granted permissions
    trusted = false :: boolean()                 % Trusted sandbox flag
}).

%% Type for sandbox record (defined after record)
-type sandbox() :: #sandbox{}.

%% Internal state record
-record(state, {
    sandboxes = #{} :: #{sandbox_id() => sandbox()},     % All sandboxes
    app_sandboxes = #{} :: #{app_id() => sandbox_id()},  % App to sandbox map
    message_handlers = #{} :: map()              % Message handlers
}).

-type internal_state() :: #state{}.

%% Export types
-export_type([sandbox_id/0, sandbox_state/0, message/0, resource_quota/0, sandbox/0, app_id/0]).

%% Constants
-define(SANDBOX_PREFIX, <<"app-sandbox-">>).
-define(DEFAULT_MEMORY_QUOTA_MB, 256).
-define(DEFAULT_CPU_LIMIT_MS, 5000).
-define(DEFAULT_STORAGE_QUOTA_MB, 100).
-define(SANDBOX_TIMEOUT_MS, 30000).
-define(SANDBOX_INACTIVITY_TIMEOUT_MS, 300000).  % 5 minutes

%% CSP directives for sandbox isolation
-define(CSP_DIRECTIVES, [
    <<"script-src 'self' 'unsafe-inline'">>,
    <<"style-src 'self' 'unsafe-inline'">>,
    <<"img-src 'self' data:">>,
    <<"font-src 'self'">>,
    <<"connect-src 'self'">>,
    <<"frame-ancestors 'none'">>,
    <<"base-uri 'self'">>,
    <<"form-action 'none'">>,
    <<"default-src 'none'">>
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the sandbox manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a new sandbox for an application
-spec create_sandbox(app_id(), map()) -> {ok, sandbox_id()} | {error, term()}.
create_sandbox(AppId, Config) when is_binary(AppId), is_map(Config) ->
    gen_server:call(?MODULE, {create_sandbox, AppId, Config}, ?SANDBOX_TIMEOUT_MS).

%% @doc Destroy a sandbox and cleanup resources
-spec destroy_sandbox(sandbox_id()) -> ok | {error, term()}.
destroy_sandbox(SandboxId) when is_binary(SandboxId) ->
    gen_server:call(?MODULE, {destroy_sandbox, SandboxId}, ?SANDBOX_TIMEOUT_MS).

%% @doc Get a sandbox by ID
-spec get_sandbox(sandbox_id()) -> {ok, sandbox()} | {error, not_found}.
get_sandbox(SandboxId) when is_binary(SandboxId) ->
    gen_server:call(?MODULE, {get_sandbox, SandboxId}, ?SANDBOX_TIMEOUT_MS).

%% @doc List all active sandboxes
-spec list_sandboxes() -> [sandbox()].
list_sandboxes() ->
    gen_server:call(?MODULE, list_sandboxes, ?SANDBOX_TIMEOUT_MS).

%% @doc Send a message to a sandbox
-spec send_message(sandbox_id(), binary(), map()) -> ok | {error, term()}.
send_message(SandboxId, Method, Params) when is_binary(SandboxId), is_binary(Method) ->
    gen_server:call(?MODULE, {send_message, SandboxId, Method, Params}, ?SANDBOX_TIMEOUT_MS).

%% @doc Receive a message from sandbox
-spec receive_message(sandbox_id(), message()) -> ok | {error, term()}.
receive_message(SandboxId, Message) when is_binary(SandboxId), is_map(Message) ->
    gen_server:cast(?MODULE, {receive_message, SandboxId, Message}).

%% @doc Update sandbox state
-spec update_sandbox_state(sandbox_id(), sandbox_state()) -> ok | {error, not_found}.
update_sandbox_state(SandboxId, NewState) when is_binary(SandboxId), is_map(NewState) ->
    gen_server:call(?MODULE, {update_state, SandboxId, NewState}, ?SANDBOX_TIMEOUT_MS).

%% @doc Check if sandbox can access a resource
-spec check_resource_access(sandbox_id(), binary()) -> boolean().
check_resource_access(SandboxId, Resource) when is_binary(SandboxId), is_binary(Resource) ->
    try
        case get_sandbox(SandboxId) of
            {ok, Sandbox} ->
                sets:is_element(Resource, Sandbox#sandbox.permissions);
            {error, _} ->
                false
        end
    catch
        _:_ -> false
    end.

%% @doc Get CSP headers for a sandbox
-spec get_csp_headers(sandbox_id()) -> map().
get_csp_headers(SandboxId) when is_binary(SandboxId) ->
    CSP = binary_join(?CSP_DIRECTIVES, <<"; ">>),
    #{
        <<"Content-Security-Policy">> => CSP,
        <<"X-Content-Type-Options">> => <<"nosniff">>,
        <<"X-Frame-Options">> => <<"DENY">>,
        <<"X-XSS-Protection">> => <<"1; mode=block">>,
        <<"Referrer-Policy">> => <<"no-referrer">>
    }.

%% @doc Validate sandbox origin
-spec validate_origin(sandbox_id(), binary()) -> boolean().
validate_origin(SandboxId, Origin) when is_binary(SandboxId), is_binary(Origin) ->
    try
        case get_sandbox(SandboxId) of
            {ok, Sandbox} ->
                Sandbox#sandbox.origin =:= Origin;
            {error, _} ->
                false
        end
    catch
        _:_ -> false
    end.

%% @doc Isolate app permissions within sandbox context
-spec isolate_permissions(sandbox_id(), sets:set(binary())) -> sets:set(binary()).
isolate_permissions(SandboxId, GrantedPerms) when is_binary(SandboxId) ->
    try
        case get_sandbox(SandboxId) of
            {ok, Sandbox} ->
                sets:intersection(Sandbox#sandbox.permissions, GrantedPerms);
            {error, _} ->
                sets:new()
        end
    catch
        _:_ -> sets:new()
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, internal_state()}.
init([]) ->
    {ok, #state{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, internal_state()) ->
    {reply, term(), internal_state()}.

handle_call({create_sandbox, AppId, Config}, _From,
            #state{sandboxes = Sandboxes, app_sandboxes = AppMap} = State) ->
    SandboxId = erlmcp_app_sandbox:generate_sandbox_id(AppId),
    Origin = maps:get(<<"origin">>, Config, <<"mcp://localhost">>),
    MemQuota = maps:get(<<"memory_quota">>, Config, ?DEFAULT_MEMORY_QUOTA_MB),
    CpuLimit = maps:get(<<"cpu_limit">>, Config, ?DEFAULT_CPU_LIMIT_MS),
    StorageQuota = maps:get(<<"storage_quota">>, Config, ?DEFAULT_STORAGE_QUOTA_MB),
    Permissions = maps:get(<<"permissions">>, Config, sets:new()),

    Sandbox = #sandbox{
        id = SandboxId,
        app_id = AppId,
        origin = Origin,
        created_at = erlang:system_time(millisecond),
        last_activity = erlang:system_time(millisecond),
        memory_quota = MemQuota,
        cpu_limit = CpuLimit,
        storage_quota = StorageQuota,
        permissions = Permissions
    },

    NewSandboxes = maps:put(SandboxId, Sandbox, Sandboxes),
    NewAppMap = maps:put(AppId, SandboxId, AppMap),
    NewState = State#state{sandboxes = NewSandboxes, app_sandboxes = NewAppMap},
    {reply, {ok, SandboxId}, NewState};

handle_call({destroy_sandbox, SandboxId}, _From,
            #state{sandboxes = Sandboxes, app_sandboxes = AppMap} = State) ->
    case maps:find(SandboxId, Sandboxes) of
        {ok, Sandbox} ->
            NewSandboxes = maps:remove(SandboxId, Sandboxes),
            NewAppMap = maps:remove(Sandbox#sandbox.app_id, AppMap),
            NewState = State#state{sandboxes = NewSandboxes, app_sandboxes = NewAppMap},
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_sandbox, SandboxId}, _From, #state{sandboxes = Sandboxes} = State) ->
    case maps:find(SandboxId, Sandboxes) of
        {ok, Sandbox} ->
            UpdatedSandbox = Sandbox#sandbox{last_activity = erlang:system_time(millisecond)},
            NewSandboxes = maps:put(SandboxId, UpdatedSandbox, Sandboxes),
            {reply, {ok, UpdatedSandbox}, State#state{sandboxes = NewSandboxes}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_sandboxes, _From, #state{sandboxes = Sandboxes} = State) ->
    SandboxList = maps:values(Sandboxes),
    {reply, SandboxList, State};

handle_call({send_message, SandboxId, Method, Params}, _From,
            #state{sandboxes = Sandboxes} = State) ->
    case maps:find(SandboxId, Sandboxes) of
        {ok, Sandbox} ->
            Message = #{
                <<"method">> => Method,
                <<"params">> => Params,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            UpdatedQueue = Sandbox#sandbox.message_queue ++ [Message],
            UpdatedSandbox = Sandbox#sandbox{
                message_queue = UpdatedQueue,
                last_activity = erlang:system_time(millisecond)
            },
            NewSandboxes = maps:put(SandboxId, UpdatedSandbox, Sandboxes),
            {reply, ok, State#state{sandboxes = NewSandboxes}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_state, SandboxId, NewState}, _From,
            #state{sandboxes = Sandboxes} = State) ->
    case maps:find(SandboxId, Sandboxes) of
        {ok, Sandbox} ->
            UpdatedSandbox = Sandbox#sandbox{state = NewState},
            NewSandboxes = maps:put(SandboxId, UpdatedSandbox, Sandboxes),
            {reply, ok, State#state{sandboxes = NewSandboxes}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
-spec handle_cast(term(), internal_state()) -> {noreply, internal_state()}.

handle_cast({receive_message, SandboxId, Message}, #state{sandboxes = Sandboxes} = State) ->
    case maps:find(SandboxId, Sandboxes) of
        {ok, Sandbox} ->
            UpdatedSandbox = Sandbox#sandbox{
                last_activity = erlang:system_time(millisecond)
            },
            NewSandboxes = maps:put(SandboxId, UpdatedSandbox, Sandboxes),
            {noreply, State#state{sandboxes = NewSandboxes}};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), internal_state()) -> {noreply, internal_state()}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), internal_state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), internal_state(), term()) -> {ok, internal_state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate a unique sandbox ID
-spec generate_sandbox_id(binary()) -> binary().
generate_sandbox_id(AppId) ->
    Timestamp = integer_to_binary(erlang:system_time(nanosecond)),
    Random = base64:encode(crypto:strong_rand_bytes(4)),
    <<AppId/binary, "-sandbox-", Timestamp/binary, "-", Random/binary>>.

%% @private Join binary strings with separator
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([H|T], Sep) ->
    lists:foldl(fun(E, Acc) ->
        <<Acc/binary, Sep/binary, E/binary>>
    end, H, T).
