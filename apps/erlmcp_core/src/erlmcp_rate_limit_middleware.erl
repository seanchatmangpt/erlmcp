%%%-------------------------------------------------------------------
%% @doc Rate Limit Middleware Module
%%
%% This module implements middleware for automatic rate limiting
%% of MCP requests. It intercepts requests before processing and
%% returns 429 Too Many Requests if limits are exceeded.
%%
%% Features:
%% - Automatic client identification
%% - Per-method rate limits
%% - Priority-based limiting
%% - Retry-After header injection
%% - Transparent passthrough for high-priority clients
%%
%% Usage:
%% ```erlang
%% % In your request handler
%% case erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs) of
%%     {ok, _Remaining} ->
%%         % Process request normally
%%         handle_request(Method, Params);
%%     {error, rate_limited, RetryAfter} ->
%%         % Return 429 error
%%         {error, ?MCP_ERROR_RATE_LIMITED, <<"Rate limit exceeded">>, #{
%%             <<"retry_after_ms">> => RetryAfter
%%         }}
%% end
%% ```
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_rate_limit_middleware).

-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    stop/0,
    check_request/3,
    check_request/4,
    add_rate_limit/2,
    remove_rate_limit/1,
    get_rate_limits/0,
    extract_client_id/1,
    inject_retry_after/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type client_id() :: term().
-type method() :: binary().
-type rate_limit_config() :: #{
    method => binary(),
    max_rate => pos_integer(),
    scope => per_client | per_tool | global
}.

%% State record
-record(state, {
    method_limits :: #{binary() => rate_limit_config()},
    default_limit :: pos_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Check if request should be rate limited
%% @param ClientId - Client identifier
%% @param Method - MCP method being called
%% @param TimeMs - Current time in milliseconds
%% @returns {ok, Remaining} | {error, rate_limited, RetryAfterMs}
-spec check_request(client_id(), method(), integer()) ->
    {ok, non_neg_integer()} | {error, rate_limited, pos_integer()}.
check_request(ClientId, Method, TimeMs) ->
    check_request(ClientId, Method, TimeMs, normal).

%% @doc Check request with priority
-spec check_request(client_id(), method(), integer(), erlmcp_rate_limiter:priority()) ->
    {ok, non_neg_integer()} | {error, rate_limited, pos_integer()}.
check_request(ClientId, Method, TimeMs, Priority) ->
    gen_server:call(?MODULE, {check_request, ClientId, Method, TimeMs, Priority}).

%% @doc Add per-method rate limit
%% @param Method - MCP method name
%% @param Config - Rate limit configuration
%% @returns ok
-spec add_rate_limit(method(), rate_limit_config()) -> ok.
add_rate_limit(Method, Config) ->
    gen_server:call(?MODULE, {add_rate_limit, Method, Config}).

%% @doc Remove rate limit for method
%% @param Method - MCP method name
%% @returns ok
-spec remove_rate_limit(method()) -> ok.
remove_rate_limit(Method) ->
    gen_server:call(?MODULE, {remove_rate_limit, Method}).

%% @doc Get all configured rate limits
%% @returns Map of method to config
-spec get_rate_limits() -> #{binary() => rate_limit_config()}.
get_rate_limits() ->
    gen_server:call(?MODULE, get_rate_limits).

%% @doc Extract client ID from request
%% @param Request - MCP request map
%% @returns ClientId
-spec extract_client_id(map()) -> client_id().
extract_client_id(#{<<"_client_ip">> := IP}) ->
    IP;
extract_client_id(#{<<"_session_id">> := SessionId}) ->
    SessionId;
extract_client_id(_) ->
    <<"unknown">>.

%% @doc Inject Retry-After header into error response
%% @param ErrorResponse - Original error response
%% @param RetryAfterMs - Retry after duration in milliseconds
%% @returns Updated error response
-spec inject_retry_after(map(), pos_integer()) -> map().
inject_retry_after(ErrorResponse, RetryAfterMs) ->
    RetryAfterSec = max(1, RetryAfterMs div 1000),
    ErrorResponse#{
        <<"_headers">> => #{
            <<"Retry-After">> => integer_to_binary(RetryAfterSec)
        }
    }.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    % Load default method limits from config
    MethodLimits = load_method_limits(),
    DefaultLimit = application:get_env(erlmcp, default_rate_limit, 100),

    logger:info("Rate limit middleware started with ~p method limits", [maps:size(MethodLimits)]),

    State = #state{
        method_limits = MethodLimits,
        default_limit = DefaultLimit
    },
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call({check_request, ClientId, Method, TimeMs, Priority}, _From, State) ->
    % Get rate limit config for this method
    Config = maps:get(Method, State#state.method_limits, #{
        max_rate => State#state.default_limit,
        scope => per_client
    }),

    Scope = maps:get(scope, Config, per_client),

    % Check appropriate rate limit based on scope
    Result = case Scope of
        global ->
            erlmcp_rate_limiter:check_global_rate(TimeMs);
        per_tool ->
            erlmcp_rate_limiter:check_tool_call_rate(ClientId, TimeMs);
        per_client ->
            erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs, Priority)
    end,

    {reply, Result, State};

handle_call({add_rate_limit, Method, Config}, _From, State) ->
    NewLimits = maps:put(Method, Config, State#state.method_limits),
    logger:info("Added rate limit for method ~s: ~p", [Method, Config]),
    {reply, ok, State#state{method_limits = NewLimits}};

handle_call({remove_rate_limit, Method}, _From, State) ->
    NewLimits = maps:remove(Method, State#state.method_limits),
    logger:info("Removed rate limit for method ~s", [Method]),
    {reply, ok, State#state{method_limits = NewLimits}};

handle_call(get_rate_limits, _From, State) ->
    {reply, State#state.method_limits, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Load method-specific rate limits from config
-spec load_method_limits() -> #{binary() => rate_limit_config()}.
load_method_limits() ->
    case application:get_env(erlmcp, method_rate_limits) of
        {ok, Limits} when is_map(Limits) ->
            Limits;
        {ok, Limits} when is_list(Limits) ->
            maps:from_list(Limits);
        _ ->
            % Default limits for expensive operations
            #{
                ?MCP_METHOD_TOOLS_CALL => #{
                    max_rate => 50,
                    scope => per_tool
                },
                ?MCP_METHOD_RESOURCES_READ => #{
                    max_rate => 100,
                    scope => per_client
                },
                ?MCP_METHOD_RESOURCES_SUBSCRIBE => #{
                    max_rate => 20,
                    scope => per_client
                }
            }
    end.
