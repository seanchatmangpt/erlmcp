-module(erlmcp_elicitation).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behavior(gen_server).

%% API exports
-export([
    start_link/0,
    create_form/3,
    create_form/4,
    create_url_elicitation/3,
    create_url_elicitation/4,
    get_elicitation/1,
    submit_response/2,
    cancel_elicitation/1,
    validate_form_timeout/1,
    get_form_timeout_config/0
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Cowboy handler
-export([
    elicitation_handler/3
]).

-define(ELICITATION_TABLE, elicitations).

%% Default timeout values (in milliseconds)
%% Gap #38: Form timeout validation with configurable bounds
-define(DEFAULT_FORM_TIMEOUT_MS, 600000).  %% 10 minutes default
-define(MIN_FORM_TIMEOUT_MS, 1000).        %% 1 second minimum
-define(MAX_FORM_TIMEOUT_MS, 300000).      %% 5 minutes maximum (from MCP spec)

-record(state, {
    server_pid :: pid() | undefined,
    handlers = #{} :: map()
}).

-record(elicitation, {
    id :: binary(),
    type :: form | url,
    title :: binary(),
    description :: binary() | undefined,
    schema :: map() | undefined,
    url :: binary() | undefined,
    created_at :: integer(),
    timeout_ms :: pos_integer(),           %% Gap #38: Form timeout in milliseconds
    expires_at :: integer(),
    response :: term() | undefined,
    status :: pending | completed | cancelled
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec create_form(pid(), binary(), map()) -> {ok, binary()} | {error, term()}.
create_form(ServerPid, Title, Schema) when is_pid(ServerPid), is_binary(Title), is_map(Schema) ->
    create_form(ServerPid, Title, Schema, undefined).

-spec create_form(pid(), binary(), map(), pos_integer() | undefined) -> {ok, binary()} | {error, term()}.
create_form(ServerPid, Title, Schema, TimeoutMs)
  when is_pid(ServerPid), is_binary(Title), is_map(Schema),
       (TimeoutMs =:= undefined orelse is_integer(TimeoutMs)) ->
    gen_server:call(?MODULE, {create_form, ServerPid, Title, Schema, TimeoutMs}, 5000).

-spec create_url_elicitation(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}.
create_url_elicitation(ServerPid, Title, Url) when is_pid(ServerPid), is_binary(Title), is_binary(Url) ->
    create_url_elicitation(ServerPid, Title, Url, undefined).

-spec create_url_elicitation(pid(), binary(), binary(), pos_integer() | undefined) -> {ok, binary()} | {error, term()}.
create_url_elicitation(ServerPid, Title, Url, TimeoutMs)
  when is_pid(ServerPid), is_binary(Title), is_binary(Url),
       (TimeoutMs =:= undefined orelse is_integer(TimeoutMs)) ->
    gen_server:call(?MODULE, {create_url, ServerPid, Title, Url, TimeoutMs}, 5000).

-spec get_elicitation(binary()) -> {ok, map()} | {error, not_found}.
get_elicitation(ElicitationId) when is_binary(ElicitationId) ->
    SpanCtx = erlmcp_tracing:start_span(<<"elicitation.get">>),
    try
        case ets:lookup(?ELICITATION_TABLE, ElicitationId) of
            [#elicitation{} = Elicitation] ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, elicitation_to_map(Elicitation)};
            [] ->
                erlmcp_tracing:record_error_details(SpanCtx, not_found, ElicitationId),
                {error, not_found}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, not_found}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec submit_response(binary(), term()) -> {ok, binary()} | {error, term()}.
submit_response(ElicitationId, Response) when is_binary(ElicitationId) ->
    SpanCtx = erlmcp_tracing:start_span(<<"elicitation.submit_response">>),
    try
        case ets:lookup(?ELICITATION_TABLE, ElicitationId) of
            [Elicitation] ->
                UpdatedElicitation = Elicitation#elicitation{
                    response = Response,
                    status = completed
                },
                ets:insert(?ELICITATION_TABLE, UpdatedElicitation),

                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"elicitation_id">> => ElicitationId,
                    <<"status">> => <<"completed">>
                }),

                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, ElicitationId};
            [] ->
                erlmcp_tracing:record_error_details(SpanCtx, not_found, ElicitationId),
                {error, not_found}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec cancel_elicitation(binary()) -> {ok, binary()} | {error, term()}.
cancel_elicitation(ElicitationId) when is_binary(ElicitationId) ->
    SpanCtx = erlmcp_tracing:start_span(<<"elicitation.cancel">>),
    try
        case ets:lookup(?ELICITATION_TABLE, ElicitationId) of
            [Elicitation] ->
                UpdatedElicitation = Elicitation#elicitation{
                    status = cancelled
                },
                ets:insert(?ELICITATION_TABLE, UpdatedElicitation),

                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, ElicitationId};
            [] ->
                erlmcp_tracing:record_error_details(SpanCtx, not_found, ElicitationId),
                {error, not_found}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec validate_form_timeout(pos_integer() | undefined) -> {ok, pos_integer()} | {error, {atom(), term()}}.
%% Gap #38: Form Timeout Validation
%% Validates form timeout values against MCP 2025-11-25 specification
%% - Returns ok with default timeout if undefined
%% - Validates positive integer bounds (min 1s, max 5m default)
%% - Returns error for invalid values
validate_form_timeout(undefined) ->
    {ok, get_default_form_timeout()};
validate_form_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs > 0 ->
    {MinMs, MaxMs} = get_form_timeout_bounds(),
    case TimeoutMs of
        _ when TimeoutMs < MinMs ->
            {error, {timeout_too_small, #{
                requested => TimeoutMs,
                minimum => MinMs,
                message => <<"Form timeout must be at least 1 second (1000 milliseconds)">>
            }}};
        _ when TimeoutMs > MaxMs ->
            {error, {timeout_too_large, #{
                requested => TimeoutMs,
                maximum => MaxMs,
                message => <<"Form timeout must not exceed 5 minutes (300000 milliseconds)">>
            }}};
        _ ->
            {ok, TimeoutMs}
    end;
validate_form_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs =< 0 ->
    {error, {invalid_timeout, #{
        requested => TimeoutMs,
        message => <<"Form timeout must be a positive integer (milliseconds)">>
    }}};
validate_form_timeout(Invalid) ->
    {error, {invalid_timeout, #{
        requested => Invalid,
        message => <<"Form timeout must be a positive integer or undefined (milliseconds)">>
    }}}.

-spec get_form_timeout_config() -> map().
%% Get current form timeout configuration
get_form_timeout_config() ->
    {MinMs, MaxMs} = get_form_timeout_bounds(),
    Default = get_default_form_timeout(),
    #{
        default_ms => Default,
        min_ms => MinMs,
        max_ms => MaxMs,
        config_source => get_config_source()
    }.

%% Internal helper functions
-spec get_default_form_timeout() -> pos_integer().
get_default_form_timeout() ->
    case application:get_env(erlmcp, form_timeout_ms) of
        {ok, Timeout} when is_integer(Timeout), Timeout > 0 -> Timeout;
        _ -> ?DEFAULT_FORM_TIMEOUT_MS
    end.

-spec get_form_timeout_bounds() -> {pos_integer(), pos_integer()}.
get_form_timeout_bounds() ->
    MinMs = case application:get_env(erlmcp, form_timeout_min_ms) of
        {ok, Min} when is_integer(Min), Min > 0 -> Min;
        _ -> ?MIN_FORM_TIMEOUT_MS
    end,
    MaxMs = case application:get_env(erlmcp, form_timeout_max_ms) of
        {ok, Max} when is_integer(Max), Max > 0 -> Max;
        _ -> ?MAX_FORM_TIMEOUT_MS
    end,
    {MinMs, MaxMs}.

-spec get_config_source() -> atom().
get_config_source() ->
    case {application:get_env(erlmcp, form_timeout_ms),
          application:get_env(erlmcp, form_timeout_min_ms),
          application:get_env(erlmcp, form_timeout_max_ms)} of
        {{ok, _}, _, _} -> custom;
        {_, {ok, _}, _} -> custom;
        {_, _, {ok, _}} -> custom;
        _ -> default
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    erlmcp_tracing:start_span(<<"elicitation.init">>),

    ets:new(?ELICITATION_TABLE, [named_table, set, public, {keypos, #elicitation.id}]),

    {ok, #state{}}.

handle_call({create_form, ServerPid, Title, Schema, TimeoutMs}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"elicitation.create_form">>),
    try
        %% Gap #38: Validate form timeout
        case validate_form_timeout(TimeoutMs) of
            {ok, ValidatedTimeout} ->
                ElicitationId = generate_id(),
                Now = erlang:system_time(millisecond),
                ExpiresAt = Now + ValidatedTimeout,

                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"elicitation_id">> => ElicitationId,
                    <<"title">> => Title,
                    <<"type">> => <<"form">>,
                    <<"timeout_ms">> => ValidatedTimeout
                }),

                Elicitation = #elicitation{
                    id = ElicitationId,
                    type = form,
                    title = Title,
                    schema = Schema,
                    created_at = Now,
                    timeout_ms = ValidatedTimeout,
                    expires_at = ExpiresAt,
                    status = pending
                },

                ets:insert(?ELICITATION_TABLE, Elicitation),

                %% Notify server
                ServerPid ! {elicitation_created, ElicitationId, Elicitation},

                erlmcp_tracing:set_status(SpanCtx, ok),
                {reply, {ok, ElicitationId}, State#state{server_pid = ServerPid}};
            {error, Reason} ->
                erlmcp_tracing:record_error_details(SpanCtx, form_timeout_validation_failed, Reason),
                {reply, {error, Reason}, State}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {error, {Class, CaughtReason}}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call({create_url, ServerPid, Title, Url, TimeoutMs}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"elicitation.create_url">>),
    try
        %% Gap #38: Validate form timeout
        case validate_form_timeout(TimeoutMs) of
            {ok, ValidatedTimeout} ->
                ElicitationId = generate_id(),
                Now = erlang:system_time(millisecond),
                ExpiresAt = Now + ValidatedTimeout,

                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"elicitation_id">> => ElicitationId,
                    <<"title">> => Title,
                    <<"type">> => <<"url">>,
                    <<"timeout_ms">> => ValidatedTimeout
                }),

                Elicitation = #elicitation{
                    id = ElicitationId,
                    type = url,
                    title = Title,
                    url = Url,
                    created_at = Now,
                    timeout_ms = ValidatedTimeout,
                    expires_at = ExpiresAt,
                    status = pending
                },

                ets:insert(?ELICITATION_TABLE, Elicitation),

                %% Notify server
                ServerPid ! {elicitation_created, ElicitationId, Elicitation},

                erlmcp_tracing:set_status(SpanCtx, ok),
                {reply, {ok, ElicitationId}, State#state{server_pid = ServerPid}};
            {error, Reason} ->
                erlmcp_tracing:record_error_details(SpanCtx, form_timeout_validation_failed, Reason),
                {reply, {error, Reason}, State}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {error, {Class, CaughtReason}}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, {expire_elicitation, ElicitationId}}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"elicitation.expire">>),

    case ets:lookup(?ELICITATION_TABLE, ElicitationId) of
        [Elicitation] ->
            UpdatedElicitation = Elicitation#elicitation{status = cancelled},
            ets:insert(?ELICITATION_TABLE, UpdatedElicitation),
            erlmcp_tracing:set_status(SpanCtx, ok);
        [] ->
            erlmcp_tracing:record_error_details(SpanCtx, not_found, ElicitationId)
    end,

    erlmcp_tracing:end_span(SpanCtx),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_id() -> binary().
generate_id() ->
    erlang:list_to_binary(lists:flatten(io_lib:format("elicit_~s_~w",
        [erlang:timestamp(), erlang:unique_integer()]))).

-spec elicitation_to_map(#elicitation{}) -> map().
elicitation_to_map(#elicitation{
    id = Id,
    type = Type,
    title = Title,
    description = Description,
    schema = Schema,
    url = Url,
    created_at = CreatedAt,
    timeout_ms = TimeoutMs,
    expires_at = ExpiresAt,
    response = Response,
    status = Status
}) ->
    maps:filter(fun(_, V) -> V =/= undefined end, #{
        <<"id">> => Id,
        <<"type">> => atom_to_binary(Type),
        <<"title">> => Title,
        <<"description">> => Description,
        <<"schema">> => Schema,
        <<"url">> => Url,
        <<"createdAt">> => CreatedAt,
        <<"timeoutMs">> => TimeoutMs,
        <<"expiresAt">> => ExpiresAt,
        <<"response">> => Response,
        <<"status">> => atom_to_binary(Status)
    }).

-spec elicitation_handler(binary(), map(), atom()) -> ok.
elicitation_handler(ElicitationId, Response, Handler) ->
    %% Cowboy handler for elicitation form submission
    Handler ! {elicitation_submitted, ElicitationId, Response},
    ok.
