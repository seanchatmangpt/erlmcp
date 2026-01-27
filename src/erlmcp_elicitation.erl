-module(erlmcp_elicitation).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behavior(gen_server).

%% API exports
-export([
    start_link/0,
    create_form/3,
    create_url_elicitation/3,
    get_elicitation/1,
    submit_response/2,
    cancel_elicitation/1
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
-define(FORM_TIMEOUT, 600000). %% 10 minutes

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
    gen_server:call(?MODULE, {create_form, ServerPid, Title, Schema}, 5000).

-spec create_url_elicitation(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}.
create_url_elicitation(ServerPid, Title, Url) when is_pid(ServerPid), is_binary(Title), is_binary(Url) ->
    gen_server:call(?MODULE, {create_url, ServerPid, Title, Url}, 5000).

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

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    erlmcp_tracing:start_span(<<"elicitation.init">>),

    ets:new(?ELICITATION_TABLE, [named_table, set, public, {keypos, #elicitation.id}]),

    {ok, #state{}}.

handle_call({create_form, ServerPid, Title, Schema}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"elicitation.create_form">>),
    try
        ElicitationId = generate_id(),
        Now = erlang:system_time(millisecond),
        ExpiresAt = Now + ?FORM_TIMEOUT,

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"elicitation_id">> => ElicitationId,
            <<"title">> => Title,
            <<"type">> => <<"form">>
        }),

        Elicitation = #elicitation{
            id = ElicitationId,
            type = form,
            title = Title,
            schema = Schema,
            created_at = Now,
            expires_at = ExpiresAt,
            status = pending
        },

        ets:insert(?ELICITATION_TABLE, Elicitation),

        %% Notify server
        ServerPid ! {elicitation_created, ElicitationId, Elicitation},

        erlmcp_tracing:set_status(SpanCtx, ok),
        {reply, {ok, ElicitationId}, State#state{server_pid = ServerPid}}
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {error, {Class, CaughtReason}}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call({create_url, ServerPid, Title, Url}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"elicitation.create_url">>),
    try
        ElicitationId = generate_id(),
        Now = erlang:system_time(millisecond),
        ExpiresAt = Now + ?FORM_TIMEOUT,

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"elicitation_id">> => ElicitationId,
            <<"title">> => Title,
            <<"type">> => <<"url">>
        }),

        Elicitation = #elicitation{
            id = ElicitationId,
            type = url,
            title = Title,
            url = Url,
            created_at = Now,
            expires_at = ExpiresAt,
            status = pending
        },

        ets:insert(?ELICITATION_TABLE, Elicitation),

        %% Notify server
        ServerPid ! {elicitation_created, ElicitationId, Elicitation},

        erlmcp_tracing:set_status(SpanCtx, ok),
        {reply, {ok, ElicitationId}, State#state{server_pid = ServerPid}}
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
        <<"expiresAt">> => ExpiresAt,
        <<"response">> => Response,
        <<"status">> => atom_to_binary(Status)
    }).

-spec elicitation_handler(binary(), map(), atom()) -> ok.
elicitation_handler(ElicitationId, Response, Handler) ->
    %% Cowboy handler for elicitation form submission
    Handler ! {elicitation_submitted, ElicitationId, Response},
    ok.
