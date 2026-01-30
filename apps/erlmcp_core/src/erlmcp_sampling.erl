%%%-------------------------------------------------------------------
%%% @doc erlmcp_sampling - Sampling/createMessage capability implementation
%%% Implements MCP sampling for LLM message generation
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sampling).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create_message/2,
    create_message/3,
    set_model_provider/2,
    get_model_provider/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("erlmcp.hrl").

%% State record
-record(state, {
    model_provider :: module(),           % LLM provider callback module
    default_params :: map(),              % Default model parameters
    request_count = 0 :: non_neg_integer(), % Request counter
    last_request :: integer()            % Timestamp of last request
}).

-type sampling_messages() :: [map()].
-type sampling_params() :: map().
-type sampling_result() :: {ok, map()} | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the sampling server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a message using the configured LLM provider
-spec create_message(sampling_messages(), sampling_params()) -> sampling_result().
create_message(Messages, Params) when is_list(Messages), is_map(Params) ->
    gen_server:call(?MODULE, {create_message, Messages, Params}, 30000).

%% @doc Create a message with custom timeout
-spec create_message(sampling_messages(), sampling_params(), timeout()) -> sampling_result().
create_message(Messages, Params, Timeout) when is_list(Messages), is_map(Params) ->
    gen_server:call(?MODULE, {create_message, Messages, Params}, Timeout).

%% @doc Set the model provider module
-spec set_model_provider(pid() | module(), module()) -> ok.
set_model_provider(ServerPid, ProviderModule) when is_pid(ServerPid), is_atom(ProviderModule) ->
    gen_server:call(ServerPid, {set_model_provider, ProviderModule});
set_model_provider(ProviderModule, ProviderModule) when is_atom(ProviderModule) ->
    gen_server:call(?MODULE, {set_model_provider, ProviderModule}).

%% @doc Get the current model provider
-spec get_model_provider() -> module().
get_model_provider() ->
    gen_server:call(?MODULE, get_model_provider).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Starting erlmcp_sampling server"),
    {ok, #state{
        model_provider = application:get_env(erlmcp, model_provider, erlmcp_mock_llm),
        default_params = get_default_params()
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.

handle_call({create_message, Messages, Params}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"sampling.create_message">>),
    try
        %% Update metrics
        RequestCount = State#state.request_count + 1,
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"sampling.request_count">> => RequestCount,
            <<"sampling.messages_count">> => length(Messages)
        }),

        %% Validate input
        case validate_messages(Messages) of
            ok ->
                case validate_params(Params) of
                    ok ->
                        %% Merge defaults with provided params
                        MergedParams = merge_params(Params, State#state.default_params),

                        %% Call the model provider
                        ProviderModule = State#state.model_provider,
                        Result = case code:ensure_loaded(ProviderModule) of
                            {module, ProviderModule} ->
                                case erlang:function_exported(ProviderModule, create_message, 2) of
                                    true ->
                                        ProviderModule:create_message(Messages, MergedParams);
                                    false ->
                                        logger:error("Model provider ~p does not export create_message/2",
                                                    [ProviderModule]),
                                        {error, invalid_provider}
                                end;
                            {error, LoadReason} ->
                                logger:error("Failed to load model provider ~p: ~p",
                                            [ProviderModule, LoadReason]),
                                {error, invalid_provider}
                        end,

                        case Result of
                            {ok, Response} ->
                                erlmcp_tracing:set_status(SpanCtx, ok),
                                NewState = State#state{
                                    request_count = RequestCount,
                                    last_request = erlang:system_time(millisecond)
                                },
                                {reply, {ok, Response}, NewState};
                            {error, Reason} ->
                                erlmcp_tracing:record_error_details(SpanCtx, provider_error, Reason),
                                {reply, {error, Reason}, State}
                        end;
                    {error, ValidationError} ->
                        erlmcp_tracing:record_error_details(SpanCtx, params_validation_failed, ValidationError),
                        {reply, {error, ValidationError}, State}
                end;
            {error, ValidationError} ->
                erlmcp_tracing:record_error_details(SpanCtx, messages_validation_failed, ValidationError),
                {reply, {error, ValidationError}, State}
        end
    catch
        Class:ExceptionReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, ExceptionReason, Stacktrace),
            logger:error("Sampling create_message crashed: ~p:~p~n~p", [Class, ExceptionReason, Stacktrace]),
            {reply, {error, internal_error}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call({set_model_provider, ProviderModule}, _From, State) when is_atom(ProviderModule) ->
    logger:info("Setting model provider to ~p", [ProviderModule]),
    {reply, ok, State#state{model_provider = ProviderModule}};

handle_call(get_model_provider, _From, State) ->
    {reply, State#state.model_provider, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Stopping erlmcp_sampling server"),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Validate message list format
-spec validate_messages(sampling_messages()) -> ok | {error, term()}.
validate_messages([]) ->
    {error, empty_messages};
validate_messages(Messages) when is_list(Messages) ->
    case lists:all(fun validate_message/1, Messages) of
        true -> ok;
        false -> {error, invalid_message_format}
    end;
validate_messages(_) ->
    {error, messages_must_be_list}.

%% @doc Validate single message format
-spec validate_message(map()) -> boolean().
validate_message(Message) when is_map(Message) ->
    case maps:get(<<"role">>, Message, undefined) of
        undefined -> false;
        Role when is_binary(Role) ->
            case maps:get(<<"content">>, Message, undefined) of
                undefined -> false;
                Content when is_binary(Content); is_map(Content) -> true;
                _ -> false
            end;
        _ -> false
    end;
validate_message(_) ->
    false.

%% @doc Validate sampling parameters
-spec validate_params(sampling_params()) -> ok | {error, term()}.
validate_params(Params) when is_map(Params) ->
    %% Validate temperature if provided
    case maps:get(<<"temperature">>, Params, undefined) of
        undefined ->
            ok;
        Temp when is_number(Temp), Temp >= 0.0, Temp =< 2.0 ->
            ok;
        _ ->
            {error, invalid_temperature}
    end;
validate_params(_) ->
    {error, params_must_be_map}.

%% @doc Merge user params with defaults
-spec merge_params(sampling_params(), map()) -> map().
merge_params(UserParams, DefaultParams) ->
    maps:merge(DefaultParams, UserParams).

%% @doc Get default parameters from config
-spec get_default_params() -> map().
get_default_params() ->
    #{
        <<"model">> => application:get_env(erlmcp, default_model, <<"gpt-3.5-turbo">>),
        <<"temperature">> => application:get_env(erlmcp, default_temperature, 0.7),
        <<"maxTokens">> => application:get_env(erlmcp, default_max_tokens, 1000)
    }.
