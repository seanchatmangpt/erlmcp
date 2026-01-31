%%%-------------------------------------------------------------------
%%% @doc erlmcp_llm_provider_anthropic - Anthropic Claude API Provider
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_llm_provider_anthropic).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, create_message/2,
         set_api_key/1, get_api_key/0, set_model/1, get_model/0, set_version/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

-record(state, {
    api_key :: binary() | undefined,
    model :: binary(),
    version :: binary(),
    timeout :: pos_integer()
}).

start_link() -> start_link(#{}).
start_link(Config) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

create_message(Messages, Params) ->
    gen_server:call(?MODULE, {create_message, Messages, Params}, 60000).

set_api_key(ApiKey) when is_binary(ApiKey) ->
    gen_server:call(?MODULE, {set_api_key, ApiKey}).

get_api_key() -> gen_server:call(?MODULE, get_api_key).

set_model(Model) when is_binary(Model) ->
    gen_server:call(?MODULE, {set_model, Model}).

get_model() -> gen_server:call(?MODULE, get_model).

set_version(Version) when is_binary(Version) ->
    gen_server:call(?MODULE, {set_version, Version}).

init([Config]) ->
    ApiKey = maps_get(api_key, Config, get_env_api_key()),
    Model = maps_get(model, Config, <<"claude-3-sonnet-20240229">>),
    Version = maps_get(version, Config, <<"2023-06-01">>),
    Timeout = maps_get(timeout, Config, 60000),

    ?LOG_INFO("Anthropic provider initialized with model: ~s", [Model]),
    {ok, #state{api_key = ApiKey, model = Model, version = Version, timeout = Timeout}}.

handle_call({create_message, Messages, Params}, _From, State) ->
    Result = do_create_message(Messages, Params, State),
    {reply, Result, State};

handle_call({set_api_key, ApiKey}, _From, State) ->
    {reply, ok, State#state{api_key = ApiKey}};

handle_call(get_api_key, _From, State) ->
    {reply, State#state.api_key, State};

handle_call({set_model, Model}, _From, State) ->
    {reply, ok, State#state{model = Model}};

handle_call(get_model, _From, State) ->
    {reply, State#state.model, State};

handle_call({set_version, Version}, _From, State) ->
    {reply, ok, State#state{version = Version}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    % Handle gun connection process death during API requests
    % These are temporary monitors created during HTTP requests to Anthropic API
    % The actual request will fail with timeout/error, so we just log this
    logger:warning("Gun connection process ~p died during Anthropic API request: ~p (monitor: ~p)",
                   [Pid, Reason, MonitorRef]),
    {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_create_message(Messages, Params, #state{api_key = undefined}) ->
    {error, missing_api_key};
do_create_message(Messages, Params, State) ->
    Model = maps_get(<<"model">>, Params, State#state.model),
    MaxTokens = maps_get(<<"maxTokens">>, Params, 1000),

    {SystemMessages, UserMessages} = split_messages(Messages),

    RequestBody = #{
        <<"model">> => Model,
        <<"messages">> => UserMessages,
        <<"max_tokens">> => MaxTokens
    },

    RequestBody1 = case SystemMessages of
        [] -> RequestBody;
        [#{<<"content">> := SystemContent}] ->
            RequestBody#{<<"system">> => SystemContent}
    end,

    Url = <<"https://api.anthropic.com/v1/messages">>,

    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"x-api-key">>, State#state.api_key},
        {<<"anthropic-version">>, State#state.version}
    ],

    case http_post(Url, Headers, RequestBody1, State#state.timeout) of
        {ok, ResponseBody} ->
            parse_anthropic_response(ResponseBody);
        {error, Reason} ->
            ?LOG_ERROR("Anthropic API request failed: ~p", [Reason]),
            {error, {http_error, Reason}}
    end.

split_messages(Messages) ->
    lists:foldl(fun
        (#{<<"role">> := <<"system">>, <<"content">> := Content}, {SysAcc, UserAcc}) ->
            {[#{<<"content">> => Content} | SysAcc], UserAcc};
        (#{<<"role">> := Role, <<"content">> := Content}, {SysAcc, UserAcc}) ->
            {SysAcc, [#{<<"role">> => Role, <<"content">> => Content} | UserAcc]}
    end, {[], []}, Messages).

parse_anthropic_response(ResponseBody) ->
    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"content">> := [#{<<"type">> := <<"text">>, <<"text">> := Text} | _],
          <<"model">> := Model, <<"usage">> := Usage} ->
            {ok, #{
                <<"role">> => <<"assistant">>,
                <<"content">> => Text,
                <<"model">> => Model,
                <<"stopReason">> => <<"end_of_turn">>,
                <<"usage">> => #{
                    <<"promptTokens">> => maps_get(<<"input_tokens">>, Usage, 0),
                    <<"completionTokens">> => maps_get(<<"output_tokens">>, Usage, 0),
                    <<"totalTokens">> => (maps_get(<<"input_tokens">>, Usage, 0) +
                                          maps_get(<<"output_tokens">>, Usage, 0))
                }
            }};
        #{<<"error">> := Error} ->
            Message = maps_get(<<"message">>, Error, <<"Unknown error">>),
            Type = maps_get(<<"type">>, Error, <<"api_error">>),
            {error, {anthropic_error, Type, Message}};
        Other ->
            ?LOG_ERROR("Unexpected Anthropic response format: ~p", [Other]),
            {error, invalid_response_format}
    catch
        _:_:_ -> {error, json_decode_failed}
    end.

http_post(Url, Headers, BodyMap, Timeout) ->
    #{scheme := Scheme, host := Host, port := Port} = uri_string:parse(Url),
    Path = maps_get(path, uri_string:parse(Url), <<"/">>),
    Body = jsx:encode(BodyMap),

    Transport = case Scheme of
        <<"https">> -> tls;
        <<"http">> -> tcp
    end,

    case gun:open(Host, Port, #{transport => Transport, protocols => [http]}) of
        {ok, ConnPid} ->
            MonRef = monitor(process, ConnPid),

            case gun:await_up(ConnPid, Timeout) of
                {up, _Protocol} ->
                    StreamRef = gun:post(ConnPid, Path, Headers, Body),

                    case gun:await(ConnPid, StreamRef, Timeout) of
                        {response, nofin, 200, _RespHeaders} ->
                            case gun:await_body(ConnPid, StreamRef, Timeout) of
                                {ok, ResponseBody} ->
                                    demonitor(MonRef, [flush]),
                                    gun:close(ConnPid),
                                    {ok, ResponseBody};
                                {error, Reason} ->
                                    demonitor(MonRef, [flush]),
                                    gun:close(ConnPid),
                                    {error, {body_read_failed, Reason}}
                            end;
                        {response, fin, Status, RespHeaders} ->
                            demonitor(MonRef, [flush]),
                            gun:close(ConnPid),
                            {error, {http_error, Status, RespHeaders}};
                        {error, Reason} ->
                            demonitor(MonRef, [flush]),
                            gun:close(ConnPid),
                            {error, {request_failed, Reason}}
                    end;
                {error, Reason} ->
                    demonitor(MonRef, [flush]),
                    gun:close(ConnPid),
                    {error, {connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {gun_open_failed, Reason}}
    end.

get_env_api_key() ->
    case os:getenv("ANTHROPIC_API_KEY") of
        false -> undefined;
        [] -> undefined;
        Key -> list_to_binary(Key)
    end.

maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
