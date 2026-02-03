%%%-------------------------------------------------------------------
%%% @doc erlmcp_llm_provider_local - Local LLM Provider
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_llm_provider_local).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, create_message/2, get_backend/0, set_base_url/1,
         get_base_url/0, set_model/1, get_model/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

-include_lib("kernel/include/logger.hrl").

-record(state,
        {backend :: ollama | lm_studio | openai_compatible,
         base_url :: binary(),
         model :: binary(),
         timeout :: pos_integer()}).

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

create_message(Messages, Params) ->
    gen_server:call(?MODULE, {create_message, Messages, Params}, 120000).

set_backend(Backend)
    when Backend =:= ollama; Backend =:= lm_studio; Backend =:= openai_compatible ->
    gen_server:call(?MODULE, {set_backend, Backend}).

get_backend() ->
    gen_server:call(?MODULE, get_backend).

set_base_url(Url) when is_binary(Url) ->
    gen_server:call(?MODULE, {set_base_url, Url}).

get_base_url() ->
    gen_server:call(?MODULE, get_base_url).

set_model(Model) when is_binary(Model) ->
    gen_server:call(?MODULE, {set_model, Model}).

get_model() ->
    gen_server:call(?MODULE, get_model).

init([Config]) ->
    Backend = maps_get(backend, Config, ollama),
    BaseUrl =
        case Backend of
            ollama ->
                maps_get(base_url, Config, <<"http://localhost:11434">>);
            lm_studio ->
                maps_get(base_url, Config, <<"http://localhost:1234">>);
            openai_compatible ->
                maps_get(base_url, Config, <<"http://localhost:8000">>)
        end,
    Model = maps_get(model, Config, <<"llama2">>),
    Timeout = maps_get(timeout, Config, 120000),

    ?LOG_INFO("Local LLM provider initialized: backend=~p, url=~s, model=~s",
              [Backend, BaseUrl, Model]),

    {ok,
     #state{backend = Backend,
            base_url = BaseUrl,
            model = Model,
            timeout = Timeout}}.

handle_call({create_message, Messages, Params}, _From, State) ->
    Result = do_create_message(Messages, Params, State),
    {reply, Result, State};
handle_call({set_backend, Backend}, _From, State) ->
    {reply, ok, State#state{backend = Backend}};
handle_call(get_backend, _From, State) ->
    {reply, State#state.backend, State};
handle_call({set_base_url, Url}, _From, State) ->
    {reply, ok, State#state{base_url = Url}};
handle_call(get_base_url, _From, State) ->
    {reply, State#state.base_url, State};
handle_call({set_model, Model}, _From, State) ->
    {reply, ok, State#state{model = Model}};
handle_call(get_model, _From, State) ->
    {reply, State#state.model, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    % Handle gun connection process death during API requests
    % These are temporary monitors created during HTTP requests to local LLM API
    % The actual request will fail with timeout/error, so we just log this
    logger:warning("Gun connection process ~p died during local LLM API request: ~p (monitor: ~p)",
                   [Pid, Reason, MonitorRef]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_create_message(Messages, Params, #state{backend = Backend} = State) ->
    case Backend of
        ollama ->
            do_ollama_request(Messages, Params, State);
        lm_studio ->
            do_openai_compatible_request(Messages, Params, State);
        openai_compatible ->
            do_openai_compatible_request(Messages, Params, State)
    end.

do_ollama_request(Messages, Params, State) ->
    Model = maps_get(<<"model">>, Params, State#state.model),
    Temperature = maps_get(<<"temperature">>, Params, 0.7),

    RequestBody =
        #{<<"model">> => Model,
          <<"messages">> => Messages,
          <<"stream">> => false,
          <<"options">> => #{<<"temperature">> => Temperature}},

    Url = <<(State#state.base_url)/binary, "/v1/chat/completions">>,

    Headers = [{<<"Content-Type">>, <<"application/json">>}],

    case http_post(Url, Headers, RequestBody, State#state.timeout) of
        {ok, ResponseBody} ->
            parse_ollama_response(ResponseBody);
        {error, Reason} ->
            ?LOG_ERROR("Ollama API request failed: ~p", [Reason]),
            {error, {http_error, Reason}}
    end.

do_openai_compatible_request(Messages, Params, State) ->
    Model = maps_get(<<"model">>, Params, State#state.model),
    Temperature = maps_get(<<"temperature">>, Params, 0.7),
    MaxTokens = maps_get(<<"maxTokens">>, Params, 1000),

    RequestBody =
        #{<<"model">> => Model,
          <<"messages">> => Messages,
          <<"temperature">> => Temperature,
          <<"max_tokens">> => MaxTokens},

    Url = <<(State#state.base_url)/binary, "/v1/chat/completions">>,

    Headers = [{<<"Content-Type">>, <<"application/json">>}],

    case http_post(Url, Headers, RequestBody, State#state.timeout) of
        {ok, ResponseBody} ->
            parse_openai_compatible_response(ResponseBody);
        {error, Reason} ->
            ?LOG_ERROR("OpenAI-compatible API request failed: ~p", [Reason]),
            {error, {http_error, Reason}}
    end.

parse_ollama_response(ResponseBody) ->
    try json:decode(ResponseBody) of
        #{<<"message">> := Message} ->
            Content = maps_get(<<"content">>, Message, <<>>),
            {ok,
             #{<<"role">> => <<"assistant">>,
               <<"content">> => Content,
               <<"model">> => <<"ollama">>,
               <<"stopReason">> => <<"end_of_turn">>,
               <<"usage">> =>
                   #{<<"promptTokens">> => 0,
                     <<"completionTokens">> => 0,
                     <<"totalTokens">> => 0},
               <<"_metadata">> => #{<<"provider">> => <<"ollama">>}}};
        #{<<"error">> := Error} ->
            {error, {ollama_error, Error}};
        Other ->
            ?LOG_ERROR("Unexpected Ollama response format: ~p", [Other]),
            {error, invalid_response_format}
    catch
        _:_ ->
            {error, json_decode_failed}
    end.

parse_openai_compatible_response(ResponseBody) ->
    try json:decode(ResponseBody) of
        #{<<"choices">> := [#{<<"message">> := Message} | _], <<"usage">> := Usage} ->
            {ok,
             #{<<"role">> => maps_get(<<"role">>, Message, <<"assistant">>),
               <<"content">> => maps_get(<<"content">>, Message, <<>>),
               <<"model">> => maps_get(<<"model">>, Usage, <<>>),
               <<"stopReason">> => <<"end_of_turn">>,
               <<"usage">> =>
                   #{<<"promptTokens">> => maps_get(<<"prompt_tokens">>, Usage, 0),
                     <<"completionTokens">> => maps_get(<<"completion_tokens">>, Usage, 0),
                     <<"totalTokens">> => maps_get(<<"total_tokens">>, Usage, 0)},
               <<"_metadata">> => #{<<"provider">> => <<"local">>}}};
        #{<<"error">> := Error} ->
            {error, {local_llm_error, Error}};
        Other ->
            ?LOG_ERROR("Unexpected OpenAI-compatible response format: ~p", [Other]),
            {error, invalid_response_format}
    catch
        _:_ ->
            {error, json_decode_failed}
    end.

http_post(Url, Headers, BodyMap, Timeout) ->
    #{scheme := Scheme,
      host := Host,
      port := Port} =
        uri_string:parse(Url),
    Path = maps_get(path, uri_string:parse(Url), <<"/">>),
    Body = json:encode(BodyMap),

    Transport =
        case Scheme of
            <<"https">> ->
                tls;
            <<"http">> ->
                tcp
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

maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.
