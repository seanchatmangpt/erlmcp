-module(erlmcp_protocol_versioning).
-behaviour(gen_server).
-compile({nowarn_unused_function, [transform_from_2_1_0/1, transform_from_3_0_0/1]}).

%% API
-export([start_link/0,
         negotiate_version/2,
         transform_message/3,
         supported_versions/0,
         current_version/0,
         is_version_supported/1,
         register_version_handler/2,
         transform_request/3,
         transform_response/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_VERSION, <<"2.1.0">>).
-define(LATEST_VERSION, <<"3.0.0">>).

%% Version compatibility matrix
-define(COMPATIBILITY_MATRIX, #{
    <<"2.1.0">> => #{downgrade => <<"2.0.0">>, upgrade => <<"3.0.0">>},
    <<"3.0.0">> => #{downgrade => <<"2.1.0">>, upgrade => <<"4.0.0">>}
}).

-record(state, {
    current_version :: binary(),
    version_handlers = #{} :: #{binary() => {module(), atom()}},
    negotiation_stats = #{} :: map()
}).

-type version() :: binary().
-type message() :: map().
-type transform_result() :: {ok, message()} | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec negotiate_version(binary(), [binary()]) -> {ok, binary()} | {error, term()}.
negotiate_version(ClientVersion, SupportedVersions) when is_list(SupportedVersions) ->
    gen_server:call(?SERVER, {negotiate_version, ClientVersion, SupportedVersions}).

-spec transform_message(message(), version(), version()) -> transform_result().
transform_message(Message, FromVersion, ToVersion) ->
    gen_server:call(?SERVER, {transform_message, Message, FromVersion, ToVersion}).

-spec supported_versions() -> [version()].
supported_versions() ->
    maps:keys(?COMPATIBILITY_MATRIX).

-spec current_version() -> version().
current_version() ->
    ?LATEST_VERSION.

-spec is_version_supported(version()) -> boolean().
is_version_supported(Version) ->
    lists:member(Version, supported_versions()).

-spec register_version_handler(version(), {module(), atom()}) -> ok | {error, term()}.
register_version_handler(Version, {Module, Function}) when is_atom(Module), is_atom(Function) ->
    gen_server:call(?SERVER, {register_handler, Version, Module, Function}).

-spec transform_request(message(), version(), version()) -> transform_result().
transform_request(Request, FromVersion, ToVersion) ->
    do_transform_message(Request, FromVersion, ToVersion, request).

-spec transform_response(message(), version(), version()) -> transform_result().
transform_response(Response, FromVersion, ToVersion) ->
    do_transform_message(Response, FromVersion, ToVersion, response).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    State = #state{
        current_version = ?LATEST_VERSION,
        version_handlers = initialize_default_handlers()
    },
    logger:info("Protocol versioning started, current version: ~s", [?LATEST_VERSION]),
    {ok, State}.

handle_call({negotiate_version, ClientVersion, SupportedVersions}, _From, State) ->
    case negotiate_version_impl(ClientVersion, SupportedVersions) of
        {ok, SelectedVersion} = Result ->
            NewStats = maps:update_with(SelectedVersion,
                                      fun(Count) -> Count + 1 end,
                                      1,
                                      State#state.negotiation_stats),
            {reply, Result, State#state{negotiation_stats = NewStats}};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

handle_call({transform_message, Message, FromVersion, ToVersion}, _From, State) ->
    Result = do_transform_message(Message, FromVersion, ToVersion, message),
    {reply, Result, State};

handle_call({register_handler, Version, Module, Function}, _From, State) ->
    case is_version_supported(Version) of
        true ->
            NewHandlers = maps:put(Version, {Module, Function}, State#state.version_handlers),
            {reply, ok, State#state{version_handlers = NewHandlers}};
        false ->
            {reply, {error, unsupported_version}, State}
    end;

handle_call(get_version, _From, State) ->
    {reply, State#state.current_version, State};

handle_call(get_stats, _From, State) ->
    {reply, State#state.negotiation_stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Initialize default version transformation handlers
initialize_default_handlers() ->
    #{
        <<"2.1.0">> => {?MODULE, transform_from_2_1_0},
        <<"3.0.0">> => {?MODULE, transform_from_3_0_0}
    }.

%% @private Version negotiation implementation
negotiate_version_impl(ClientVersion, SupportedVersions) ->
    case lists:member(?LATEST_VERSION, SupportedVersions) of
        true ->
            {ok, ?LATEST_VERSION};
        false ->
            case find_common_version([ClientVersion | SupportedVersions]) of
                {ok, Version} -> {ok, Version};
                {error, no_common_version} = Error -> Error
            end
    end.

%% @private Find a common supported version
find_common_version(Versions) ->
    Supported = supported_versions(),
    case [V || V <- Versions, lists:member(V, Supported)] of
        [] -> {error, no_common_version};
        [Latest | _] -> {ok, Latest} % Use latest common version
    end.

%% @private Transform message between versions
do_transform_message(Message, FromVersion, ToVersion, MessageType) ->
    case FromVersion =:= ToVersion of
        true ->
            {ok, Message};
        false ->
            case is_transform_supported(FromVersion, ToVersion) of
                true ->
                    transform_message_impl(Message, FromVersion, ToVersion, MessageType);
                false ->
                    {error, unsupported_transform}
            end
    end.

%% @private Check if transformation is supported
is_transform_supported(FromVersion, ToVersion) ->
    case ?COMPATIBILITY_MATRIX of
        #{FromVersion := #{upgrade := ToVersion}} -> true;
        #{ToVersion := #{downgrade := FromVersion}} -> true;
        _ -> false
    end.

%% @private Implementation of message transformation
transform_message_impl(Message, FromVersion, ToVersion, MessageType) ->
    try
        case MessageType of
            request -> transform_request_fields(Message, FromVersion, ToVersion);
            response -> transform_response_fields(Message, FromVersion, ToVersion);
            message -> transform_all_fields(Message, FromVersion, ToVersion)
        end
    catch
        _:Error ->
            logger:error("Failed to transform message from ~s to ~s: ~p",
                        [FromVersion, ToVersion, Error]),
            {error, transformation_failed}
    end.

%% @private Transform request fields
transform_request_fields(Request, <<"2.1.0">>, <<"3.0.0">>) ->
    %% Upgrade from 2.1.0 to 3.0.0
    %% Add protocol version field
    Request#{<<"protocolVersion">> => <<"3.0.0">>};
transform_request_fields(Request, <<"3.0.0">>, <<"2.1.0">>) ->
    %% Downgrade from 3.0.0 to 2.1.0
    %% Remove protocol version field
    maps:remove(<<"protocolVersion">>, Request);
transform_request_fields(Request, _From, _To) ->
    {ok, Request}.

%% @private Transform response fields
transform_response_fields(Response, <<"2.1.0">>, <<"3.0.0">>) ->
    %% Add protocol version to response
    Response#{<<"protocolVersion">> => <<"3.0.0">>};
transform_response_fields(Response, <<"3.0.0">>, <<"2.1.0">>) ->
    %% Remove protocol version
    maps:remove(<<"protocolVersion">>, Response);
transform_response_fields(Response, _From, _To) ->
    {ok, Response}.

%% @private Transform all message fields
transform_all_fields(Message, <<"2.1.0">>, <<"3.0.0">>) ->
    %% Add version tracking
    Message#{<<"_v">> => <<"3.0.0">>};
transform_all_fields(Message, <<"3.0.0">>, <<"2.1.0">>) ->
    %% Remove version tracking
    maps:remove(<<"_v">>, Message);
transform_all_fields(Message, _From, _To) ->
    {ok, Message}.

%% @private Transformation handler for 2.1.0 messages
transform_from_2_1_0(Message) ->
    {ok, Message#{<<"protocolVersion">> => <<"2.1.0">>}}.

%% @private Transformation handler for 3.0.0 messages
transform_from_3_0_0(Message) ->
    {ok, Message#{<<"protocolVersion">> => <<"3.0.0">>}}.
