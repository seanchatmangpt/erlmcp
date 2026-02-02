%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test Compatibility Helper Module
%%%
%%% Provides OTP version detection and feature detection utilities for
%%% writing cross-OTP-compatible Common Test suites in erlmcp.
%%%
%%% Features:
%%% - Runtime OTP version detection
%%% - Feature detection (native coverage, process iterator, etc.)
%%% - Compatibility shims for new OTP features
%%% - Graceful degradation on older OTP versions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_ct_compat).
-behaviour(gen_server).

%% API
-export([start_link/0,
         get_otp_version/0,
         get_otp_release/0,
         has_feature/1,
         supports_native_coverage/0,
         supports_process_iterator/0,
         supports_json_module/0,
         supports_priority_messages/0,
         supports_enhanced_hooks/0,
         get_processes/0,
         encode_json/1,
         decode_json/1,
         with_native_coverage/1,
         with_timeout/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    otp_version :: integer(),
    otp_release :: string(),
    features :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the compatibility helper gen_server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get OTP version as integer (e.g., 27 for OTP 27.2)
-spec get_otp_version() -> integer().
get_otp_version() ->
    gen_server:call(?SERVER, get_otp_version).

%% @doc Get OTP release string (e.g., "27.2")
-spec get_otp_release() -> string().
get_otp_release() ->
    gen_server:call(?SERVER, get_otp_release).

%% @doc Check if a feature is available on current OTP
-spec has_feature(atom()) -> boolean().
has_feature(Feature) ->
    gen_server:call(?SERVER, {has_feature, Feature}).

%% @doc Check if native code coverage is supported (OTP 27+)
-spec supports_native_coverage() -> boolean().
supports_native_coverage() ->
    has_feature(native_coverage).

%% @doc Check if process iterator is available (OTP 28+)
-spec supports_process_iterator() -> boolean().
supports_process_iterator() ->
    has_feature(process_iterator).

%% @doc Check if json module is available (OTP 28+)
-spec supports_json_module() -> boolean().
supports_json_module() ->
    has_feature(json_module).

%% @doc Check if priority messages are supported (OTP 28+)
-spec supports_priority_messages() -> boolean().
supports_priority_messages() ->
    has_feature(priority_messages).

%% @doc Check if enhanced CT hooks are available (OTP 28+)
-spec supports_enhanced_hooks() -> boolean().
supports_enhanced_hooks() ->
    has_feature(enhanced_hooks).

%% @doc Get all processes using best available method
-spec get_processes() -> [pid()].
get_processes() ->
    case supports_process_iterator() of
        true ->
            get_processes_iterator();
        false ->
            erlang:processes()
    end.

%% @doc encode JSON using best available encoder
-spec encode_json(map()) -> binary().
encode_json(Data) ->
    case supports_json_module() of
        true ->
            json:encode(Data);
        false ->
            erlmcp_json_native:encode(convert_atom_keys(Data))
    end.

%% @doc decode JSON using best available decoder
-spec decode_json(binary()) -> map().
decode_json(JSON) ->
    case supports_json_module() of
        true ->
            json:decode(JSON);
        false ->
            erlmcp_json_native:decode(JSON)
    end.

%% @doc Execute function with native coverage if available
-spec with_native_coverage(fun(() -> A)) -> A.
with_native_coverage(Fun) ->
    case supports_native_coverage() of
        true ->
            case cover:compile_beam(directory, [{native, true}]) of
                {ok, _} ->
                    try
                        Fun()
                    after
                        cover:stop()
                    end;
                {error, _} ->
                    Fun()
            end;
        false ->
            Fun()
    end.

%% @doc Execute function with timeout, using best available mechanism
-spec with_timeout(atom(), non_neg_integer(), fun(() -> A)) ->
    {ok, A} | {error, timeout} | {error, term()}.
with_timeout(TestName, TimeoutMs, Fun) ->
    Parent = self(),
    Pid = spawn(fun() ->
        Result = try
            {ok, Fun()}
        catch
            Type:Reason:Stack ->
                {error, {Type, Reason, Stack}}
        end,
        Parent ! {TestName, Result}
    end),
    receive
        {TestName, Result} ->
            Result
    after TimeoutMs ->
        erlang:exit(Pid, kill),
        {error, timeout}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    OTPRelease = erlang:system_info(otp_release),
    OTPVersion = parse_otp_version(OTPRelease),
    Features = detect_features(OTPVersion),
    ct:pal("Initialized CT Compat Helper: OTP ~s (version ~p)",
           [OTPRelease, OTPVersion]),
    ct:pal("Available features: ~p", [maps:keys(Features)]),
    {ok, #state{
        otp_version = OTPVersion,
        otp_release = OTPRelease,
        features = Features
    }}.

handle_call(get_otp_version, _From, State) ->
    {reply, State#state.otp_version, State};

handle_call(get_otp_release, _From, State) ->
    {reply, State#state.otp_release, State};

handle_call({has_feature, Feature}, _From, State) ->
    HasFeature = maps:get(Feature, State#state.features, false),
    {reply, HasFeature, State};

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

%% @doc Parse OTP version from release string
parse_otp_version(ReleaseString) ->
    % Handle both "27" and "27.2.1" formats
    case string:split(ReleaseString, ".") of
        [Major | _] ->
            try list_to_integer(Major)
            catch error:badarg -> 0 end;
        [] ->
            try list_to_integer(ReleaseString)
            catch error:badarg -> 0 end
    end.

%% @doc Detect available features based on OTP version and runtime checks
detect_features(OTPVersion) ->
    #{
        native_coverage => detect_native_coverage(OTPVersion),
        process_iterator => detect_process_iterator(),
        json_module => detect_json_module(),
        priority_messages => detect_priority_messages(),
        enhanced_hooks => OTPVersion >= 28,
        colored_output => OTPVersion >= 27,
        sbom_support => OTPVersion >= 28
    }.

%% @doc Detect if native coverage is available
detect_native_coverage(OTPVersion) ->
    % Native coverage requires OTP 27+
    case OTPVersion >= 27 of
        false ->
            false;
        true ->
            % Try to compile with native option
            try
                {ok, _} = cover:compile_module(?MODULE, [{native, true}]),
                true
            catch
                _:_ -> false
            end
    end.

%% @doc Detect if process iterator is available (OTP 28+)
detect_process_iterator() ->
    try
        _ = erlang:processes_iterator(),
        true
    catch
        error:undef -> false
    end.

%% @doc Detect if json module is available (OTP 28+)
detect_json_module() ->
    try
        _ = json:module_info(),
        true
    catch
        error:undef -> false
    end.

%% @doc Detect if priority messages are available (OTP 28+)
detect_priority_messages() ->
    try
        _OldValue = process_flag(priority, true),
        process_flag(priority, false),
        true
    catch
        error:badarg -> false
    end.

%% @doc Get processes using iterator (OTP 28+)
get_processes_iterator() ->
    Iterator = erlang:processes_iterator(),
    collect_processes(Iterator, []).

collect_processes(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} when is_pid(Pid) ->
            collect_processes(NewIterator, [Pid | Acc]);
        done ->
            lists:reverse(Acc)
    end.

%% @doc Convert atom keys to binary for jsx compatibility
convert_atom_keys(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        BinKey = atom_to_binary(K, utf8),
        maps:put(BinKey, convert_atom_keys(V), Acc)
    end, #{}, Map);
convert_atom_keys(List) when is_list(List) ->
    [convert_atom_keys(Item) || Item <- List];
convert_atom_keys(Term) ->
    Term.
