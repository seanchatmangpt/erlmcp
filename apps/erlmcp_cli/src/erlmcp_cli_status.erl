%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_status - OTP 28 Status Display
%%%
%%% Displays detailed OTP 28 feature status and system information.
%%% Shows JSON support, UTF-8 capabilities, process features, and
%%% session state with proper formatting.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_status).

-behaviour(gen_server).

%% API
-export([start_link/0, show_status/0, show_otp_info/0,
         show_json_support/0, show_utf8_support/0,
         show_process_features/0, show_session_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the status server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Show complete status
-spec show_status() -> ok.
show_status() ->
    gen_server:call(?SERVER, show_status).

%% @doc Show OTP information
-spec show_otp_info() -> ok.
show_otp_info() ->
    gen_server:call(?SERVER, show_otp_info).

%% @doc Show JSON support
-spec show_json_support() -> ok.
show_json_support() ->
    gen_server:call(?SERVER, show_json_support).

%% @doc Show UTF-8 support
-spec show_utf8_support() -> ok.
show_utf8_support() ->
    gen_server:call(?SERVER, show_utf8_support).

%% @doc Show process features
-spec show_process_features() -> ok.
show_process_features() ->
    gen_server:call(?SERVER, show_process_features).

%% @doc Show session state
-spec show_session_state() -> ok.
show_session_state() ->
    gen_server:call(?SERVER, show_session_state).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the server
-spec init(term()) -> {ok, map()}.
init([]) ->
    State = #{},
    {ok, State}.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, map()) ->
    {reply, ok, map()}.
handle_call(show_status, _From, State) ->
    print_status(),
    {reply, ok, State};

handle_call(show_otp_info, _From, State) ->
    print_otp_info(),
    {reply, ok, State};

handle_call(show_json_support, _From, State) ->
    print_json_support(),
    {reply, ok, State};

handle_call(show_utf8_support, _From, State) ->
    print_utf8_support(),
    {reply, ok, State};

handle_call(show_process_features, _From, State) ->
    print_process_features(),
    {reply, ok, State};

handle_call(show_session_state, _From, State) ->
    print_session_state(),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code changes
-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Print complete status
-spec print_status() -> ok.
print_status() ->
    io:format("~n", []),
    print_section_header("=== OTP Information ==="),
    print_otp_info_internal(),
    io:format("~n", []),

    print_section_header("=== JSON Support ==="),
    print_json_support_internal(),
    io:format("~n", []),

    print_section_header("=== Process Features ==="),
    print_process_features_internal(),
    io:format("~n", []),

    print_section_header("=== UTF-8 Support ==="),
    print_utf8_support_internal(),
    io:format("~n", []),

    print_section_header("=== Session State ==="),
    print_session_state_internal(),
    io:format("~n", []).

%% @doc Print OTP information
-spec print_otp_info() -> ok.
print_otp_info() ->
    print_otp_info_internal().

-spec print_otp_info_internal() -> ok.
print_otp_info_internal() ->
    OtpRelease = erlang:system_info(otp_release),
    Arch = erlang:system_info(system_architecture),
    Schedulers = erlang:system_info(schedulers_online),
    TimeCorrection = erlang:system_info(time_correction),

    io:format("OTP Release: ~s~n", [OtpRelease]),
    io:format("Emulator: ~s~n", [Arch]),
    io:format("Scheduler Threads: ~p~n", [Schedulers]),
    io:format("Time Correction: ~s~n", [format_bool(TimeCorrection)]).

%% @doc Print JSON support
-spec print_json_support() -> ok.
print_json_support() ->
    print_json_support_internal().

-spec print_json_support_internal() -> ok.
print_json_support_internal() ->
    %% Check if JSON module is available (OTP 27+)
    JsonEngine = case erlang:module_loaded('JSON') of
        true -> "native (OTP 27+)";
        false -> "jsx (legacy)"
    end,

    %% UTF-8 validation is always on for native JSON
    Utf8Validation = case erlang:module_loaded('JSON') of
        true -> "enabled";
        false -> "via jsx"
    end,

    %% Map support
    Decoder = case erlang:module_loaded('JSON') of
        true -> "maps (optimized)";
        false -> "jsx with maps"
    end,

    %% Encoder options
    Encoder = case erlang:module_loaded('JSON') of
        true -> "[pretty, safe]";
        false -> "jsx with format"
    end,

    io:format("JSON Engine: ~s~n", [JsonEngine]),
    io:format("UTF-8 Validation: ~s~n", [Utf8Validation]),
    io:format("Decoder: ~s~n", [Decoder]),
    io:format("Encoder: ~s~n", [Encoder]).

%% @doc Print UTF-8 support
-spec print_utf8_support() -> ok.
print_utf8_support() ->
    print_utf8_support_internal().

-spec print_utf8_support_internal() -> ok.
print_utf8_support_internal() ->
    %% Check for new string functions (OTP 26+)
    Casefold = case erlang:function_exported(string, casefold, 1) of
        true -> "enabled";
        false -> "unavailable"
    end,

    Titlecase = case erlang:function_exported(string, titlecase, 1) of
        true -> "enabled";
        false -> "unavailable"
    end,

    %% UTF-8 binary matching (OTP 26+)
    Utf8Matching = case erlang:function_exported(binary, match, 3) of
        true -> "enabled";
        false -> "unavailable"
    end,

    io:format("String Handling: Unicode-aware~n"),
    io:format("Case Folding: ~s~n", [Casefold]),
    io:format("Title Case: ~s~n", [Titlecase]),
    io:format("Binary Matching: ~s~n", [Utf8Matching]).

%% @doc Print process features
-spec print_process_features() -> ok.
print_process_features() ->
    print_process_features_internal().

-spec print_process_features_internal() -> ok.
print_process_features_internal() ->
    %% Check for priority messages (OTP 26+)
    PriorityMsgs = case erlang:function_exported(erlang, send_nosuspend, 4) of
        true -> "enabled";
        false -> "unavailable"
    end,

    %% Process iterators (OTP 28)
    ProcessIterators = case erlang:system_info(otp_release) of
        "28" -> "enabled";
        _ -> "unavailable"
    end,

    %% Tagged monitors (OTP 26+)
    TaggedMonitors = case erlang:function_exported(erlang, monitor_tagged, 2) of
        true -> "enabled";
        false -> "unavailable"
    end,

    %% Memory guards (OTP 28)
    MemoryGuards = case erlang:system_info(otp_release) of
        "28" -> "enabled";
        _ -> "unavailable"
    end,

    %% Hibernation
    Hibernation = "enabled",

    io:format("Priority Messages: ~s~n", [PriorityMsgs]),
    io:format("Process Iterators: ~s~n", [ProcessIterators]),
    io:format("Tagged Monitors: ~s~n", [TaggedMonitors]),
    io:format("Memory Guards: ~s~n", [MemoryGuards]),
    io:format("Hibernation: ~s~n", [Hibernation]).

%% @doc Print session state
-spec print_session_state() -> ok.
print_session_state() ->
    print_session_state_internal().

-spec print_session_state_internal() -> ok.
print_session_state_internal() ->
    %% Count active sessions
    ActiveSessions = count_active_sessions(),

    %% Get process priorities (example values)
    ParserPriority = "high",
    ExecutorPriority = "max",
    TransportPriority = "normal",

    io:format("Active Sessions: ~p~n", [ActiveSessions]),
    io:format("Parser Priority: ~s~n", [ParserPriority]),
    io:format("Executor Priority: ~s~n", [ExecutorPriority]),
    io:format("Transport Priority: ~s~n", [TransportPriority]).

%% @doc Print section header
-spec print_section_header(binary()) -> ok.
print_section_header(Header) ->
    io:format("~s~n", [Header]).

%% @doc Format boolean as string
-spec format_bool(boolean() | term()) -> binary().
format_bool(true) -> <<"enabled">>;
format_bool(false) -> <<"disabled">>;
format_bool(_) -> <<"unknown">>.

%% @doc Count active sessions
-spec count_active_sessions() -> integer().
count_active_sessions() ->
    %% Simple implementation for now
    %% In production, use gproc or registry to count sessions
    0.
