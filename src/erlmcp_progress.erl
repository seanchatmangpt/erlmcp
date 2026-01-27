%%%===================================================================
%%% Module: erlmcp_progress
%%% Purpose: Tool progress notification and tracking system for MCP
%%%
%%% Provides:
%%% - Progress token generation and lifecycle management
%%% - In-flight tool call tracking with ETS
%%% - Progress notification delivery to clients
%%% - Timeout detection and cleanup
%%%
%%% Design:
%%% - ETS table stores active progress tokens with metadata
%%% - Each token tracks tool name, start time, and client PID
%%% - Progress can be sent as percentage or absolute values
%%% - Automatic cleanup on completion
%%%===================================================================

-module(erlmcp_progress).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    generate_token/0,
    track_tool_call/3,
    send_progress/4,
    get_progress/1,
    list_active_tokens/0,
    cleanup_completed/1,
    check_timeout/1,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Type definitions
-type progress_token() :: binary().
-type progress_data() :: #{
    percentage => float(),
    absolute => {non_neg_integer(), non_neg_integer()},
    message => binary(),
    context => map()
}.
-type token_metadata() :: #{
    tool_name := binary(),
    start_time := integer(),
    client_pid := pid(),
    last_update := integer(),
    progress_data := progress_data() | undefined
}.

-export_type([progress_token/0, progress_data/0, token_metadata/0]).

%% Constants
-define(ETS_TABLE, erlmcp_progress_tokens).
-define(PROGRESS_TIMEOUT_MS, 30000).  % 30 second timeout
-define(CLEANUP_INTERVAL_MS, 5000).   % Check every 5 seconds

%% State record
-record(state, {
    ets_table :: ets:table(),
    cleanup_timer :: reference() | undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Generate a unique progress token
-spec generate_token() -> progress_token().
generate_token() ->
    Timestamp = erlang:system_time(millisecond),
    RandomPart = erlang:unique_integer([positive]),
    Token = <<
        (integer_to_binary(Timestamp))/binary, $-,
        (integer_to_binary(RandomPart))/binary
    >>,
    Token.

%% Track a tool call with its progress token
-spec track_tool_call(progress_token(), binary(), pid()) -> ok | {error, term()}.
track_tool_call(Token, ToolName, ClientPid) when is_binary(Token), is_binary(ToolName), is_pid(ClientPid) ->
    gen_server:call(?MODULE, {track_tool_call, Token, ToolName, ClientPid}).

%% Send progress update for a tool call
-spec send_progress(progress_token(), progress_data(), pid(), binary()) -> ok | {error, term()}.
send_progress(Token, ProgressData, TransportPid, TransportId)
  when is_binary(Token), is_map(ProgressData), is_pid(TransportPid) ->
    gen_server:call(?MODULE, {send_progress, Token, ProgressData, TransportPid, TransportId}).

%% Get current progress for a token
-spec get_progress(progress_token()) -> {ok, token_metadata()} | {error, not_found}.
get_progress(Token) when is_binary(Token) ->
    gen_server:call(?MODULE, {get_progress, Token}).

%% List all active progress tokens
-spec list_active_tokens() -> [progress_token()].
list_active_tokens() ->
    gen_server:call(?MODULE, list_active_tokens).

%% Mark token as completed and clean up
-spec cleanup_completed(progress_token()) -> ok.
cleanup_completed(Token) when is_binary(Token) ->
    gen_server:cast(?MODULE, {cleanup_completed, Token}).

%% Check for timeout on specific token
-spec check_timeout(progress_token()) -> ok | {error, timeout}.
check_timeout(Token) when is_binary(Token) ->
    gen_server:call(?MODULE, {check_timeout, Token}).

%% Stop the progress manager
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    Table = ets:new(?ETS_TABLE, [
        named_table,
        {keypos, 1},
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    % Start cleanup timer
    Timer = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_timeout_tokens),

    logger:info("Progress notification manager started, cleanup interval: ~Bms", [?CLEANUP_INTERVAL_MS]),
    {ok, #state{ets_table = Table, cleanup_timer = Timer}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call({track_tool_call, Token, ToolName, ClientPid}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Metadata = #{
        tool_name => ToolName,
        start_time => Now,
        client_pid => ClientPid,
        last_update => Now,
        progress_data => undefined
    },

    % Monitor client process
    erlang:monitor(process, ClientPid),

    try
        ets:insert(?ETS_TABLE, {Token, Metadata}),
        logger:debug("Tracking tool call: token=~s, tool=~s", [Token, ToolName]),
        {reply, ok, State}
    catch
        Error:Reason ->
            logger:error("Failed to track tool call: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({send_progress, Token, ProgressData, TransportPid, _TransportId}, _From, State) ->
    Now = erlang:system_time(millisecond),

    case ets:lookup(?ETS_TABLE, Token) of
        [] ->
            logger:warning("Progress token not found: ~s", [Token]),
            {reply, {error, token_not_found}, State};
        [{Token, Metadata}] ->
            case maps:get(client_pid, Metadata) of
                undefined ->
                    {reply, {error, no_client_pid}, State};
                _ClientPid ->
                    % Update metadata with new progress
                    UpdatedMetadata = Metadata#{
                        last_update => Now,
                        progress_data => ProgressData
                    },
                    ets:insert(?ETS_TABLE, {Token, UpdatedMetadata}),

                    % Send notification to client
                    Notification = build_progress_notification(Token, ProgressData),
                    ProgressMsg = erlmcp_json_rpc:encode_notification(
                        <<"notifications/progress">>,
                        Notification
                    ),

                    % Send via transport if available
                    try
                        case TransportPid of
                            undefined -> ok;
                            _ ->
                                gen_server:cast(TransportPid, {send_data, ProgressMsg})
                        end,

                        logger:debug("Progress sent: token=~s, percentage=~w",
                            [Token, maps:get(percentage, ProgressData, undefined)]),
                        {reply, ok, State}
                    catch
                        SendError:SendReason ->
                            logger:error("Failed to send progress: ~p:~p", [SendError, SendReason]),
                            {reply, {error, SendReason}, State}
                    end
            end
    end;

handle_call({get_progress, Token}, _From, State) ->
    case ets:lookup(?ETS_TABLE, Token) of
        [] ->
            {reply, {error, not_found}, State};
        [{Token, Metadata}] ->
            {reply, {ok, Metadata}, State}
    end;

handle_call(list_active_tokens, _From, State) ->
    Tokens = ets:match(?ETS_TABLE, {'$1', '_'}),
    FlatTokens = lists:flatten(Tokens),
    {reply, FlatTokens, State};

handle_call({check_timeout, Token}, _From, State) ->
    case ets:lookup(?ETS_TABLE, Token) of
        [] ->
            {reply, {error, token_not_found}, State};
        [{Token, Metadata}] ->
            LastUpdate = maps:get(last_update, Metadata),
            Now = erlang:system_time(millisecond),
            ElapsedMs = Now - LastUpdate,

            if
                ElapsedMs > ?PROGRESS_TIMEOUT_MS ->
                    logger:warning("Progress token timeout: ~s (elapsed: ~Bms)", [Token, ElapsedMs]),
                    ets:delete(?ETS_TABLE, Token),
                    {reply, {error, timeout}, State};
                true ->
                    {reply, ok, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({cleanup_completed, Token}, State) ->
    ets:delete(?ETS_TABLE, Token),
    logger:debug("Cleaned up progress token: ~s", [Token]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(cleanup_timeout_tokens, State) ->
    % Check all tokens for timeout and remove stale ones
    Now = erlang:system_time(millisecond),

    ets:foldl(fun({Token, Metadata}, Acc) ->
        LastUpdate = maps:get(last_update, Metadata),
        ElapsedMs = Now - LastUpdate,

        if
            ElapsedMs > ?PROGRESS_TIMEOUT_MS ->
                logger:info("Cleaning up stale progress token: ~s", [Token]),
                ets:delete(?ETS_TABLE, Token),
                Acc;
            true ->
                Acc
        end
    end, ok, ?ETS_TABLE),

    % Reschedule cleanup
    Timer = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_timeout_tokens),
    {noreply, State#state{cleanup_timer = Timer}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Clean up tokens for crashed client processes
    Pattern = {'_', #{client_pid => Pid, '$_' => '_'}},
    Tokens = ets:match(?ETS_TABLE, Pattern),
    lists:foreach(fun([Token]) ->
        ets:delete(?ETS_TABLE, Token),
        logger:info("Cleaned up progress token for dead client: ~s", [Token])
    end, Tokens),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, #state{cleanup_timer = Timer}) ->
    if Timer =/= undefined -> erlang:cancel_timer(Timer); true -> ok end,
    logger:info("Progress notification manager stopped: ~p", [Reason]),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Build progress notification payload
-spec build_progress_notification(progress_token(), progress_data()) -> map().
build_progress_notification(Token, ProgressData) ->
    Base = #{
        <<"progressToken">> => Token
    },

    % Add percentage if available
    WithPercentage = case maps:get(percentage, ProgressData, undefined) of
        undefined -> Base;
        Pct -> Base#{<<"percentage">> => Pct}
    end,

    % Add absolute progress if available
    WithAbsolute = case maps:get(absolute, ProgressData, undefined) of
        undefined -> WithPercentage;
        {Current, Total} ->
            WithPercentage#{
                <<"current">> => Current,
                <<"total">> => Total
            }
    end,

    % Add message if available
    WithMessage = case maps:get(message, ProgressData, undefined) of
        undefined -> WithAbsolute;
        Msg -> WithAbsolute#{<<"message">> => Msg}
    end,

    % Add context if available
    case maps:get(context, ProgressData, undefined) of
        undefined -> WithMessage;
        Context -> WithMessage#{<<"context">> => Context}
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

progress_token_generation_test_() ->
    {setup,
        fun() -> {ok, _} = start_link() end,
        fun(_) -> stop() end,
        [
            ?_test(test_generate_unique_tokens()),
            ?_test(test_token_format()),
            ?_test(test_track_tool_call()),
            ?_test(test_get_progress()),
            ?_test(test_send_progress()),
            ?_test(test_list_active_tokens()),
            ?_test(test_cleanup_completed()),
            ?_test(test_timeout_detection()),
            ?_test(test_progress_data_formats()),
            ?_test(test_client_monitor())
        ]
    }.

test_generate_unique_tokens() ->
    Token1 = generate_token(),
    Token2 = generate_token(),
    ?assert(is_binary(Token1)),
    ?assert(is_binary(Token2)),
    ?assertNotEqual(Token1, Token2).

test_token_format() ->
    Token = generate_token(),
    ?assert(is_binary(Token)),
    Parts = binary:split(Token, <<"-">>),
    ?assertEqual(2, length(Parts)),
    [TimestampPart, RandomPart] = Parts,
    ?assert(try binary_to_integer(TimestampPart), true catch _ -> false end),
    ?assert(try binary_to_integer(RandomPart), true catch _ -> false end).

test_track_tool_call() ->
    Token = generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(1000) end),

    ok = track_tool_call(Token, <<"test_tool">>, ClientPid),
    {ok, Metadata} = get_progress(Token),

    ?assertEqual(<<"test_tool">>, maps:get(tool_name, Metadata)),
    ?assertEqual(ClientPid, maps:get(client_pid, Metadata)),
    ?assert(is_integer(maps:get(start_time, Metadata))),

    exit(ClientPid, kill).

test_get_progress() ->
    Token = generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(1000) end),
    track_tool_call(Token, <<"my_tool">>, ClientPid),

    {ok, Metadata} = get_progress(Token),
    ?assertEqual(<<"my_tool">>, maps:get(tool_name, Metadata)),

    {error, not_found} = get_progress(<<"nonexistent">>),

    exit(ClientPid, kill).

test_send_progress() ->
    Token = generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(5000) end),
    track_tool_call(Token, <<"processing">>, ClientPid),

    ProgressData = #{
        percentage => 50.0,
        message => <<"Processing...">>
    },

    ok = send_progress(Token, ProgressData, undefined, undefined),

    {ok, Updated} = get_progress(Token),
    ?assertEqual(ProgressData, maps:get(progress_data, Updated)),

    exit(ClientPid, kill).

test_list_active_tokens() ->
    Token1 = generate_token(),
    Token2 = generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(5000) end),

    track_tool_call(Token1, <<"tool1">>, ClientPid),
    track_tool_call(Token2, <<"tool2">>, ClientPid),

    Active = list_active_tokens(),
    ?assert(lists:member(Token1, Active)),
    ?assert(lists:member(Token2, Active)),

    exit(ClientPid, kill).

test_cleanup_completed() ->
    Token = generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(1000) end),
    track_tool_call(Token, <<"test">>, ClientPid),

    {ok, _} = get_progress(Token),
    cleanup_completed(Token),

    {error, not_found} = get_progress(Token),

    exit(ClientPid, kill).

test_timeout_detection() ->
    Token = generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(1000) end),
    track_tool_call(Token, <<"timeout_test">>, ClientPid),

    % Should be ok initially
    ok = check_timeout(Token),

    % Wait for timeout
    timer:sleep(35000),
    {error, timeout} = check_timeout(Token),

    exit(ClientPid, kill).

test_progress_data_formats() ->
    Token1 = generate_token(),
    Token2 = generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(5000) end),

    track_tool_call(Token1, <<"tool1">>, ClientPid),
    track_tool_call(Token2, <<"tool2">>, ClientPid),

    % Test percentage format
    ProgressPct = #{percentage => 75.5},
    ok = send_progress(Token1, ProgressPct, undefined, undefined),

    % Test absolute format
    ProgressAbs = #{absolute => {25, 100}},
    ok = send_progress(Token2, ProgressAbs, undefined, undefined),

    {ok, M1} = get_progress(Token1),
    {ok, M2} = get_progress(Token2),

    ?assertEqual(ProgressPct, maps:get(progress_data, M1)),
    ?assertEqual(ProgressAbs, maps:get(progress_data, M2)),

    exit(ClientPid, kill).

test_client_monitor() ->
    Token = generate_token(),
    ClientPid = spawn(fun() -> ok end),

    track_tool_call(Token, <<"monitor_test">>, ClientPid),
    timer:sleep(100),  % Let process die

    % Give monitor time to trigger
    timer:sleep(100),

    % Token should be cleaned up
    timer:sleep(1000),
    {error, not_found} = get_progress(Token).

-endif.
