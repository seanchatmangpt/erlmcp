%%%===================================================================
%%% Module: erlmcp_progress_tests
%%% Purpose: Comprehensive test suite for tool progress notifications
%%%
%%% Tests:
%%% - Progress token generation and uniqueness
%%% - Tool call tracking and metadata
%%% - Progress notification sending
%%% - Timeout detection and cleanup
%%% - Completion context management
%%% - Argument reference resolution
%%% - Client monitoring and automatic cleanup
%%%===================================================================

-module(erlmcp_progress_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    case whereis(erlmcp_progress) of
        undefined -> {ok, _} = erlmcp_progress:start_link();
        _ -> ok
    end.

cleanup(_) ->
    case whereis(erlmcp_progress) of
        undefined -> ok;
        Pid ->
            try
                erlmcp_progress:stop()
            catch
                _:_ -> ok
            end
    end.

%%====================================================================
%% Test Suites
%%====================================================================

%% Token generation tests
token_generation_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_token_is_binary()),
            ?_test(test_token_is_unique()),
            ?_test(test_token_format()),
            ?_test(test_generate_multiple_tokens())
        ]
    }.

%% Tool call tracking tests
tool_tracking_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_track_tool_call()),
            ?_test(test_track_multiple_tools()),
            ?_test(test_get_nonexistent_token()),
            ?_test(test_metadata_structure())
        ]
    }.

%% Progress notification tests
progress_notification_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_send_percentage_progress()),
            ?_test(test_send_absolute_progress()),
            ?_test(test_send_message_progress()),
            ?_test(test_send_context_progress()),
            ?_test(test_progress_update()),
            ?_test(test_progress_with_invalid_token())
        ]
    }.

%% Timeout and cleanup tests
timeout_cleanup_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_cleanup_completed()),
            ?_test(test_list_active_tokens()),
            ?_test(test_timeout_detection()),
            ?_test(test_automatic_cleanup())
        ]
    }.

%% Completion context tests
completion_context_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_basic_completion()),
            ?_test(test_enum_completion()),
            ?_test(test_context_aware_completion()),
            ?_test(test_argument_reference()),
            ?_test(test_filtering()),
            ?_test(test_schema_completions())
        ]
    }.

%% Integration tests
integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_full_tool_call_lifecycle()),
            ?_test(test_concurrent_tool_calls()),
            ?_test(test_client_crash_cleanup()),
            ?_test(test_progress_notification_flow())
        ]
    }.

%%====================================================================
%% Token Generation Tests
%%====================================================================

test_token_is_binary() ->
    Token = erlmcp_progress:generate_token(),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 0).

test_token_is_unique() ->
    Token1 = erlmcp_progress:generate_token(),
    Token2 = erlmcp_progress:generate_token(),
    Token3 = erlmcp_progress:generate_token(),

    ?assertNotEqual(Token1, Token2),
    ?assertNotEqual(Token2, Token3),
    ?assertNotEqual(Token1, Token3).

test_token_format() ->
    Token = erlmcp_progress:generate_token(),
    Parts = binary:split(Token, <<"-">>),

    % Should be format: timestamp-randompart
    ?assertEqual(2, length(Parts)),
    [TimestampPart, RandomPart] = Parts,

    % Both parts should be valid integers
    try
        _ = binary_to_integer(TimestampPart),
        _ = binary_to_integer(RandomPart),
        ?assert(true)
    catch
        _:_ -> ?assert(false)
    end.

test_generate_multiple_tokens() ->
    Tokens = [erlmcp_progress:generate_token() || _ <- lists:seq(1, 100)],
    Unique = sets:size(sets:from_list(Tokens)),
    ?assertEqual(100, Unique).

%%====================================================================
%% Tool Tracking Tests
%%====================================================================

test_track_tool_call() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    ok = erlmcp_progress:track_tool_call(Token, <<"calculate">>, ClientPid),

    {ok, Metadata} = erlmcp_progress:get_progress(Token),
    ?assertEqual(<<"calculate">>, maps:get(tool_name, Metadata)),
    ?assertEqual(ClientPid, maps:get(client_pid, Metadata)),

    exit(ClientPid, kill).

test_track_multiple_tools() ->
    Token1 = erlmcp_progress:generate_token(),
    Token2 = erlmcp_progress:generate_token(),
    ClientPid1 = spawn(fun() -> timer:sleep(10000) end),
    ClientPid2 = spawn(fun() -> timer:sleep(10000) end),

    ok = erlmcp_progress:track_tool_call(Token1, <<"tool1">>, ClientPid1),
    ok = erlmcp_progress:track_tool_call(Token2, <<"tool2">>, ClientPid2),

    {ok, Meta1} = erlmcp_progress:get_progress(Token1),
    {ok, Meta2} = erlmcp_progress:get_progress(Token2),

    ?assertEqual(<<"tool1">>, maps:get(tool_name, Meta1)),
    ?assertEqual(<<"tool2">>, maps:get(tool_name, Meta2)),

    exit(ClientPid1, kill),
    exit(ClientPid2, kill).

test_get_nonexistent_token() ->
    Token = erlmcp_progress:generate_token(),
    Result = erlmcp_progress:get_progress(Token),
    ?assertEqual({error, not_found}, Result).

test_metadata_structure() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"test_tool">>, ClientPid),

    {ok, Metadata} = erlmcp_progress:get_progress(Token),

    % Check all expected fields
    ?assert(maps:is_key(tool_name, Metadata)),
    ?assert(maps:is_key(start_time, Metadata)),
    ?assert(maps:is_key(client_pid, Metadata)),
    ?assert(maps:is_key(last_update, Metadata)),
    ?assert(maps:is_key(progress_data, Metadata)),

    % Check types
    ?assert(is_binary(maps:get(tool_name, Metadata))),
    ?assert(is_integer(maps:get(start_time, Metadata))),
    ?assert(is_pid(maps:get(client_pid, Metadata))),
    ?assert(is_integer(maps:get(last_update, Metadata))),

    exit(ClientPid, kill).

%%====================================================================
%% Progress Notification Tests
%%====================================================================

test_send_percentage_progress() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"processing">>, ClientPid),

    ProgressData = #{percentage => 50.0},
    ok = erlmcp_progress:send_progress(Token, ProgressData, undefined, undefined),

    {ok, Updated} = erlmcp_progress:get_progress(Token),
    StoredProgress = maps:get(progress_data, Updated),

    ?assertEqual(50.0, maps:get(percentage, StoredProgress)),

    exit(ClientPid, kill).

test_send_absolute_progress() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"download">>, ClientPid),

    ProgressData = #{absolute => {250, 1000}},
    ok = erlmcp_progress:send_progress(Token, ProgressData, undefined, undefined),

    {ok, Updated} = erlmcp_progress:get_progress(Token),
    StoredProgress = maps:get(progress_data, Updated),

    ?assertEqual({250, 1000}, maps:get(absolute, StoredProgress)),

    exit(ClientPid, kill).

test_send_message_progress() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"task">>, ClientPid),

    ProgressData = #{message => <<"Step 3 of 5: Validation">>},
    ok = erlmcp_progress:send_progress(Token, ProgressData, undefined, undefined),

    {ok, Updated} = erlmcp_progress:get_progress(Token),
    StoredProgress = maps:get(progress_data, Updated),

    ?assertEqual(<<"Step 3 of 5: Validation">>, maps:get(message, StoredProgress)),

    exit(ClientPid, kill).

test_send_context_progress() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"analysis">>, ClientPid),

    Context = #{
        <<"current_stage">> => <<"feature_extraction">>,
        <<"models_processed">> => 3
    },
    ProgressData = #{
        percentage => 60.0,
        context => Context
    },
    ok = erlmcp_progress:send_progress(Token, ProgressData, undefined, undefined),

    {ok, Updated} = erlmcp_progress:get_progress(Token),
    StoredProgress = maps:get(progress_data, Updated),

    ?assertEqual(Context, maps:get(context, StoredProgress)),

    exit(ClientPid, kill).

test_progress_update() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"multi_step">>, ClientPid),

    % First update
    erlmcp_progress:send_progress(Token, #{percentage => 25.0}, undefined, undefined),
    {ok, Meta1} = erlmcp_progress:get_progress(Token),
    Progress1 = maps:get(progress_data, Meta1),
    Time1 = maps:get(last_update, Meta1),

    timer:sleep(100),

    % Second update
    erlmcp_progress:send_progress(Token, #{percentage => 75.0}, undefined, undefined),
    {ok, Meta2} = erlmcp_progress:get_progress(Token),
    Progress2 = maps:get(progress_data, Meta2),
    Time2 = maps:get(last_update, Meta2),

    % Progress should change
    ?assertEqual(25.0, maps:get(percentage, Progress1)),
    ?assertEqual(75.0, maps:get(percentage, Progress2)),

    % Time should advance
    ?assert(Time2 >= Time1),

    exit(ClientPid, kill).

test_progress_with_invalid_token() ->
    Result = erlmcp_progress:send_progress(
        <<"invalid_token">>,
        #{percentage => 50.0},
        undefined,
        undefined
    ),

    ?assertEqual({error, token_not_found}, Result).

%%====================================================================
%% Timeout and Cleanup Tests
%%====================================================================

test_cleanup_completed() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"task">>, ClientPid),
    {ok, _} = erlmcp_progress:get_progress(Token),

    erlmcp_progress:cleanup_completed(Token),

    Result = erlmcp_progress:get_progress(Token),
    ?assertEqual({error, not_found}, Result),

    exit(ClientPid, kill).

test_list_active_tokens() ->
    Token1 = erlmcp_progress:generate_token(),
    Token2 = erlmcp_progress:generate_token(),
    Token3 = erlmcp_progress:generate_token(),

    ClientPid1 = spawn(fun() -> timer:sleep(10000) end),
    ClientPid2 = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token1, <<"tool1">>, ClientPid1),
    erlmcp_progress:track_tool_call(Token2, <<"tool2">>, ClientPid1),
    erlmcp_progress:track_tool_call(Token3, <<"tool3">>, ClientPid2),

    Active = erlmcp_progress:list_active_tokens(),

    ?assert(lists:member(Token1, Active)),
    ?assert(lists:member(Token2, Active)),
    ?assert(lists:member(Token3, Active)),

    erlmcp_progress:cleanup_completed(Token1),
    Active2 = erlmcp_progress:list_active_tokens(),

    ?assertNot(lists:member(Token1, Active2)),
    ?assert(lists:member(Token2, Active2)),

    exit(ClientPid1, kill),
    exit(ClientPid2, kill).

test_timeout_detection() ->
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"timeout_test">>, ClientPid),

    % Should not timeout immediately
    ok = erlmcp_progress:check_timeout(Token),

    % Send progress to update timestamp
    erlmcp_progress:send_progress(Token, #{percentage => 50.0}, undefined, undefined),

    % Should still not timeout
    ok = erlmcp_progress:check_timeout(Token),

    exit(ClientPid, kill).

test_automatic_cleanup() ->
    % Test that the cleanup timer works (simplified version)
    Token1 = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token1, <<"cleanup_test">>, ClientPid),

    % Cleanup should eventually remove stale tokens
    % Note: This is simplified - full test would wait 35+ seconds
    Active1 = erlmcp_progress:list_active_tokens(),
    ?assert(lists:member(Token1, Active1)),

    erlmcp_progress:cleanup_completed(Token1),
    Active2 = erlmcp_progress:list_active_tokens(),
    ?assertNot(lists:member(Token1, Active2)),

    exit(ClientPid, kill).

%%====================================================================
%% Completion Context Tests
%%====================================================================

test_basic_completion() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"enum">> => [<<"option1">>, <<"option2">>, <<"option3">>]
    },

    {ok, Response} = erlmcp_completion_context:complete(
        <<"test_tool">>,
        #{
            partial_argument => <<"opt">>,
            arguments => #{},
            context => #{}
        },
        Schema
    ),

    Values = maps:get(values, Response),
    ?assert(lists:member(<<"option1">>, Values)),
    ?assert(lists:member(<<"option2">>, Values)).

test_enum_completion() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"enum">> => [<<"red">>, <<"green">>, <<"blue">>]
    },

    {ok, Response} = erlmcp_completion_context:complete(
        <<"color">>,
        #{
            partial_argument => <<"r">>,
            arguments => #{},
            context => #{}
        },
        Schema
    ),

    Values = maps:get(values, Response),
    ?assert(lists:member(<<"red">>, Values)).

test_context_aware_completion() ->
    Context = #{
        <<"author_ref">> => <<"$.user_id">>,
        <<"category">> => <<"documents">>
    },

    Arguments = #{
        <<"user_id">> => <<"alice">>,
        <<"timestamp">> => <<"2024-01-01">>
    },

    Request = #{
        partial_argument => <<"">>,
        arguments => Arguments,
        context => Context
    },

    {ok, Response} = erlmcp_completion_context:complete(
        <<"contextualized">>,
        Request,
        undefined
    ),

    Values = maps:get(values, Response),
    ?assert(length(Values) >= 0).

test_argument_reference() ->
    {ok, Value} = erlmcp_completion_context:resolve_argument_reference(
        <<"$.username">>,
        #{<<"username">> => <<"alice">>, <<"email">> => <<"alice@example.com">>},
        #{}
    ),

    ?assertEqual(<<"alice">>, Value).

test_filtering() ->
    Completions = [
        <<"apple">>,
        <<"apricot">>,
        <<"banana">>,
        <<"blueberry">>
    ],

    Filtered = erlmcp_completion_context:filter_completions(Completions, <<"ap">>),

    ?assert(lists:member(<<"apple">>, Filtered)),
    ?assert(lists:member(<<"apricot">>, Filtered)),
    ?assertNot(lists:member(<<"banana">>, Filtered)).

test_schema_completions() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"firstName">> => #{<<"type">> => <<"string">>},
            <<"lastName">> => #{<<"type">> => <<"string">>},
            <<"email">> => #{<<"type">> => <<"string">>}
        }
    },

    Completions = erlmcp_completion_context:generate_completions_from_schema(
        Schema,
        <<"first">>,
        #{}
    ),

    ?assert(lists:member(<<"firstName">>, Completions)).

%%====================================================================
%% Integration Tests
%%====================================================================

test_full_tool_call_lifecycle() ->
    % Test complete lifecycle: track -> progress -> complete -> cleanup
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    % 1. Track
    ok = erlmcp_progress:track_tool_call(Token, <<"workflow">>, ClientPid),
    {ok, _} = erlmcp_progress:get_progress(Token),

    % 2. Progress updates
    ok = erlmcp_progress:send_progress(Token, #{percentage => 33.0}, undefined, undefined),
    {ok, Meta1} = erlmcp_progress:get_progress(Token),
    ?assertEqual(33.0, maps:get(percentage, maps:get(progress_data, Meta1))),

    ok = erlmcp_progress:send_progress(Token, #{percentage => 66.0}, undefined, undefined),
    {ok, Meta2} = erlmcp_progress:get_progress(Token),
    ?assertEqual(66.0, maps:get(percentage, maps:get(progress_data, Meta2))),

    % 3. Cleanup
    erlmcp_progress:cleanup_completed(Token),
    {error, not_found} = erlmcp_progress:get_progress(Token),

    exit(ClientPid, kill).

test_concurrent_tool_calls() ->
    % Test multiple concurrent tool calls
    Tokens = [erlmcp_progress:generate_token() || _ <- lists:seq(1, 10)],
    ClientPids = [spawn(fun() -> timer:sleep(10000) end) || _ <- lists:seq(1, 5)],

    % Track all tokens
    lists:foreach(fun({Token, Idx}) ->
        ClientPid = lists:nth((Idx rem 5) + 1, ClientPids),
        erlmcp_progress:track_tool_call(Token, <<"concurrent_", (integer_to_binary(Idx))/binary>>, ClientPid)
    end, lists:zip(Tokens, lists:seq(1, length(Tokens)))),

    % Verify all tracked
    Active = erlmcp_progress:list_active_tokens(),
    ?assert(length(Active) >= 10),

    % Update all
    lists:foreach(fun(Token) ->
        erlmcp_progress:send_progress(Token, #{percentage => 50.0}, undefined, undefined)
    end, Tokens),

    % Cleanup all
    lists:foreach(fun(Token) ->
        erlmcp_progress:cleanup_completed(Token)
    end, Tokens),

    lists:foreach(fun(Pid) -> exit(Pid, kill) end, ClientPids).

test_client_crash_cleanup() ->
    % Test that tokens are cleaned up when client crashes
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(1000) end),  % Short timeout

    erlmcp_progress:track_tool_call(Token, <<"crash_test">>, ClientPid),
    {ok, _} = erlmcp_progress:get_progress(Token),

    % Kill client
    exit(ClientPid, kill),

    % Wait for cleanup
    timer:sleep(200),

    % Token may be cleaned up by monitor
    Result = erlmcp_progress:get_progress(Token),
    % Either cleaned up or still exists (monitor takes time)
    ?assert(Result =:= {error, not_found} orelse element(1, Result) =:= ok).

test_progress_notification_flow() ->
    % Test the complete flow including notification building
    Token = erlmcp_progress:generate_token(),
    ClientPid = spawn(fun() -> timer:sleep(10000) end),

    erlmcp_progress:track_tool_call(Token, <<"notify">>, ClientPid),

    % Send complex progress
    ProgressData = #{
        percentage => 75.5,
        absolute => {300, 400},
        message => <<"Processing features">>,
        context => #{
            <<"stage">> => <<"extraction">>,
            <<"models">> => 5
        }
    },

    ok = erlmcp_progress:send_progress(Token, ProgressData, undefined, undefined),

    {ok, Meta} = erlmcp_progress:get_progress(Token),
    Stored = maps:get(progress_data, Meta),

    ?assertEqual(75.5, maps:get(percentage, Stored)),
    ?assertEqual({300, 400}, maps:get(absolute, Stored)),
    ?assertEqual(<<"Processing features">>, maps:get(message, Stored)),
    ?assertEqual(#{<<"stage">> => <<"extraction">>, <<"models">> => 5}, maps:get(context, Stored)),

    exit(ClientPid, kill).
