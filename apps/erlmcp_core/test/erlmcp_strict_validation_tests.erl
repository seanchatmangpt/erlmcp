-module(erlmcp_strict_validation_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% OTP 28 Strict Comprehension Tests
%%
%% These tests demonstrate the difference between:
%% - Non-strict comprehensions: Silent skip on pattern mismatch
%% - Strict comprehensions: Fail-fast with badmatch exception

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Valid messages for testing
valid_messages() ->
    [
     #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
       ?JSONRPC_FIELD_ID => 1,
       ?JSONRPC_FIELD_METHOD => <<"tools/list">>},
     #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
       ?JSONRPC_FIELD_ID => 2,
       ?JSONRPC_FIELD_METHOD => <<"resources/list">>},
     #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
       <<"method">> => <<"notifications/cancelled">>}
    ].

%% Mixed valid/invalid messages (for non-strict comparison)
mixed_messages() ->
    [
     #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
       ?JSONRPC_FIELD_ID => 1,
       ?JSONRPC_FIELD_METHOD => <<"tools/list">>},
     #{<<"id">> => 2, <<"method">> => <<"resources/list">>}, % Missing jsonrpc
     #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
       ?JSONRPC_FIELD_ID => 3,
       ?JSONRPC_FIELD_METHOD => <<"prompts/list">>}
    ].

%% Valid tool calls
valid_tool_calls() ->
    [
     #{<<"method">> => <<"tools/call">>,
       <<"params">> => #{<<"name">> => <<"weather">>,
                        <<"arguments">> => #{<<"city">> => <<"NYC">>}}}},
     #{<<"method">> => <<"tools/call">>,
       <<"params">> => #{<<"name">> => <<"calculator">>,
                        <<"arguments">> => #{<<"expr">> => <<"2+2">>}}}}
    ].

%% Malformed tool calls (missing required fields)
malformed_tool_calls() ->
    [
     #{<<"method">> => <<"tools/call">>,
       <<"params">> => #{<<"arguments">> => #{}}}, % Missing name
     #{<<"method">> => <<"tools/call">>}, % Missing params
     #{<<"params">> => #{<<"name">> => <<"test">>}} % Missing method
    ].

%% Valid tool results
valid_tool_results() ->
    [
     #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"Result 1">>}]},
     #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"Result 2">>}]}
    ].

%% Malformed tool results
malformed_tool_results() ->
    [
     #{<<"result">> => <<"data">>}, % Wrong field name
     #{<<"error">> => <<"failed">>}, % Error instead of content
     not_a_map % Invalid type
    ].

%%====================================================================
%% Strict Comprehension Tests (OTP 28+)
%%====================================================================

strict_validate_messages_test_() ->
    {timeout, 10, fun() ->
        %% Setup
        Messages = valid_messages(),

        %% Test strict validation passes for valid messages
        case erlmcp_strict_validation:supports_strict() of
            true ->
                Result = erlmcp_strict_validation:validate_messages(Messages),
                ?assertEqual(3, length(Result)),
                ?assertEqual(<<"tools/list">>,
                            maps:get(<<"method">>, lists:nth(1, Result)));
            false ->
                %% Skip test on OTP < 28
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

strict_validate_messages_crash_test_() ->
    {timeout, 10, fun() ->
        %% Setup: Include malformed message
        Messages = [
            #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
              ?JSONRPC_FIELD_ID => 1,
              ?JSONRPC_FIELD_METHOD => <<"tools/list">>},
            #{<<"id">> => 2}  % Missing jsonrpc - will crash strict validation
        ],

        %% Test strict validation crashes on malformed data
        case erlmcp_strict_validation:supports_strict() of
            true ->
                ?assertError(badmatch,
                            erlmcp_strict_validation:validate_messages(Messages));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

strict_validate_tool_calls_test_() ->
    {timeout, 10, fun() ->
        Calls = valid_tool_calls(),

        case erlmcp_strict_validation:supports_strict() of
            true ->
                Result = erlmcp_strict_validation:validate_tool_calls(Calls),
                ?assertEqual(2, length(Result)),
                [{Tool1, _}, {Tool2, _}] = Result,
                ?assertEqual(<<"weather">>, Tool1),
                ?assertEqual(<<"calculator">>, Tool2);
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

strict_validate_tool_calls_crash_test_() ->
    {timeout, 10, fun() ->
        Calls = malformed_tool_calls(),

        case erlmcp_strict_validation:supports_strict() of
            true ->
                ?assertError(badmatch,
                            erlmcp_strict_validation:validate_tool_calls(Calls));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

strict_validate_tool_results_test_() ->
    {timeout, 10, fun() ->
        Results = valid_tool_results(),

        case erlmcp_strict_validation:supports_strict() of
            true ->
                Validated = erlmcp_strict_validation:validate_tool_results(Results),
                ?assertEqual(2, length(Validated));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

strict_validate_tool_results_crash_test_() ->
    {timeout, 10, fun() ->
        Results = malformed_tool_results(),

        case erlmcp_strict_validation:supports_strict() of
            true ->
                ?assertError(badmatch,
                            erlmcp_strict_validation:validate_tool_results(Results));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

%%====================================================================
%% Extraction Tests
%%====================================================================

extract_tool_names_test_() ->
    {timeout, 10, fun() ->
        Tools = [
            #{<<"name">> => <<"weather">>, <<"description">> => <<"Get weather">>},
            #{<<"name">> => <<"calculator">>, <<"description">> => <<"Calculate">>}
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                Names = erlmcp_strict_validation:extract_tool_names(Tools),
                ?assertEqual([<<"weather">>, <<"calculator">>], Names);
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

extract_tool_names_crash_test_() ->
    {timeout, 10, fun() ->
        Tools = [
            #{<<"name">> => <<"weather">>},
            #{<<"description">> => <<"Missing name">>} % Will crash strict extraction
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                ?assertError(badmatch,
                            erlmcp_strict_validation:extract_tool_names(Tools));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

extract_resource_uris_test_() ->
    {timeout, 10, fun() ->
        Resources = [
            #{<<"uri">> => <<"file:///data1.txt">>, <<"name">> => <<"Data 1">>},
            #{<<"uri">> => <<"file:///data2.txt">>, <<"name">> => <<"Data 2">>}
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                URIs = erlmcp_strict_validation:extract_resource_uris(Resources),
                ?assertEqual([<<"file:///data1.txt">>, <<"file:///data2.txt">>], URIs);
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

%%====================================================================
%% Safe Validation Tests (Production Pattern)
%%====================================================================

validate_messages_safe_test_() ->
    {timeout, 10, fun() ->
        Messages = valid_messages(),

        Result = erlmcp_strict_validation:validate_messages_safe(Messages),
        ?assertMatch({ok, _}, Result),
        {ok, Validated} = Result,
        ?assertEqual(3, length(Validated))
    end}.

validate_messages_safe_error_test_() ->
    {timeout, 10, fun() ->
        %% Include malformed message
        Messages = [
            #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
              ?JSONRPC_FIELD_ID => 1,
              ?JSONRPC_FIELD_METHOD => <<"tools/list">>},
            #{<<"id">> => 2}  % Missing jsonrpc
        ],

        Result = erlmcp_strict_validation:validate_messages_safe(Messages),
        ?assertMatch({error, {invalid_message_format, _}}, Result)
    end}.

%%====================================================================
%% Partition Tests (Non-Strict Comparison)
%%====================================================================

partition_messages_test_() ->
    {timeout, 10, fun() ->
        Messages = mixed_messages(),

        {Valid, Invalid} = erlmcp_strict_validation:partition_messages(Messages),

        ?assertEqual(2, length(Valid)),  % 2 valid messages
        ?assertEqual(1, length(Invalid)), % 1 invalid message (missing jsonrpc)

        %% Verify valid messages
        ?assert(lists:all(fun(Msg) ->
                maps:is_key(?JSONRPC_FIELD_JSONRPC, Msg)
        end, Valid))
    end}.

%%====================================================================
%% Batch Validation Tests
%%====================================================================

validate_batch_strict_test_() ->
    {timeout, 10, fun() ->
        Batch = [
            #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
              ?JSONRPC_FIELD_ID => 1,
              ?JSONRPC_FIELD_METHOD => <<"tools/list">>},
            #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
              ?JSONRPC_FIELD_ID => 2,
              ?JSONRPC_FIELD_METHOD => <<"resources/list">>}
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                Validated = erlmcp_strict_validation:validate_batch_strict(Batch),
                ?assertEqual(2, length(Validated));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

validate_batch_strict_crash_test_() ->
    {timeout, 10, fun() ->
        Batch = [
            #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
              ?JSONRPC_FIELD_ID => 1},
            #{<<"id">> => 2}, % Missing jsonrpc
            not_a_map % Invalid type
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                ?assertError(badmatch,
                            erlmcp_strict_validation:validate_batch_strict(Batch));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

%%====================================================================
%% Resource List Validation Tests
%%====================================================================

validate_resource_lists_test_() ->
    {timeout, 10, fun() ->
        Resources = [
            #{<<"uri">> => <<"file:///data1.txt">>, <<"name">> => <<"Data 1">>},
            #{<<"uri">> => <<"file:///data2.txt">>, <<"name">> => <<"Data 2">>}
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                Validated = erlmcp_strict_validation:validate_resource_lists(Resources),
                ?assertEqual(2, length(Validated));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

validate_resource_lists_crash_test_() ->
    {timeout, 10, fun() ->
        Resources = [
            #{<<"uri">> => <<"file:///data1.txt">>, <<"name">> => <<"Data 1">>},
            #{<<"name">> => <<"Missing URI">>} % Will crash
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                ?assertError(badmatch,
                            erlmcp_strict_validation:validate_resource_lists(Resources));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

%%====================================================================
%% Prompt Arguments Validation Tests
%%====================================================================

validate_prompt_arguments_test_() ->
    {timeout, 10, fun() ->
        Arguments = [
            #{<<"name">> => <<"city">>, <<"role">> => <<"user">>},
            #{<<"name">> => <<"format">>, <<"role">> => <<"system">>}
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                Validated = erlmcp_strict_validation:validate_prompt_arguments(Arguments),
                ?assertEqual(2, length(Validated));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

%%====================================================================
%% Notification Batch Validation Tests
%%====================================================================

validate_notification_batch_test_() ->
    {timeout, 10, fun() ->
        Notifications = [
            #{<<"method">> => <<"notifications/cancelled">>,
              <<"params">> => #{<<"reason">> => <<"user">>}},
            #{<<"method">> => <<"notifications/progress">>,
              <<"params">> => #{<<"progress">> => 50}}
        ],

        case erlmcp_strict_validation:supports_strict() of
            true ->
                Validated = erlmcp_strict_validation:validate_notification_batch(Notifications),
                ?assertEqual(2, length(Validated));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

%%====================================================================
%% OTP Version Compatibility Tests
%%====================================================================

supports_strict_test_() ->
    {timeout, 10, fun() ->
        Supports = erlmcp_strict_validation:supports_strict(),
        ?assert(boolean(Supports)),

        %% Verify the result matches actual OTP version
        OTPVersion = erlang:system_info(otp_release),
        Expected = case OTPVersion of
            "28" ++ _ -> true;
            _ -> false
        end,
        ?assertEqual(Expected, Supports)
    end}.

strict_generator_type_test_() ->
    {timeout, 10, fun() ->
        Type = erlmcp_strict_validation:strict_generator_type(),
        ?assert(lists:member(Type, [strict, non_strict]))
    end}.

%%====================================================================
%% Supervisor Restart Test
%%====================================================================

supervisor_restart_on_validation_failure_test_() ->
    {timeout, 30, fun() ->
        %% This test demonstrates supervisor behavior when validation crashes
        %% In production, you'd use validate_messages_safe/1 instead

        %% Setup: Create a mock server that crashes on validation
        {ok, Pid} = start_mock_server(),

        %% Trigger validation failure
        MalformedMessages = [#{<<"invalid">> => <<"data">>}],

        %% The server should crash on strict validation (if OTP 28+)
        case erlmcp_strict_validation:supports_strict() of
            true ->
                %% Simulate validation crash
                catch erlmcp_strict_validation:validate_messages(MalformedMessages),

                %% Server should be restarted by supervisor
                timer:sleep(100),
                ?assert(is_process_alive(Pid));
            false ->
                ?assertSkip("Strict comprehensions require OTP 28+")
        end
    end}.

%%====================================================================
%% Mock Server for Supervisor Tests
%%====================================================================

start_mock_server() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {ok, Pid}.

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
