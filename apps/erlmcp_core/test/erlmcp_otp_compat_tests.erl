%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Compatibility Layer Test Suite
%%%
%%% Tests the OTP compatibility layer for:
%%% - Version detection (compile-time and runtime)
%%% - Feature detection macros
%%% - Safe API shims
%%% - JSON encoding/decoding fallbacks
%%% - Process enumeration safety
%%%
%%% Chicago School TDD:
%%% - Real OTP version detection
%%% - Observable behavior only
%%% - No mocks or fakes
%%% - Tests work on both OTP 27 and 28
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp_compat_tests).

-include_lib("eunit/include/eunit.hrl").
-include("otp_compat.hrl").

%%====================================================================
%% Test Macros
%%====================================================================

%% Test file exists and can be included
otp_compat_header_exists_test() ->
    ?assert(true).

%%====================================================================
%% Version Detection Tests
%%====================================================================

otp_version_test() ->
    Version = erlmcp_otp_compat:otp_version(),
    case Version of
        {Major, _, _} when is_integer(Major) ->
            ?assert(Major >= 25);
        _ ->
            ?assert(false, {invalid_version_format, Version})
    end.

is_otp_28_plus_test() ->
    Is28Plus = erlmcp_otp_compat:is_otp_28_plus(),
    ?assert(is_boolean(Is28Plus)),

    %% Verify consistent with HAVE_PRIORITY_MESSAGES macro
    MacroResult = ?HAVE_PRIORITY_MESSAGES,
    ?assertEqual(Is28Plus, MacroResult).

is_otp_27_plus_test() ->
    Is27Plus = erlmcp_otp_compat:is_otp_27_plus(),
    ?assert(is_boolean(Is27Plus)),

    %% Verify consistent with HAVE_NATIVE_JSON macro
    MacroResult = ?HAVE_NATIVE_JSON,
    ?assertEqual(Is27Plus, MacroResult).

%%====================================================================
%% Feature Detection Tests
%%====================================================================

native_json_detection_test() ->
    IsAvailable = erlmcp_otp_compat:have_native_json(),
    ?assert(is_boolean(IsAvailable)),

    %% If available, verify functions exist
    case IsAvailable of
        true ->
            ?assert(erlang:function_exported(json, encode, 1)),
            ?assert(erlang:function_exported(json, decode, 1));
        false ->
            ok
    end.

process_iterator_detection_test() ->
    IsAvailable = erlmcp_otp_compat:have_process_iterator(),
    ?assert(is_boolean(IsAvailable)),

    %% If available, verify functions exist
    case IsAvailable of
        true ->
            ?assert(erlang:function_exported(erlang, processes_iterator, 0)),
            ?assert(erlang:function_exported(erlang, process_next, 1));
        false ->
            ok
    end.

priority_messages_detection_test() ->
    IsAvailable = erlmcp_otp_compat:have_priority_messages(),
    ?assert(is_boolean(IsAvailable)),

    %% Priority messages only on OTP 28+
    Version = erlmcp_otp_compat:otp_version(),
    case Version of
        {Major, _, _} when Major >= 28 ->
            ?assert(IsAvailable =:= true);
        _ ->
            ?assert(IsAvailable =:= false)
    end.

%%====================================================================
%% JSON Encoding/Decoding Tests
%%====================================================================

json_encode_test() ->
    TestData = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test_method">>,
        <<"params">> => #{
            <<"key">> => <<"value">>
        }
    },

    Encoded = erlmcp_otp_compat:json_encode(TestData),
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 0),
    ?assert(<< "{">> =:= binary:part(Encoded, {0, 1})).

json_decode_test() ->
    TestData = #{
        <<"test">> => <<"value">>,
        <<"number">> => 42,
        <<"nested">> => #{
            <<"key">> => true
        }
    },

    Encoded = erlmcp_otp_compat:json_encode(TestData),
    Decoded = erlmcp_otp_compat:json_decode(Encoded),

    ?assertEqual(TestData, Decoded).

json_roundtrip_test() ->
    %% Test various data types
    TestDataList = [
        #{<<"simple">> => <<"value">>},
        #{<<"number">> => 42},
        #{<<"float">> => 3.14},
        #{<<"bool">> => true},
        #{<<"null">> => null},
        #{<<"array">> => [1, 2, 3]},
        #{<<"nested">> => #{<<"deep">> => #{<<"value">> => 42}}},
        #{<<"complex">> => #{
            <<"string">> => <<"test">>,
            <<"number">> => 123,
            <<"float">> => 1.5,
            <<"bool">> => false,
            <<"null">> => null,
            <<"array">> => [<<"a">>, <<"b">>],
            <<"nested">> => #{<<"x">> => 1}
        }}
    ],

    lists:foreach(fun(TestData) ->
        Encoded = erlmcp_otp_compat:json_encode(TestData),
        Decoded = erlmcp_otp_compat:json_decode(Encoded),
        ?assertEqual(TestData, Decoded)
    end, TestDataList).

json_macro_encode_test() ->
    TestData = #{<<"test">> => <<"macro">>},

    %% Test compile-time macro
    Encoded = ?JSON_ENCODE(TestData),
    ?assert(is_binary(Encoded)),

    %% Test runtime macro
    EncodedSafe = ?JSON_ENCODE_SAFE(TestData),
    ?assert(is_binary(Encoded)),

    %% Both should produce valid JSON
    ?assertMatch(#{"test" := <<"macro">>},
                 jsx:decode(Encoded, [return_maps])),
    ?assertMatch(#{"test" := <<"macro">>},
                 jsx:decode(EncodedSafe, [return_maps])).

json_macro_decode_test() ->
    TestData = #{<<"test">> => <<"decode">>},

    %% Encode using module
    Encoded = erlmcp_otp_compat:json_encode(TestData),

    %% Test compile-time macro
    Decoded = ?JSON_DECODE(Encoded),
    ?assertEqual(TestData, Decoded),

    %% Test runtime macro
    DecodedSafe = ?JSON_DECODE_SAFE(Encoded),
    ?assertEqual(TestData, DecodedSafe).

%%====================================================================
%% Process Enumeration Tests
%%====================================================================

safe_process_count_test() ->
    Count1 = erlmcp_otp_compat:safe_process_count(),
    ?assert(is_integer(Count1)),
    ?assert(Count1 > 0),

    %% Verify macro works too
    Count2 = ?SAFE_PROCESS_COUNT(),
    ?assertEqual(Count1, Count2),

    %% Should match system_info
    SystemCount = erlang:system_info(process_count),
    ?assertEqual(SystemCount, Count1).

safe_processes_test() ->
    %% Get list of processes
    PidList = erlmcp_otp_compat:safe_processes(),
    ?assert(is_list(PidList)),
    ?assert(length(PidList) > 0),

    %% All elements should be PIDs
    lists:foreach(fun(Pid) ->
        ?assert(is_pid(Pid))
    end, PidList),

    %% Count should match
    Count = length(PidList),
    SafeCount = erlmcp_otp_compat:safe_process_count(),
    ?assertEqual(SafeCount, Count).

process_iterator_count_test() ->
    %% This test only validates that the function works
    %% It does NOT test the O(1) memory property (that's a benchmark)
    case erlmcp_otp_compat:have_process_iterator() of
        true ->
            %% Test iterator counting
            Iterator = erlang:processes_iterator(),
            Count = erlmcp_otp_compat:count_processes_iterator(Iterator, 0),

            ?assert(is_integer(Count)),
            ?assert(Count > 0),

            %% Should match system count
            SystemCount = erlang:system_info(process_count),
            ?assertEqual(SystemCount, Count);
        false ->
            %% Skip test on OTP <28
            ?assert(true)
    end.

process_iterator_to_list_test() ->
    case erlmcp_otp_compat:have_process_iterator() of
        true ->
            %% Test iterator to list conversion
            Iterator = erlang:processes_iterator(),
            PidList = erlmcp_otp_compat:processes_iterator_to_list(Iterator, []),

            ?assert(is_list(PidList)),
            ?assert(length(PidList) > 0),

            %% All elements should be PIDs
            lists:foreach(fun(Pid) ->
                ?assert(is_pid(Pid))
            end, PidList),

            %% Count should match
            Count = length(PidList),
            SystemCount = erlang:system_info(process_count),
            ?assertEqual(SystemCount, Count);
        false ->
            %% Skip test on OTP <28
            ?assert(true)
    end.

%%====================================================================
%% Priority Messages Tests
%%====================================================================

set_priority_high_test() ->
    %% This test just verifies the function doesn't crash
    Result = erlmcp_otp_compat:set_priority_high(),
    ?assertEqual(ok, Result),

    %% Test macro too
    ?SET_PRIORITY_HIGH(),
    ?assert(true).

send_priority_test() ->
    %% Spawn a test process
    Tester = self(),

    Receiver = spawn(fun() ->
        receive
            Msg -> Tester ! {received, Msg}
        end
    end),

    %% Send priority message
    Msg = {test_message, make_ref()},
    erlmcp_otp_compat:send_priority(Receiver, Msg),

    %% Verify received
    receive
        {received, Msg} ->
            ?assert(true);
        {received, Other} ->
            ?assertEqual(Msg, Other)
    after 1000 ->
        ?assert(false, "Message not received")
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

full_stack_test() ->
    %% Test the full compatibility layer stack
    TestData = #{<<"test">> => <<"full_stack">>},

    %% 1. Check OTP version
    Version = erlmcp_otp_compat:otp_version(),
    ?assertMatch({_, _, _}, Version),

    %% 2. Check features
    HasJSON = erlmcp_otp_compat:have_native_json(),
    HasIterator = erlmcp_otp_compat:have_process_iterator(),
    HasPriority = erlmcp_otp_compat:have_priority_messages(),
    ?assert(is_boolean(HasJSON)),
    ?assert(is_boolean(HasIterator)),
    ?assert(is_boolean(HasPriority)),

    %% 3. Test JSON roundtrip
    Encoded = erlmcp_otp_compat:json_encode(TestData),
    Decoded = erlmcp_otp_compat:json_decode(Encoded),
    ?assertEqual(TestData, Decoded),

    %% 4. Test process enumeration
    Count = erlmcp_otp_compat:safe_process_count(),
    ?assert(Count > 0),

    %% 5. Test priority (no-op on OTP <28)
    erlmcp_otp_compat:set_priority_high(),
    ?assert(true),

    ok.

%%====================================================================
%% Edge Cases
%%====================================================================

empty_json_test() ->
    %% Empty map
    Empty = #{},
    Encoded = erlmcp_otp_compat:json_encode(Empty),
    Decoded = erlmcp_otp_compat:json_decode(Encoded),
    ?assertEqual(Empty, Decoded).

large_json_test() ->
    %% Large nested structure
    Large = lists:foldl(fun(N, Acc) ->
        Acc#{<<"key_", (integer_to_binary(N))/binary>> => N}
    end, #{}, lists:seq(1, 100)),

    Encoded = erlmcp_otp_compat:json_encode(Large),
    Decoded = erlmcp_otp_compat:json_decode(Encoded),
    ?assertEqual(Large, Decoded).

unicode_json_test() ->
    %% Unicode strings
    Unicode = #{
        <<"emoji">> => <<"😀🎉">>,
        <<"chinese">> => <<"你好">>,
        <<"arabic">> => <<"مرحبا">>,
        <<"russian">> => <<"Привет">>
    },

    Encoded = erlmcp_otp_compat:json_encode(Unicode),
    Decoded = erlmcp_otp_compat:json_decode(Encoded),
    ?assertEqual(Unicode, Decoded).

zero_processes_test() ->
    %% This test validates the safety functions work even if
    %% the process count is unexpectedly low (unlikely in practice)
    Count = erlmcp_otp_compat:safe_process_count(),
    ?assert(Count >= 1),  %% At least this test process

    PidList = erlmcp_otp_compat:safe_processes(),
    ?assert(length(PidList) >= 1).
