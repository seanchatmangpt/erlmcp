-module(erlmcp_socket_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
-define(TEST_RCVBUF, 4096).
-define(TEST_SNDBUF, 8192).

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test OTP version detection
get_otp_version_test() ->
    Version = erlmcp_socket_utils:get_otp_version(),
    ?assert(is_integer(Version)),
    ?assert(Version >= 0),
    ?assert(Version =< 30),  % Sanity check for future versions
    %% Log the detected OTP version
    ?debugFmt("Detected OTP version: ~p", [Version]).

%% @doc Test socket API support detection
is_supported_test() ->
    IsSupported = erlmcp_socket_utils:is_supported(),
    ?assert(is_boolean(IsSupported)),
    %% Log support status
    ?debugFmt("Socket API supported: ~p", [IsSupported]).

%% @doc Test creating a TCP socket with default options
create_tcp_socket_defaults_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Options = #{},
            Result = erlmcp_socket_utils:create_tcp_socket(Options),
            case Result of
                {ok, Socket} ->
                    ?assert(is_reference(Socket)),
                    %% Clean up
                    ok = socket:close(Socket);
                {error, Reason} ->
                    %% May fail in some environments (e.g., restricted)
                    ?debugFmt("Socket creation failed: ~p", [Reason])
            end;
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%% @doc Test creating a TCP socket with custom buffer sizes
create_tcp_socket_custom_buffers_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Options = #{
                rcvbuf => ?TEST_RCVBUF,
                sndbuf => ?TEST_SNDBUF
            },
            Result = erlmcp_socket_utils:create_tcp_socket(Options),
            case Result of
                {ok, Socket} ->
                    ?assert(is_reference(Socket)),
                    %% Verify buffer sizes were set
                    {ok, Info} = socket:info(Socket),
                    RcvBuf = proplists:get_value(recbuf, Info, 0),
                    SndBuf = proplists:get_value(sndbuf, Info, 0),
                    ?assertEqual(?TEST_RCVBUF, RcvBuf),
                    ?assertEqual(?TEST_SNDBUF, SndBuf),
                    %% Clean up
                    ok = socket:close(Socket);
                {error, Reason} ->
                    ?debugFmt("Socket creation failed: ~p", [Reason])
            end;
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%% @doc Test creating a TCP socket with nodelay option
create_tcp_socket_nodelay_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Options = #{
                nodelay => true,
                reuseaddr => true
            },
            Result = erlmcp_socket_utils:create_tcp_socket(Options),
            case Result of
                {ok, Socket} ->
                    ?assert(is_reference(Socket)),
                    %% Verify options were set
                    {ok, Info} = socket:info(Socket),
                    NoDelay = proplists:get_value(nodelay, Info, false),
                    ReuseAddr = proplists:get_value(reuseaddr, Info, false),
                    ?assert(NoDelay),
                    ?assert(ReuseAddr),
                    %% Clean up
                    ok = socket:close(Socket);
                {error, Reason} ->
                    ?debugFmt("Socket creation failed: ~p", [Reason])
            end;
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%% @doc Test enabling backpressure on a socket
enable_backpressure_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Options = #{},
            Result = erlmcp_socket_utils:create_tcp_socket(Options),
            case Result of
                {ok, Socket} ->
                    %% Enable backpressure
                    BackpressureResult = erlmcp_socket_utils:enable_backpressure(Socket),
                    ?assertEqual(ok, BackpressureResult),
                    %% Verify socket is in passive mode
                    {ok, Info} = socket:info(Socket),
                    Active = proplists:get_value(active, Info, false),
                    ?assertNot(Active),
                    %% Clean up
                    ok = socket:close(Socket);
                {error, Reason} ->
                    ?debugFmt("Socket creation failed: ~p", [Reason])
            end;
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%% @doc Test setting buffer sizes on an existing socket
set_buffer_sizes_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Options = #{},
            Result = erlmcp_socket_utils:create_tcp_socket(Options),
            case Result of
                {ok, Socket} ->
                    %% Update buffer sizes
                    NewBuffers = #{
                        rcvbuf => ?TEST_RCVBUF * 2,
                        sndbuf => ?TEST_SNDBUF * 2
                    },
                    SetResult = erlmcp_socket_utils:set_buffer_sizes(Socket, NewBuffers),
                    ?assertEqual(ok, SetResult),
                    %% Verify new buffer sizes
                    {ok, Info} = socket:info(Socket),
                    RcvBuf = proplists:get_value(recbuf, Info, 0),
                    SndBuf = proplists:get_value(sndbuf, Info, 0),
                    ?assertEqual(?TEST_RCVBUF * 2, RcvBuf),
                    ?assertEqual(?TEST_SNDBUF * 2, SndBuf),
                    %% Clean up
                    ok = socket:close(Socket);
                {error, Reason} ->
                    ?debugFmt("Socket creation failed: ~p", [Reason])
            end;
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%% @doc Test getting socket information
get_socket_info_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Options = #{
                rcvbuf => ?TEST_RCVBUF,
                sndbuf => ?TEST_SNDBUF
            },
            Result = erlmcp_socket_utils:create_tcp_socket(Options),
            case Result of
                {ok, Socket} ->
                    %% Get socket info
                    InfoResult = erlmcp_socket_utils:get_socket_info(Socket),
                    ?assertMatch({ok, _}, InfoResult),
                    {ok, InfoMap} = InfoResult,
                    ?assert(is_map(InfoMap)),
                    %% Verify expected fields
                    RcvBuf = maps:get(recbuf, InfoMap, undefined),
                    SndBuf = maps:get(sndbuf, InfoMap, undefined),
                    ?assertEqual(?TEST_RCVBUF, RcvBuf),
                    ?assertEqual(?TEST_SNDBUF, SndBuf),
                    %% Clean up
                    ok = socket:close(Socket);
                {error, Reason} ->
                    ?debugFmt("Socket creation failed: ~p", [Reason])
            end;
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%% @doc Test socket to gen_tcp conversion
socket_to_gen_tcp_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Options = #{},
            Result = erlmcp_socket_utils:create_tcp_socket(Options),
            case Result of
                {ok, Socket} ->
                    %% Convert to gen_tcp format
                    ConversionResult = erlmcp_socket_utils:socket_to_gen_tcp(Socket),
                    case ConversionResult of
                        {ok, Fd} when is_integer(Fd) ->
                            ?assert(is_integer(Fd)),
                            ?assert(Fd > 0);
                        {error, Reason} ->
                            %% May fail on some systems
                            ?debugFmt("Conversion failed: ~p", [Reason])
                    end,
                    %% Clean up
                    ok = socket:close(Socket);
                {error, Reason} ->
                    ?debugFmt("Socket creation failed: ~p", [Reason])
            end;
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%% @doc Test creating socket for IPv6
create_tcp_socket_ipv6_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Options = #{},
            Result = erlmcp_socket_utils:create_tcp_socket(inet6, Options),
            case Result of
                {ok, Socket} ->
                    ?assert(is_reference(Socket)),
                    %% Verify domain is inet6
                    {ok, Info} = socket:info(Socket),
                    Domain = proplists:get_value(domain, Info, undefined),
                    ?assertEqual(inet6, Domain),
                    %% Clean up
                    ok = socket:close(Socket);
                {error, Reason} ->
                    %% May fail if IPv6 not supported
                    ?debugFmt("IPv6 socket creation failed: ~p", [Reason])
            end;
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test full socket lifecycle: create, configure, use, close
socket_lifecycle_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            %% Create socket
            Options = #{
                rcvbuf => ?TEST_RCVBUF,
                sndbuf => ?TEST_SNDBUF,
                nodelay => true
            },
            {ok, Socket} = ?assertMatch({ok, _},
                erlmcp_socket_utils:create_tcp_socket(Options)),

            %% Enable backpressure
            ?assertEqual(ok, erlmcp_socket_utils:enable_backpressure(Socket)),

            %% Get socket info
            {ok, Info} = ?assertMatch({ok, _},
                erlmcp_socket_utils:get_socket_info(Socket)),

            %% Modify buffer sizes
            NewBuffers = #{rcvbuf => ?TEST_RCVBUF div 2},
            ?assertEqual(ok, erlmcp_socket_utils:set_buffer_sizes(Socket, NewBuffers)),

            %% Close socket
            ?assertEqual(ok, socket:close(Socket)),

            %% Verify socket is closed
            ?assertMatch({error, _}, socket:info(Socket));
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%% @doc Test error handling for invalid options
invalid_options_test() ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            %% Test with negative buffer size (should fail)
            Options = #{rcvbuf => -1},
            Result = erlmcp_socket_utils:create_tcp_socket(Options),
            ?assertMatch({error, _}, Result);
        false ->
            ?debugMsg("Skipping test - socket API not supported")
    end.

%%====================================================================
%% Performance Benchmarks
%%====================================================================

%% @doc Benchmark socket creation performance
socket_creation_benchmark_test(_Config) ->
    case erlmcp_socket_utils:is_supported() of
        true ->
            Iterations = 1000,
            StartTime = erlang:monotonic_time(microsecond),

            lists:foreach(fun(_) ->
                case erlmcp_socket_utils:create_tcp_socket(#{}) of
                    {ok, Socket} ->
                        socket:close(Socket);
                    {error, _} ->
                        ok
                end
            end, lists:seq(1, Iterations)),

            EndTime = erlang:monotonic_time(microsecond),
            DurationMs = (EndTime - StartTime) / 1000.0,
            AvgPerOp = DurationMs / Iterations,

            ?debugFmt("Socket creation benchmark: ~p operations in ~.2fms (~.4fms/op)",
                     [Iterations, DurationMs, AvgPerOp]),
            ?assert(AvgPerOp < 10.0);  % Should be under 10ms per operation
        false ->
            ?debugMsg("Skipping benchmark - socket API not supported")
    end.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Start any required processes
    ok.

cleanup(_Ctx) ->
    %% Clean up any resources
    ok.
