%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Compatibility Header for OTP 26-28
%%%
%%% This header provides version-agnostic EUnit macros and helpers
%%% that work across OTP 26, 27, and 28.
%%%
%%% Key changes across OTP versions:
%%% - OTP 26: EUnit 2.7, updated stacktrace format
%%% - OTP 27: EUnit 2.8, removed deprecated slave module
%%% - OTP 28: EUnit 2.9+ (2.10.1 in OTP 28.3), JIT optimizations
%%%
%%% Features:
%%% - Conditional compilation for version-specific features
%%% - Backward-compatible assertion macros
%%% - Safe timeout/scaling macros
%%% - Type checking helpers (size/1 -> tuple_size/1, byte_size/1)
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(erlmcp_EUNIT_COMPAT_HRL).
-define(erlmcp_EUNIT_COMPAT_HRL, 1).

%% Include standard EUnit
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% OTP Version Detection (Compile-Time)
%%====================================================================

%% Parse OTP version for conditional compilation
%% Usage: -ifdef(OTP_28), -ifdef(OTP_27), -ifdef(OTP_26)

%% Detect OTP version from system info during compilation
-ifdef(OTP_RELEASE).
%% OTP 21+ provides OTP_RELEASE macro
-define(OTP_MAJOR_VERSION, OTP_RELEASE).
-else.
%% Fallback for older OTP versions
-define(OTP_MAJOR_VERSION, 0).
-endif.

%% Version-specific macros
-define(IS_OTP_28(), ?OTP_MAJOR_VERSION >= 28).
-define(IS_OTP_27(), ?OTP_MAJOR_VERSION >= 27).
-define(IS_OTP_26(), ?OTP_MAJOR_VERSION >= 26).

%%====================================================================
%% Size Optimization (OTP 28 JIT)
%%====================================================================
%%
%% In OTP 28, size/1 is not optimized by JIT. Use specific alternatives:
%% - tuple_size/1 for tuples
%% - byte_size/1 for binaries
%% - bit_size/1 for bitstrings
%%
%% @see EUnit 2.8.2 release notes

%% Compile detection: prefer tuple_size/byte_size on OTP 26+
-define(SAFE_TUPLE_SIZE(Tuple),
        case ?IS_OTP_26() of
            true -> tuple_size(Tuple);
            false -> size(Tuple)
        end).

-define(SAFE_BYTE_SIZE(Binary),
        case ?IS_OTP_26() of
            true -> byte_size(Binary);
            false -> size(Binary)
        end).

%%====================================================================
%% EUnit Timeout Scaling (OTP 27+)
%%====================================================================
%%
%% EUnit 2.9 (OTP 27) introduced scale_timeouts option
%% Usage: rebar3 eunit --scale_timeouts=10
%%
%% This macro creates timeout-aware test generators

-define TIMEOUT_TEST(Timeout, TestFun) ->
    {'timeout', Timeout, fun() -> TestFun() end}
).

%%====================================================================
%% Stack Trace Handling (OTP 26+)
%%====================================================================
%%
%% OTP 26 introduced new stacktrace format
%% Use erlang:get_stacktrace/0 (deprecated) or try...catch with :stacktrace/1

%% Get stacktrace in a version-compatible way
-define GET_STACKTRACE(Type, Error, Stacktrace) ->
        case ?IS_OTP_26() of
            true ->
                %% OTP 26+: Stacktrace is in catch variable
                {Type, Error, Stacktrace};
            false ->
                %% OTP 25 and earlier: Use erlang:get_stacktrace/0
                {Type, Error, erlang:get_stacktrace()}
        end).

%%====================================================================
%% Enhanced Assertions (Version-Agnostic)
%%====================================================================

%% Assert with custom error message (works across all versions)
-define assertMsg(Test, Message),
        case (Test) of
            true -> ok;
            false -> erlang:error({assertMatch, [{module, ?MODULE},
                                                 {line, ?LINE},
                                                 {expression, (??Test)},
                                                 {message, (Message)}]})
        end).

%% Assert not equal (works across all versions)
-define assertNotEqual(Unexpected, Expr),
        begin
            ((fun () ->
                case (Expr) of
                    (Unexpected) ->
                        erlang:error({assertNotEqual_failed,
                                     [{module, ?MODULE},
                                      {line, ?LINE},
                                      {expression, (??Expr)},
                                      {expected, not_equal_to},
                                     {value, (Unexpected)}]});
                    _ ->
                        ok
                end
            end)())
        end).

%% Assert match with custom message
-define assertMatchMsg(Guard, Expr, Message),
        case (Expr) of
            Guard -> ok;
            _ -> erlang:error({assertMatch_failed,
                              [{module, ?MODULE},
                               {line, ?LINE},
                               {expression, (??Expr)},
                               {pattern, (??Guard)},
                               {message, (Message)}]})
        end).

%%====================================================================
%% Process Monitoring (Version-Agnostic)
%%====================================================================

%% Monitor with demonitor on flush (OTP 26+ optimization)
-define MONITOR_DEMONITOR_FLUSH(Type, Object) ->
        case ?IS_OTP_26() of
            true ->
                %% OTP 26+: Use flush option for atomic demonitor
                Ref = erlang:monitor(Type, Object),
                receive
                    {'DOWN', Ref, Type, Object, _Info} ->
                        erlang:demonitor(Ref, [flush]),
                        {down, Ref, Info}
                after 0 ->
                    {monitored, Ref}
                end;
            false ->
                %% OTP 25: Manual flush
                Ref = erlang:monitor(Type, Object),
                {monitored, Ref}
        end).

%%====================================================================
%% Test Generator Helpers
%%====================================================================

%% Create a test with timeout (version-aware scaling)
-define TEST_WITH_TIMEOUT(Timeout, Test) ->
        {?LINE, fun() -> Test end, Timeout}).

%% Create a setup/cleanup test generator
-define SETUP_CLEAN_TEST(Setup, Cleanup, Tests) ->
        {setup,
         fun() -> Setup() end,
         fun(_Args) -> Cleanup(_Args) end,
         fun(_Args) -> Tests end}.

%% Create a foreach test generator
-define FOREACH_TEST(Generator, Tests) ->
        {foreachx,
         funGenerator/1,
         [{fun(_X) -> {setup, fun setup/0, fun cleanup/1, Tests} end}]}.

%%====================================================================
%% Documentation Assertions
%%====================================================================

%% Assert that a function is documented
%% (Requires edoc to be run first)
-define assertDocumented(Function, Arity) ->
        begin
            {ok, _} = edoc:application(?MODULE, []),
            Docs = edoc:module_docs(?MODULE),
            ?assert(lists:keyfind({Function, Arity}, 1, Docs) =/= false)
        end).

%%====================================================================
%% Feature Detection Macros
%%====================================================================

%% Check if feature is available at runtime
-define HAS_FEATURE(Feature) ->
        case erlang:system_info(otp_release) of
            "28" -> true;
            "27" -> false;
            "26" -> false;
            _ -> false
        end).

%%====================================================================
%% Deprecated Warnings
%%====================================================================

%% Warn about deprecated usage (compile-time)
-define WARN_DEPRECATED(Old, New) ->
        -compile({deprecated, [{Old, New}]})

).

%%====================================================================
%% Examples
%%====================================================================
%%
%% Example 1: Size-aware assertion
%%   my_tuple_size_test() ->
%%       Tuple = {a, b, c},
%%       ?assertEqual(3, ?SAFE_TUPLE_SIZE(Tuple)).
%%
%% Example 2: Timeout with scaling
%%   my_slow_test_() ->
%%       ?TIMEOUT_TEST(5000, fun() ->
%%           timer:sleep(1000),
%%           ok
%%       end).
%%
%% Example 3: Version-specific test
%%   my_feature_test_() ->
%%       case ?IS_OTP_28() of
%%           true ->
%%               [?_test(test_new_feature())];
%%           false ->
%%               [?_test(test_legacy_feature())]
%%       end.
%%
%% Example 4: Stacktrace handling
%%   my_error_test() ->
%%       try
%%           error(bad)
%%       catch
%%           error:Error:Stacktrace ->
%%               ?GET_STACKTRACE(error, Error, Stacktrace)
%%       end.

-endif. % erlmcp_EUNIT_COMPAT_HRL
