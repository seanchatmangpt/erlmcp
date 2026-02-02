%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Compatibility Example for erlmcp Modules
%%%
%%% This example demonstrates how to implement OTP version compatibility
%%% in erlmcp modules using the otp_compat.hrl layer.
%%%
%%% Features demonstrated:
%%% - JSON encoding/decoding with automatic module selection
%%% - Process enumeration with memory-efficient iterator
%%% - Priority message handling with graceful fallback
%%% - PCRE2 regex compilation with validation
%%% - Version-specific performance optimizations
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp_example).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_registry_count/0,
         send_critical_alert/1,
         validate_input/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("otp_compat.hrl").  % OTP compatibility layer
-include("erlmcp.hrl").     % erlmcp types

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the example server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get registry process count with version-specific optimization
-spec get_registry_count() -> non_neg_integer().
get_registry_count() ->
    gen_server:call(?MODULE, get_registry_count).

%% @doc Send critical alert with priority support
-spec send_critical_alert(binary()) -> ok.
send_critical_alert(AlertMessage) ->
    gen_server:cast(?MODULE, {critical_alert, AlertMessage}).

%% @doc Validate input with regex (PCRE2 compatible)
-spec validate_input(binary()) -> boolean().
validate_input(Input) ->
    gen_server:call(?MODULE, {validate_input, Input}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    % Enable priority message support if available (OTP 28+)
    case ?HAVE_PRIORITY_MESSAGES of
        true ->
            PrioAlias = alias([priority]),
            {ok, #{priority_alias => PrioAlias, state => initializing}};
        false ->
            {ok, #{state => initializing}}
    end.

handle_call(get_registry_count, _From, State) ->
    % Use version-specific process enumeration
    Count = get_version_specific_process_count(),
    {reply, Count, State};

handle_call({validate_input, Input}, _From, State) ->
    % Use PCRE2-compatible regex validation
    Result = validate_with_pcre2(Input),
    {reply, Result, State}.

handle_cast({critical_alert, AlertMessage}, State) ->
    % Send critical alert with priority if available
    send_priority_alert(AlertMessage),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get process count using version-specific optimized method
get_version_specific_process_count() ->
    case ?HAVE_PROCESS_ITERATOR of
        true ->
            % OTP 28+ - O(1) memory, streaming enumeration
            ?SAFE_PROCESS_COUNT();
        false ->
            % OTP 26-27 - Fallback to system info
            % Note: processes() would allocate memory for large systems
            erlang:system_info(process_count)
    end.

%% @doc Send critical alert with priority support
send_priority_alert(AlertMessage) ->
    Alert = {critical_alert, AlertMessage, erlang:timestamp()},

    case ?HAVE_PRIORITY_MESSAGES of
        true ->
            % OTP 28+ - Priority message support
            ?SEND_PRIORITY(alert_handler, Alert);
        false ->
            % OTP 26-27 - Fallback to normal priority
            erlang:send(alert_handler, Alert, [nosuspend])
    end.

%% @doc Validate input using PCRE2-compatible regex
validate_with_pcre2(Input) ->
    % Example: Validate input as alphanumeric with basic regex
    % This pattern is PCRE2-compatible (OTP 28) and works with PCRE (OTP 27)
    Pattern = <<"[a-zA-Z0-9\\-_]+">>,

    % Use version-safe regex compilation
    case compile_pcre2_pattern(Pattern) of
        {ok, CompiledPattern} ->
            case re:run(Input, CompiledPattern, [{capture, none}]) of
                nomatch -> false;
                {match, _} -> true
            end;
        {error, _} ->
            false  % Pattern compilation failed
    end.

%% @doc Compile regex pattern with PCRE2 validation
compile_pcre2_pattern(Pattern) ->
    % OTP 28+ includes PCRE2 with stricter validation
    case re:compile(Pattern, [unicode]) of
        {ok, Compiled} ->
            % Additional validation for PCRE2 breaking changes
            case validate_pcre2_compatibility(Pattern) of
                true -> {ok, Compiled};
                false -> {error, "PCRE2 incompatible pattern"}
            end;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Validate pattern against PCRE2 breaking changes
validate_pcre2_compatibility(Pattern) ->
    % PCRE2 has stricter syntax than PCRE
    % Common breaking patterns to check:
    - Invalid: "\\i", "\\B", "\\8", "(?|...)"
    - Valid: "\\w", "\\d", "\\b", "(?:...)", "(...)", "[a-z]"

    % Check for known PCRE-specific patterns that break in PCRE2
    case re:run(Pattern, "\\\\[iB8]", [{capture, none}]) of
        nomatch -> true;  % No problematic patterns found
        _ -> false        % Pattern contains PCRE-specific syntax
    end.

%%====================================================================
%% Version-Specific Optimizations
%%====================================================================

%% @doc Example of OTP 28+ specific optimizations
-ifndef(OTP_28_PLUS).
% OTP 26-27 implementations

optimize_for_large_scale() ->
    % Legacy optimization for OTP <28
    case erlang:system_info(process_count) of
        N when N > 10000 ->
            logger:warning("Large system detected (~p processes)", [N]),
            use_memory_conservative_approach();
        _ ->
            use_standard_approach()
    end.
-else.
% OTP 28+ implementations

optimize_for_large_scale() ->
    % New optimization for OTP 28+
    case ?SAFE_PROCESS_COUNT() of
        N when N > 10000 ->
            % Use process iterator for memory efficiency
            Iterator = erlang:processes_iterator(),
            stream_process_data(Iterator);
        _ ->
            use_standard_approach()
    end.
-endif.

%% @doc Example of strict generator usage (OTP 28+)
-ifndef(OTP_28_PLUS).
% OTP 26-27 - Use relaxed generators
process_valid_modules(Modules) ->
    % Relaxed generator - silently ignores invalid modules
    [ModName || {module, ModName} <- Modules].
-else.
% OTP 28+ - Use strict generators for better error handling
process_valid_modules(Modules) ->
    % Strict generator - fails fast on invalid data
    [ModName || {module, ModName} <:- Modules].
-endif.

%%====================================================================
%% Compatibility Helpers
%%====================================================================

%% @doc Example of version-specific data handling
handle_json_data(Data) ->
    % Auto-select JSON encoding method based on OTP version
    case ?HAVE_NATIVE_JSON of
        true ->
            % OTP 27+ - Native JSON module
            JSON = json:encode(Data),
            handle_native_json(JSON);
        false ->
            % OTP 26 - Legacy JSX fallback
            JSON = jsx:encode(Data),
            handle_jsx_json(JSON)
    end.

%% @doc Handle native JSON (OTP 27+)
handle_native_json(JSON) ->
    % Native JSON may have better performance
    logger:debug("Using native JSON encoding"),
    process_json(JSON).

%% @doc Handle legacy JSON (OTP 26)
handle_jsx_json(JSON) ->
    % Legacy JSX encoding
    logger:debug("Using legacy JSX encoding"),
    process_json(JSON).

%% @doc Common JSON processing
process_json(JSON) ->
    % Process JSON data (same for both versions)
    try
        Decoded = ?JSON_DECODE(JSON),
        success(Decoded)
    catch
        error:Reason ->
            error({json_decode_failed, Reason})
    end.

%%====================================================================
%% Performance Monitoring
%%====================================================================

%% @doc Monitor version-specific performance
monitor_performance() ->
    Metrics = #{
        otp_version => ?OTP_VERSION(),
        process_count => get_version_specific_process_count(),
        json_encoding => measure_json_performance(),
        regex_performance => measure_regex_performance()
    },
    send_metrics(Metrics).

%% @doc Measure JSON encoding performance
measure_json_performance() ->
    TestData = #{test => data, count => 1000},

    case ?HAVE_NATIVE_JSON of
        true ->
            % Test native JSON performance
            {Time, _} = timer:tc(fun() -> json:encode(TestData) end),
            {native, Time};
        false ->
            % Test JSX performance
            {Time, _} = timer:tc(fun() -> jsx:encode(TestData) end),
            {jsx, Time}
    end.

%% @doc Measure regex compilation performance
measure_regex_performance() ->
    Pattern = <<"[a-zA-Z0-9\\-_]+">>,

    {Time, Result} = timer:tc(fun() ->
        re:compile(Pattern, [unicode])
    end),

    {regex_compile, Time, Result}.

%%====================================================================
%% Error Handling Examples
%%====================================================================

%% @doc Example of version-aware error handling
handle_version_specific_error(Error) ->
    case Error of
        {regex_compile, Pattern, Reason} when ?IS_OTP_28_PLUS() ->
            % PCRE2-specific error handling
            handle_pcre2_error(Pattern, Reason);
        {priority_send, _} when ?IS_OTP_28_PLUS() ->
            % Priority message error (OTP 28)
            handle_priority_error();
        {json_encode, _} ->
            % JSON encoding error
            handle_json_error();
        _ ->
            handle_generic_error(Error)
    end.

%% @doc Handle PCRE2 specific errors
handle_pcre2_error(Pattern, Reason) ->
    logger:error("PCRE2 pattern error: ~p, Reason: ~p", [Pattern, Reason]),

    % Try to suggest fix for common PCRE2 issues
    case Reason of
        {bad_regex, unrecognized_char} ->
            suggest_pcre2_fix(Pattern);
        _ ->
            no_fix_available
    end.

%% @doc Suggest PCRE2 pattern fixes
suggest_pcre2_fix(Pattern) ->
    % Common PCRE2 issues and fixes:
    % "\\i" -> "\\\\w" (word character)
    % "\\B" -> "\\\\b" (word boundary)
    % "\\8" -> "\\" (escape sequence issue)

    case binary:match(Pattern, <<\\i>>) of
        nomatch -> nomatch;
        _ -> logger:info("Consider replacing \\i with \\\\w")
    end.

%%====================================================================
%% Test Functions
%%====================================================================

%% @doc Test all version-specific functionality
run_compatibility_tests() ->
    Tests = [
        test_json_encoding(),
        test_process_enumeration(),
        test_priority_messages(),
        test_regex_compilation()
    ],

    Results = lists:map(fun run_test/1, Tests),
    report_test_results(Results).

%% @doc Test JSON encoding compatibility
test_json_encoding() ->
    TestData = #{test => value, number => 42},

    % Test both encoding methods
    Encoded1 = ?JSON_ENCODE(TestData),
    Decoded1 = ?JSON_DECODE(Encoded1),

    % Verify round-trip compatibility
    case Decoded1 of
        TestData -> json_encoding_ok;
        _ -> json_encoding_failed
    end.

%% @doc Test process enumeration efficiency
test_process_enumeration() ->
    % Measure memory usage of different methods
    Count1 = erlang:system_info(process_count),
    Count2 = ?SAFE_PROCESS_COUNT(),

    % Results should be the same, but method differs
    {method_comparison, Count1, Count2}.

%% @doc Test priority message functionality
test_priority_messages() ->
    case ?HAVE_PRIORITY_MESSAGES of
        true ->
            % Test priority message sending
            Self = self(),
            PrioAlias = alias([priority]),
            erlang:send(PrioAlias, test_msg, [priority]),

            receive
                test_msg -> priority_test_ok
            after 1000 ->
                priority_test_timeout
            end;
        false ->
            priority_not_supported
    end.

%% @doc Test regex compilation compatibility
test_regex_compilation() ->
    Patterns = [
        % Valid PCRE2 patterns
        <<"[a-zA-Z0-9\\-_]+">>,     % Character class
        <<"(start|end)">>,          % Alternation
        <<"(\\\\d+)-(\\\\d+)">>,   % Capturing groups

        % Patterns that work with both
        <<"\\\\w+">>,              % Word characters
        <<"\\\\d+">>,              % Digits
        <<"[\\x00-\\x1F]">>       % Character range
    ],

    Results = lists:map(fun compile_test_pattern/1, Patterns),
    {pattern_results, Results}.

%% @doc Test individual pattern compilation
compile_test_pattern(Pattern) ->
    case re:compile(Pattern, [unicode]) of
        {ok, _} -> {Pattern, success};
        {error, _} -> {Pattern, failed}
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

run_test(TestFun) ->
    try
        Result = TestFun(),
        {success, Result}
    catch
        Error:Reason ->
            {failed, {Error, Reason, TestFun}}
    end.

report_test_results(Results) ->
    Passed = [R || {success, _} <- Results],
    Failed = [R || {failed, _} <- Results],

    logger:info("Compatibility Tests: ~p passed, ~p failed",
                [length(Passed), length(Failed)]),

    case Failed of
        [] -> all_tests_passed;
        _ -> tests_failed(Failed)
    end.

tests_failed(FailedTests) ->
    lists:foreach(fun({failed, {Error, Reason, TestFun}}) ->
                     logger:error("Test ~p failed: ~p:~p",
                                 [TestFun, Error, Reason])
                  end,
                  FailedTests),
    tests_failed.