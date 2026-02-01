%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Compatibility Layer for erlmcp v3
%%%
%%% Purpose: Compile-time and runtime OTP version detection
%%%          with safe fallbacks for missing features.
%%%
%%% Supported Versions:
%%%   - OTP 28.3.1+ (primary target, full feature support)
%%%   - OTP 27 (fallback, degraded feature set)
%%%
%%% Features Detected:
%%%   - Native JSON module (json:encode/decode)
%%%   - Process iterators (erlang:processes_iterator/0)
%%%   - Priority messages (process_flag(priority, high))
%%%
%%% Usage:
%%%   -include("otp_compat.hrl").
%%%
%%%   %% Use version-specific code
%%%   -ifdef(OTP_28_PLUS).
%%%       % OTP 28+ optimized code
%%%   -else.
%%%       % Fallback for OTP 27
%%%   -endif.
%%%
%%%   %% Use safe macros
%%%   Count = ?SAFE_PROCESS_COUNT(),
%%%   Encoded = ?JSON_ENCODE(Data),
%%%
%%% @end
%%%-------------------------------------------------------------------

%%====================================================================
%% Version Macros (defined by rebar3)
%%====================================================================
%% These macros are defined via platform_define in rebar.config:
%%
%% {platform_define, "^2[8-9]|^[3-9]", 'OTP_28_PLUS'}
%%
%% DO NOT define these manually - they are set by the build system.
%%
%% Available macros:
%%   - OTP_28_PLUS: Defined for OTP 28.3.1+
%%
%% Usage:
%%   -ifdef(OTP_28_PLUS).
%%       % OTP 28+ code here
%%   -else.
%%       % OTP 27 fallback here
%%   -endif.

%%====================================================================
%% Feature Detection (runtime-safe)
%%====================================================================

%%-----------------------------------------------------------------------------
%% Native JSON Module (OTP 27+)
%%-----------------------------------------------------------------------------
%% Check if native json module is available.
%% Returns: true | false
%%
%% Usage:
%%   case ?HAVE_NATIVE_JSON of
%%       true -> use_native_json();
%%       false -> use_jsx()
%%   end.
%%-----------------------------------------------------------------------------
-define(HAVE_NATIVE_JSON,
    erlang:function_exported(json, encode, 1)).

%%-----------------------------------------------------------------------------
%% Process Iterator (OTP 28+)
%%-----------------------------------------------------------------------------
%% Check if process iterator API is available.
%% Returns: true | false
%%
%% OTP 28.3.1+ provides O(1) memory process enumeration.
%%-----------------------------------------------------------------------------
-define(HAVE_PROCESS_ITERATOR,
    erlang:function_exported(erlang, processes_iterator, 0)).

%%-----------------------------------------------------------------------------
%% Process Next Function (OTP 28+)
%%-----------------------------------------------------------------------------
%% Check if process_next/1 is available.
%% Returns: true | false
%%
%% Used with processes_iterator/0 for iteration.
%%-----------------------------------------------------------------------------
-define(HAVE_PROCESS_NEXT,
    erlang:function_exported(erlang, process_next, 1)).

%%-----------------------------------------------------------------------------
%% Priority Messages (EEP 76, OTP 28+)
%%-----------------------------------------------------------------------------
%% Check if priority message support is available.
%% Returns: true | false
%%
%% OTP 28+ supports process_flag(priority, high) for priority message delivery.
%%-----------------------------------------------------------------------------
-define(HAVE_PRIORITY_MESSAGES,
    case erlang:system_info(otp_release) of
        Vsn when is_list(Vsn) ->
            [Major|_] = string:split(Vsn, "."),
            (list_to_integer(Major) >= 28);
        _ ->
            false
    end).

%%====================================================================
%% JSON Encoding/Decoding (compile-time selection)
%%====================================================================

%%-----------------------------------------------------------------------------
%% JSON Encode - Compile-time Module Selection
%%-----------------------------------------------------------------------------
%% Usage: ?JSON_ENCODE(Data)
%%
%% OTP 28+: Uses native json:encode/1
%% OTP 27:  Uses jsx:encode/1
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(JSON_ENCODE(Data), json:encode(Data)).
-define(JSON_DECODE(Binary), json:decode(Binary)).
-else.
-define(JSON_ENCODE(Data), jsx:encode(Data)).
-define(JSON_DECODE(Binary), jsx:decode(Binary, [return_maps])).
-endif.

%%-----------------------------------------------------------------------------
%% JSON Encode/Decode - Runtime Fallback
%%-----------------------------------------------------------------------------
%% Usage: ?JSON_ENCODE_SAFE(Data)
%%
%% Uses native json if available at runtime, otherwise falls back to jsx.
%% Useful for modules that need to support both OTP 27 and 28 at runtime.
%%-----------------------------------------------------------------------------
-define(JSON_ENCODE_SAFE(Data),
    case ?HAVE_NATIVE_JSON of
        true -> json:encode(Data);
        false -> jsx:encode(Data)
    end).

-define(JSON_DECODE_SAFE(Binary),
    case ?HAVE_NATIVE_JSON of
        true -> json:decode(Binary);
        false -> jsx:decode(Binary, [return_maps])
    end).

%%====================================================================
%% Process Enumeration (safe, memory-efficient)
%%====================================================================

%%-----------------------------------------------------------------------------
%% Safe Process Count
%%-----------------------------------------------------------------------------
%% Usage: Count = ?SAFE_PROCESS_COUNT()
%%
%% Returns the number of processes in the system.
%%
%% OTP 28+: Uses process iterator (O(1) memory, O(N) time)
%% OTP 27:  Uses system_info(process_count) (O(1) memory, O(1) time)
%%
%% This is safe to use in production code.
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(SAFE_PROCESS_COUNT(),
    begin
        Iterator = erlang:processes_iterator(),
        count_processes_iterator(Iterator, 0)
    end).
-else.
-define(SAFE_PROCESS_COUNT(),
    erlang:system_info(process_count)).
-endif.

%%-----------------------------------------------------------------------------
%% Safe Process List
%%-----------------------------------------------------------------------------
%% Usage: PidList = ?SAFE_PROCESSES()
%%
%% Returns a list of all PIDs in the system.
%%
%% WARNING: This may allocate large amounts of memory for many processes.
%%
%% OTP 28+: Builds list from iterator with warning for >10K processes
%% OTP 27:  Uses erlang:processes() with warning for >10K processes
%%
%% Prefer ?SAFE_PROCESS_COUNT() if you only need the count.
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(SAFE_PROCESSES(),
    begin
        Iterator = erlang:processes_iterator(),
        PidList = processes_iterator_to_list(Iterator, []),
        case length(PidList) of
            N when N > 10000 ->
                logger:warning("Process enumeration of ~p processes may be inefficient", [N]),
                PidList;
            _ ->
                PidList
        end
    end).
-else.
-define(SAFE_PROCESSES(),
    begin
        Count = erlang:system_info(process_count),
        case Count > 10000 of
            true ->
                logger:warning("Process enumeration of ~p processes inefficient on OTP <28. "
                               "Consider upgrading to OTP 28.3.1+ for O(1) memory.",
                              [Count]),
                erlang:processes();
            false ->
                erlang:processes()
        end
    end).
-endif.

%%====================================================================
%% Priority Messages (compile-time)
%%====================================================================

%%-----------------------------------------------------------------------------
%% Set Process Priority High
%%-----------------------------------------------------------------------------
%% Usage: ?SET_PRIORITY_HIGH()
%%
%% OTP 28+: Sets process_flag(priority, high)
%% OTP 27:  No-op (graceful degradation)
%%
%% Critical messages on OTP 28 will preempt normal traffic.
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(SET_PRIORITY_HIGH(),
    process_flag(priority, high)).
-else.
-define(SET_PRIORITY_HIGH(),
    ok).
-endif.

%%-----------------------------------------------------------------------------
%% Send Priority Message
%%-----------------------------------------------------------------------------
%% Usage: ?SEND_PRIORITY(Pid, Message)
%%
%% OTP 28+: Sends with priority flag
%% OTP 27:  Sends without priority (normal ordering)
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(SEND_PRIORITY(Pid, Msg),
    erlang:send(Pid, Msg, [nosuspend, {priority, high}])).
-else.
-define(SEND_PRIORITY(Pid, Msg),
    erlang:send(Pid, Msg, [nosuspend])).
-endif.

%%-----------------------------------------------------------------------------
%% Send Alias Message (OTP 25+)
%%-----------------------------------------------------------------------------
%% Usage: ?SEND_ALIAS(Alias, Message)
%%
%% OTP 25+: Uses alias for process reference
%% OTP 24:  Uses PID directly
%%-----------------------------------------------------------------------------
-ifdef(OTP_25_PLUS).
-define(SEND_ALIAS(Alias, Msg),
    erlang:send(Alias, Msg, [nosuspend])).
-else.
-define(SEND_ALIAS(Alias, Msg),
    case Alias of
        Pid when is_pid(Pid) -> erlang:send(Pid, Msg, [nosuspend]);
        _ -> erlang:send(Alias, Msg, [nosuspend])
    end).
-endif.

%%====================================================================
%% Version Information (runtime)
%%====================================================================

%%-----------------------------------------------------------------------------
%% Get OTP Version String
%%-----------------------------------------------------------------------------
%% Usage: OtpVersion = ?OTP_VERSION()
%%
%% Returns: <<"27">>, <<"28">>, etc.
%%-----------------------------------------------------------------------------
-define(OTP_VERSION(),
    list_to_binary(erlang:system_info(otp_release))).

%%-----------------------------------------------------------------------------
%% Check if OTP 28+
%%-----------------------------------------------------------------------------
%% Usage: case ?IS_OTP_28_PLUS() of true -> ... end
%%
%% Runtime version check.
%%-----------------------------------------------------------------------------
-define(IS_OTP_28_PLUS(),
    ?HAVE_PRIORITY_MESSAGES).

%%-----------------------------------------------------------------------------
%% Check if OTP 27+
%%-----------------------------------------------------------------------------
%% Usage: case ?IS_OTP_27_PLUS() of true -> ... end
%%
%% Runtime version check.
%%-----------------------------------------------------------------------------
-define(IS_OTP_27_PLUS(),
    ?HAVE_NATIVE_JSON).

%%====================================================================
%% Conditional Logging
%%====================================================================

%%-----------------------------------------------------------------------------
%% Log Deprecation Warning
%%-----------------------------------------------------------------------------
%% Usage: ?LOG_DEPRECATION(Feature, Alternative)
%%
%% Logs a deprecation warning on OTP 27.
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(LOG_DEPRECATION(Feature, Alternative), ok).
-else.
-define(LOG_DEPRECATION(Feature, Alternative),
    logger:warning("~s is deprecated on OTP <28. Use ~s instead. "
                   "Please upgrade to OTP 28.3.1+ for full feature support.",
                  [Feature, Alternative])).
-endif.

%%-----------------------------------------------------------------------------
%% Log Performance Warning
%%-----------------------------------------------------------------------------
%% Usage: ?LOG_PERF_WARNING(Operation)
%%
%% Logs a performance warning when using suboptimal code path.
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(LOG_PERF_WARNING(Operation), ok).
-else.
-define(LOG_PERF_WARNING(Operation),
    logger:warning("~s using fallback code path on OTP <28. "
                   "Upgrade to OTP 28.3.1+ for optimal performance.",
                  [Operation])).
-endif.

%%====================================================================
%% Helper Function Placeholders
%%====================================================================
%% These functions should be implemented in erlmcp_otp_compat module
%% They are referenced by macros above.

%% Note: The following functions are expected to exist:
%%
%% - count_processes_iterator(Iterator, Acc) -> non_neg_integer()
%%   Counts processes using iterator (OTP 28+)
%%
%% - processes_iterator_to_list(Iterator, Acc) -> [pid()]
%%   Converts iterator to list (OTP 28+)
%%
%% Implementation in erlmcp_otp_compat.erl:
%%   -ifdef(OTP_28_PLUS).
%%   count_processes_iterator(Iterator, Acc) ->
%%       case erlang:process_next(Iterator) of
%%           {_Pid, NewIterator} ->
%%               count_processes_iterator(NewIterator, Acc + 1);
%%           none ->
%%               Acc
%%       end.
%%
%%   processes_iterator_to_list(Iterator, Acc) ->
%%       case erlang:process_next(Iterator) of
%%           {Pid, NewIterator} ->
%%               processes_iterator_to_list(NewIterator, [Pid | Acc]);
%%           none ->
%%               lists:reverse(Acc)
%%       end.
%%   -endif.

%%====================================================================
%% Compilation Guards
%%====================================================================

%% Ensure we're running on supported OTP version
%% This will cause compilation to fail with a clear message on very old OTP
-ifdef(OTP_26_OR_EARLIER).
-error("erlmcp v3 requires OTP 27 or later. Please upgrade Erlang/OTP.").
-endif.

%%====================================================================
%% End of otp_compat.hrl
%%====================================================================
