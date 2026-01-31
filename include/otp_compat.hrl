%% @doc OTP Version Compatibility Header
%%
%% Provides feature flags and macros for conditional compilation based on OTP version.
%% This enables erlmcp to leverage OTP 28+ features while maintaining compatibility with OTP 25-27.
%%
%% Usage:
%%   -include("otp_compat.hrl").
%%
%%   ?IF_OTP_28(
%%       %% OTP 28+ code
%%       process_flag(priority, high),
%%       %% Fallback for older OTP
%%       ok
%%   )
%%
%% Supported OTP Versions: 25, 26, 27, 28
%% @end

-ifndef(OTP_COMPAT_HRL).
-define(OTP_COMPAT_HRL, true).

%%==============================================================================
%% OTP Version Detection
%%==============================================================================

%% OTP 28+ detection
%% OTP 28 introduces: priority messages, process iteration improvements, JSON BIFs
-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE >= 28).
        -define(OTP_28, true).
        -define(HAS_PRIORITY_MESSAGES, true).
        -define(HAS_PROCESS_ITERATION_V2, true).
        -define(HAS_JSON_BIF, true).
    -endif.
-endif.

%% OTP 27+ detection
%% OTP 27 introduces: json module, improved process monitoring
-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE >= 27).
        -define(OTP_27, true).
        -define(HAS_JSON_MODULE, true).
        -define(HAS_IMPROVED_MONITORING, true).
    -endif.
-endif.

%% OTP 26+ detection
%% OTP 26 introduces: process_info improvements, socket improvements
-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE >= 26).
        -define(OTP_26, true).
        -define(HAS_PROCESS_INFO_V2, true).
    -endif.
-endif.

%% OTP 25+ detection (minimum supported version)
-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE >= 25).
        -define(OTP_25, true).
    -endif.
-endif.

%% Fallback for pre-OTP-17 (should not happen in erlmcp)
-ifndef(OTP_RELEASE).
    -define(OTP_LEGACY, true).
    -error("erlmcp requires OTP 25 or later").
-endif.

%%==============================================================================
%% Feature Flag Macros
%%==============================================================================

%% Priority Messages (OTP 28+)
%% Allows sending messages with priority for critical protocol messages
-ifdef(HAS_PRIORITY_MESSAGES).
    %% Send a high-priority message
    -define(SEND_PRIORITY(Pid, Msg),
            erlang:send(Pid, Msg, [priority])).

    %% Send a normal-priority message (explicit)
    -define(SEND_NORMAL(Pid, Msg),
            erlang:send(Pid, Msg, [])).
-else.
    %% Fallback: normal send for older OTP versions
    -define(SEND_PRIORITY(Pid, Msg), Pid ! Msg).
    -define(SEND_NORMAL(Pid, Msg), Pid ! Msg).
-endif.

%% Process Iteration V2 (OTP 28+)
%% Improved processes/0 with better performance and safety
-ifdef(HAS_PROCESS_ITERATION_V2).
    %% Get all processes (optimized in OTP 28)
    -define(ALL_PROCESSES(), erlang:processes()).

    %% Safe process info retrieval
    -define(SAFE_PROCESS_INFO(Pid, Item),
            erlang:process_info(Pid, Item)).
-else.
    %% Fallback: standard implementation
    -define(ALL_PROCESSES(), erlang:processes()).
    -define(SAFE_PROCESS_INFO(Pid, Item),
            erlang:process_info(Pid, Item)).
-endif.

%% JSON BIF Support (OTP 28+)
%% Native JSON encoding/decoding for better performance
-ifdef(HAS_JSON_BIF).
    %% Use native json module (OTP 28+)
    -define(JSON_ENCODE(Term), json:encode(Term)).
    -define(JSON_DECODE(Binary), json:decode(Binary)).
    -define(JSON_AVAILABLE, true).
-else.
    -ifdef(HAS_JSON_MODULE).
        %% Use json module (OTP 27)
        -define(JSON_ENCODE(Term), json:encode(Term)).
        -define(JSON_DECODE(Binary), json:decode(Binary)).
        -define(JSON_AVAILABLE, true).
    -else.
        %% Fallback: use jsx (all versions)
        -define(JSON_ENCODE(Term), jsx:encode(Term)).
        -define(JSON_DECODE(Binary), jsx:decode(Binary)).
        -define(JSON_AVAILABLE, false).
    -endif.
-endif.

%% Process Flag Priority (OTP 28+)
%% Set process priority for critical components
-ifdef(HAS_PRIORITY_MESSAGES).
    -define(SET_PROCESS_PRIORITY(Level),
            process_flag(priority, Level)).
-else.
    %% Fallback: no-op for older OTP
    -define(SET_PROCESS_PRIORITY(_Level), ok).
-endif.

%% Improved Monitoring (OTP 27+)
%% Enhanced monitor options and capabilities
-ifdef(HAS_IMPROVED_MONITORING).
    -define(MONITOR_NODE(Node, Opts),
            erlang:monitor(process, Node, Opts)).
-else.
    -define(MONITOR_NODE(Node, _Opts),
            erlang:monitor(process, Node)).
-endif.

%%==============================================================================
%% Conditional Compilation Helpers
%%==============================================================================

%% Execute code only on OTP 28+
-ifdef(OTP_28).
    -define(IF_OTP_28(TrueExpr, _FalseExpr), TrueExpr).
-else.
    -define(IF_OTP_28(_TrueExpr, FalseExpr), FalseExpr).
-endif.

%% Execute code only on OTP 27+
-ifdef(OTP_27).
    -define(IF_OTP_27(TrueExpr, _FalseExpr), TrueExpr).
-else.
    -define(IF_OTP_27(_TrueExpr, FalseExpr), FalseExpr).
-endif.

%% Execute code only on OTP 26+
-ifdef(OTP_26).
    -define(IF_OTP_26(TrueExpr, _FalseExpr), TrueExpr).
-else.
    -define(IF_OTP_26(_TrueExpr, FalseExpr), FalseExpr).
-endif.

%%==============================================================================
%% Performance Optimization Macros
%%==============================================================================

%% Hibernate optimization (all OTP versions)
%% Use for gen_server processes with long idle periods
-define(HIBERNATE(), hibernate).

%% Message queue optimization
-ifdef(OTP_27).
    %% OTP 27+ has improved message queue handling
    -define(OPTIMIZE_MESSAGE_QUEUE(State),
            garbage_collect(),
            State).
-else.
    -define(OPTIMIZE_MESSAGE_QUEUE(State), State).
-endif.

%%==============================================================================
%% Type Compatibility
%%==============================================================================

%% Map types (available in all supported OTP versions)
-type otp_compat_map() :: map().

%% Process priority levels
-ifdef(HAS_PRIORITY_MESSAGES).
    -type process_priority() :: low | normal | high | max.
-else.
    -type process_priority() :: low | normal | high | max.
    %% Note: Type exists but process_flag(priority, _) may not work
-endif.

%%==============================================================================
%% Compatibility Assertions
%%==============================================================================

%% Compile-time assertion that we're on a supported OTP version
-ifndef(OTP_25).
    -ifndef(OTP_LEGACY).
        -error("erlmcp requires Erlang/OTP 25 or later. Current version is unsupported.").
    -endif.
-endif.

%%==============================================================================
%% Feature Detection Functions
%%==============================================================================

%% These are meant to be used at runtime for dynamic feature detection

%% @doc Check if priority messages are available
-ifdef(HAS_PRIORITY_MESSAGES).
    -define(HAS_PRIORITY_MESSAGES_RT(), true).
-else.
    -define(HAS_PRIORITY_MESSAGES_RT(), false).
-endif.

%% @doc Check if native JSON is available
-ifdef(JSON_AVAILABLE).
    -define(HAS_NATIVE_JSON_RT(), true).
-else.
    -define(HAS_NATIVE_JSON_RT(), false).
-endif.

%% @doc Get current OTP release version
-ifdef(OTP_RELEASE).
    -define(OTP_VERSION(), ?OTP_RELEASE).
-else.
    -define(OTP_VERSION(), unknown).
-endif.

%%==============================================================================
%% Transport Compatibility
%%==============================================================================

%% Socket options compatibility
-ifdef(OTP_26).
    -define(SOCKET_OPTS_COMPAT(Opts), Opts).
-else.
    %% May need to filter out OTP 26+ specific options
    -define(SOCKET_OPTS_COMPAT(Opts),
            lists:filter(fun({K, _V}) ->
                K =/= exclusive andalso K =/= reuseport
            end, Opts)).
-endif.

%%==============================================================================
%% Documentation Macros
%%==============================================================================

%% @doc Mark a function as OTP 28+ only
-define(REQUIRES_OTP_28,
        "This function requires Erlang/OTP 28 or later").

%% @doc Mark a function as OTP 27+ only
-define(REQUIRES_OTP_27,
        "This function requires Erlang/OTP 27 or later").

%%==============================================================================
%% End of Header
%%==============================================================================

-endif. %% OTP_COMPAT_HRL
