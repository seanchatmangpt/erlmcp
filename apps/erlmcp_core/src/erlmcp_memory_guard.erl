%%%-------------------------------------------------------------------
%%% @doc
%%% Memory Guard for ErlMCP - OTP 28 Process Memory Limiting
%%%
%%% This module provides comprehensive memory guard functionality for
%%% preventing memory leaks in long-running model contexts using OTP 28's
%%% advanced process flag support.
%%%
%%% == Key Features ==
%%%
%%% 1. **Per-Process Memory Limits**: min/max heap and binary heap sizes
%%% 2. **Context Guard**: Specialized limits for LLM context processes
%%% 3. **Tool Guard**: Memory limits for tool execution processes
%%% 4. **Transport Guard**: Limits for transport layer processes
%%% 5. **Memory Monitoring**: Real-time memory usage tracking
%%% 6. **Auto-Hibernation**: Force hibernation when approaching limits
%%% 7. **OTP Compatibility**: Graceful degradation on OTP < 28
%%%
%%% == OTP 28 Process Flags ==
%%%
%%% <ul><li>
%%%   `min_heap_size`: Minimum heap size (prevents frequent GCs)
%%% </li><li>
%%%   `max_heap_size`: Maximum heap size (triggers GC or kills process)
%%% </li><li>
%%%   `min_bin_vheap_size`: Minimum binary virtual heap size
%%% </li><li>
%%%   `max_bin_vheap_size`: Maximum binary virtual heap size
%%% </li></ul>
%%%
%%% == Usage Example ==
%%%
%%% ```erlang
%%% %% Enable memory guard for a context process
%%% ok = erlmcp_memory_guard:enable_context_guard(),
%%%
%%% %% Check current memory usage
%%% {Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage(),
%%% logger:info("Process memory: heap=~p, binary=~p", [Heap, BinHeap]),
%%%
%%% %% Force hibernation to reduce memory footprint
%%% ok = erlmcp_memory_guard:force_hibernate(),
%%%
%%% %% Validate memory is within configured limits
%%% {ok, Percent} = erlmcp_memory_guard:validate_memory(context),
%%% case Percent of
%%%     P when P > 90 -> erlmcp_memory_guard:force_hibernate();
%%%     _ -> ok
%%% end
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_guard).

%% API
-export([configure_limits/2, enable_context_guard/0, enable_tool_guard/0,
         enable_transport_guard/0, get_memory_usage/0, force_hibernate/0,
         validate_memory/1, get_limits/1, is_otp_28_or_later/0]).

%% Type definitions
-type process_type() :: context | tool | transport | generic.
-type memory_limit() :: #{max_heap => pos_integer(),
                          max_bin_heap => pos_integer(),
                          hibernate_threshold => float()}.
-type memory_usage() :: {Heap :: non_neg_integer(), BinHeap :: non_neg_integer()}.

-export_type([process_type/0, memory_limit/0, memory_usage/0]).

%%====================================================================
%% Default Limits Configuration (in words)
%%====================================================================

-define(DEFAULT_CONTEXT_LIMITS,
        #{max_heap => 100_000_000,           % 100MB heap (~400MB actual with overhead)
          max_bin_heap => 50_000_000,        % 50MB binary heap
          hibernate_threshold => 0.9}).      % Hibernate at 90%

-define(DEFAULT_TOOL_LIMITS,
        #{max_heap => 50_000_000,            % 50MB heap
          max_bin_heap => 25_000_000,        % 25MB binary heap
          hibernate_threshold => 0.85}).     % Hibernate at 85%

-define(DEFAULT_TRANSPORT_LIMITS,
        #{max_heap => 30_000_000,            % 30MB heap
          max_bin_heap => 15_000_000,        % 15MB binary heap
          hibernate_threshold => 0.80}).     % Hibernate at 80%

-define(DEFAULT_GENERIC_LIMITS,
        #{max_heap => 20_000_000,            % 20MB heap
          max_bin_heap => 10_000_000,        % 10MB binary heap
          hibernate_threshold => 0.80}).     % Hibernate at 80%

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Configure memory limits for the current process.
%% Sets min/max heap and binary heap sizes using OTP 28 process flags.
%%
%% @param MaxHeap Maximum heap size in words
%% @param MaxBinHeap Maximum binary virtual heap size in words
-spec configure_limits(pos_integer(), pos_integer()) -> ok.
configure_limits(MaxHeap, MaxBinHeap) when MaxHeap > 0, MaxBinHeap > 0 ->
    %% Set minimum heap sizes to prevent excessive GC cycles
    %% Use 10% of max as minimum to balance memory usage and GC frequency
    MinHeap = max(100, MaxHeap div 10),
    MinBinHeap = max(50, MaxBinHeap div 10),

    try
        %% OTP 28: Set heap size limits
        true = erlang:process_flag(min_heap_size, MinHeap),
        true = erlang:process_flag(max_heap_size, MaxHeap),

        %% OTP 28: Set binary heap size limits
        true = erlang:process_flag(min_bin_vheap_size, MinBinHeap),
        true = erlang:process_flag(max_bin_vheap_size, MaxBinHeap),

        logger:debug("Memory guard configured: max_heap=~p, max_bin_heap=~p",
                     [MaxHeap, MaxBinHeap]),
        ok
    catch
        error:badarg ->
            %% Process flags not supported (OTP < 28)
            logger:warning("Memory guard flags not supported (requires OTP 28+)"),
            ok
    end.

%% @doc Enable memory guard for LLM context processes.
%% Uses conservative limits (100MB heap, 50MB binary heap) to prevent
%% memory leaks in long-running model contexts.
%%
%% Context processes accumulate large message histories and should be
%% aggressively managed to prevent unbounded growth.
%%
%% @return ok
-spec enable_context_guard() -> ok.
enable_context_guard() ->
    Limits = get_limits(context),
    MaxHeap = maps:get(max_heap, Limits),
    MaxBinHeap = maps:get(max_bin_heap, Limits),

    ok = configure_limits(MaxHeap, MaxBinHeap),

    %% Set high priority for context processes (they are critical path)
    try
        erlang:process_flag(priority, high)
    catch
        _:_ -> ok
    end,

    logger:info("Context memory guard enabled: ~p bytes", [MaxHeap * erlang:system_info(wordsize)]),
    ok.

%% @doc Enable memory guard for tool execution processes.
%% Uses moderate limits (50MB heap, 25MB binary heap) for tool processes.
%%
%% Tools are typically short-lived but may process large payloads
%% (file reads, data analysis, etc.).
%%
%% @return ok
-spec enable_tool_guard() -> ok.
enable_tool_guard() ->
    Limits = get_limits(tool),
    MaxHeap = maps:get(max_heap, Limits),
    MaxBinHeap = maps:get(max_bin_heap, Limits),

    ok = configure_limits(MaxHeap, MaxBinHeap),

    logger:info("Tool memory guard enabled: ~p bytes", [MaxHeap * erlang:system_info(wordsize)]),
    ok.

%% @doc Enable memory guard for transport layer processes.
%% Uses lower limits (30MB heap, 15MB binary heap) for transport processes.
%%
%% Transports handle message encoding/decoding and should maintain
%% low memory footprints to handle high connection counts.
%%
%% @return ok
-spec enable_transport_guard() -> ok.
enable_transport_guard() ->
    Limits = get_limits(transport),
    MaxHeap = maps:get(max_heap, Limits),
    MaxBinHeap = maps:get(max_bin_heap, Limits),

    ok = configure_limits(MaxHeap, MaxBinHeap),

    logger:info("Transport memory guard enabled: ~p bytes", [MaxHeap * erlang:system_info(wordsize)]),
    ok.

%% @doc Get current memory usage for the calling process.
%% Returns tuple of {HeapSize, BinaryHeapSize} in bytes.
%%
%% @return {HeapSize, BinaryHeapSize}
-spec get_memory_usage() -> memory_usage().
get_memory_usage() ->
    {memory, Heap} = erlang:process_info(self(), memory),
    {binary, BinHeap} = erlang:process_info(self(), binary),
    {Heap, BinHeap}.

%% @doc Force the current process to hibernate.
%% Hibernate discards the call stack and minimizes memory footprint,
%% useful when approaching memory limits.
%%
%% @return ok
-spec force_hibernate() -> ok.
force_hibernate() ->
    BeforeMemory = element(2, erlang:process_info(self(), memory)),

    %% Trigger hibernation
    erlang:hibernate(erlang, apply, [fun() -> ok end, []]),

    %% Wait a bit for hibernation to take effect
    timer:sleep(10),

    AfterMemory = element(2, erlang:process_info(self(), memory)),
    SavedBytes = BeforeMemory - AfterMemory,

    logger:info("Process hibernated, saved ~p bytes (~.2f%)",
                [SavedBytes, (SavedBytes / BeforeMemory) * 100]),
    ok.

%% @doc Validate memory usage is within configured limits.
%% Returns {ok, UsagePercent} if within limits, {warning, Percent} if
%% approaching hibernation threshold, {error, Percent} if exceeded.
%%
%% @param ProcessType Type of process (context, tool, transport, generic)
%% @return {ok | warning | error, UsagePercent}
-spec validate_memory(process_type()) ->
    {ok, float()} | {warning, float()} | {error, float()}.
validate_memory(ProcessType) ->
    Limits = get_limits(ProcessType),
    MaxHeap = maps:get(max_heap, Limits) * erlang:system_info(wordsize),
    Threshold = maps:get(hibernate_threshold, Limits),

    {CurrentHeap, _} = get_memory_usage(),
    UsagePercent = (CurrentHeap / MaxHeap) * 100,

    case UsagePercent of
        P when P >= Threshold * 100 ->
            logger:warning("Process memory usage critical: ~.2f% (~p / ~p bytes)",
                          [P, CurrentHeap, MaxHeap]),
            {error, UsagePercent};
        P when P >= Threshold * 90 ->
            logger:warning("Process memory usage high: ~.2f% (~p / ~p bytes)",
                          [UsagePercent, CurrentHeap, MaxHeap]),
            {warning, UsagePercent};
        _UsagePercent ->
            {ok, UsagePercent}
    end.

%% @doc Get memory limits for a process type.
%% Returns map with max_heap, max_bin_heap, and hibernate_threshold.
%% Reads from application config (sys.config) with defaults as fallback.
%%
%% @param ProcessType Type of process
%% @return Memory limits map
-spec get_limits(process_type()) -> memory_limit().
get_limits(ProcessType) ->
    case application:get_env(erlmcp_core, memory_guard) of
        {ok, Config} ->
            case lists:keyfind(ProcessType, 1, Config) of
                {ProcessType, Limits} ->
                    MaxHeap = proplists:get_value(max_heap_size, Limits,
                        default_max_heap(ProcessType)),
                    MaxBinHeap = proplists:get_value(max_bin_vheap_size, Limits,
                        default_max_bin_heap(ProcessType)),
                    Threshold = default_threshold(ProcessType),
                    #{max_heap => MaxHeap,
                      max_bin_heap => MaxBinHeap,
                      hibernate_threshold => Threshold};
                false ->
                    %% No config for this type, use defaults
                    default_limits(ProcessType)
            end;
        undefined ->
            %% No config at all, use defaults
            default_limits(ProcessType)
    end.

%% @private Get default limits for a process type
-spec default_limits(process_type()) -> memory_limit().
default_limits(context) -> ?DEFAULT_CONTEXT_LIMITS;
default_limits(tool) -> ?DEFAULT_TOOL_LIMITS;
default_limits(transport) -> ?DEFAULT_TRANSPORT_LIMITS;
default_limits(generic) -> ?DEFAULT_GENERIC_LIMITS.

%% @private Get default max heap size for process type
-spec default_max_heap(process_type()) -> pos_integer().
default_max_heap(context) -> 100_000_000;
default_max_heap(tool) -> 50_000_000;
default_max_heap(transport) -> 30_000_000;
default_max_heap(generic) -> 20_000_000.

%% @private Get default max binary heap size for process type
-spec default_max_bin_heap(process_type()) -> pos_integer().
default_max_bin_heap(context) -> 50_000_000;
default_max_bin_heap(tool) -> 25_000_000;
default_max_bin_heap(transport) -> 15_000_000;
default_max_bin_heap(generic) -> 10_000_000.

%% @private Get default hibernate threshold for process type
-spec default_threshold(process_type()) -> float().
default_threshold(context) -> 0.9;
default_threshold(tool) -> 0.85;
default_threshold(transport) -> 0.80;
default_threshold(generic) -> 0.80.

%% @doc Check if running on OTP 28 or later.
%% Memory guard flags require OTP 28+.
%%
%% @return true if OTP 28+, false otherwise
-spec is_otp_28_or_later() -> boolean().
is_otp_28_or_later() ->
    try
        %% Try to set a flag that only exists in OTP 28+
        erlang:process_flag(min_heap_size, 100),
        %% Restore to default
        erlang:process_flag(min_heap_size, 0),
        true
    catch
        error:badarg -> false
    end.
