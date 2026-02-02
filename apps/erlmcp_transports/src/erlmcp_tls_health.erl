%%%-------------------------------------------------------------------
%%% @doc
%%% TLS Health Monitoring for erlmcp Transports
%%%
%%% This module provides comprehensive monitoring and metrics collection
%%% for TLS connections across all transport types (TCP, HTTP, WebSocket).
%%% It tracks handshake success rates, cipher suite usage, certificate
%%% expiration, and provides real-time health checks.
%%%
%%% == Features ==
%%%
%%% - Handshake success rate tracking by transport type
%%% - Cipher suite usage distribution
%%% - Certificate expiration monitoring with alerts
%%% - Handshake timing percentiles (p50, p95, p99)
%%% - Failure reason categorization
%%% - Real-time health status API
%%% - OTP 27-28 TLS 1.3 optimizations tracking
%%%
%%% == Architecture ==
%%%
%%% Uses ETS tables for high-performance metrics storage:
%%% - tls_handshakes: {TransportType, Result, Timestamp}
%%% - tls_ciphers: {CipherSuite, TransportType, Count}
%%% - tls_failures: {TransportType, Reason, Count}
%%% - tls_certificates: {CertFile, ExpirationDate, Status}
%%% - tls_timing: {TransportType, HandshakeTimeMs}
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Report successful TLS handshake
%%% ok = erlmcp_tls_health:report_handshake(tcp, success, #{cipher => "TLS_AES_256_GCM_SHA384", timing => 45}).
%%'
%%% %% Check overall TLS health
%%% {ok, Health} = erlmcp_tls_health:check_tls_health().
%%%'
%%% %% Get per-transport statistics
%%% {ok, Stats} = erlmcp_tls_health:get_tls_stats(http).
%%%'
%%% %% Get cipher usage distribution
%%% {ok, Ciphers} = erlmcp_tls_health:get_cipher_usage().
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tls_health).

-behaviour(gen_server).

%% API
-export([start_link/0,
         report_handshake/3,
         report_handshake_failure/3,
         check_tls_health/0,
         get_tls_stats/1,
         get_cipher_usage/0,
         get_certificate_status/0,
         reset_metrics/0,
         get_handshake_percentiles/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%% ETS Table names
-define(TAB_HANDSHAKES, tls_health_handshakes).
-define(TAB_CIPHERS, tls_health_ciphers).
-define(TAB_FAILURES, tls_health_failures).
-define(TAB_CERTIFICATES, tls_health_certificates).
-define(TAB_TIMING, tls_health_timing).

%% Metrics window (milliseconds)
-define(METRICS_WINDOW, 300000). %% 5 minutes

%% Alert thresholds
-define(SUCCESS_RATE_THRESHOLD, 0.95). %% 95% success rate
-define(CERT_EXPIRY_ALERT_DAYS, 30).   %% 30 days

%%====================================================================
%% Type Definitions
%%====================================================================

-type transport_type() :: tcp | http | ws | sse.
-type handshake_result() :: success | failure.
-type handshake_info() ::
    #{cipher => string(),
      protocol => tls_version(),
      timing => non_neg_integer(),
      cert_file => file:filename_all()}.
-type tls_version() :: 'tlsv1.2' | 'tlsv1.3'.
-type failure_reason() :: cert_expired | cert_invalid | cipher_mismatch | timeout | unknown.
-type health_status() :: healthy | degraded | critical.
-type percentile() :: p50 | p95 | p99.

-record(state,
        {handshake_tab :: ets:tid(),
         cipher_tab :: ets:tid(),
         failure_tab :: ets:tid(),
         certificate_tab :: ets:tid(),
         timing_tab :: ets:tid(),
         cleanup_timer :: reference()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the TLS health monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Report a successful TLS handshake
-spec report_handshake(transport_type(), handshake_result(), handshake_info()) -> ok.
report_handshake(TransportType, Result, Info) ->
    gen_server:cast(?SERVER, {report_handshake, TransportType, Result, Info}),
    ok.

%% @doc Report a failed TLS handshake with reason
-spec report_handshake_failure(transport_type(), failure_reason(), handshake_info()) -> ok.
report_handshake_failure(TransportType, Reason, Info) ->
    gen_server:cast(?SERVER, {report_failure, TransportType, Reason, Info}),
    ok.

%% @doc Check overall TLS health status
-spec check_tls_health() -> {ok, health_status(), map()}.
check_tls_health() ->
    gen_server:call(?SERVER, check_health, 5000).

%% @doc Get TLS statistics for a specific transport
-spec get_tls_stats(transport_type()) -> {ok, map()} | {error, term()}.
get_tls_stats(TransportType) ->
    gen_server:call(?SERVER, {get_stats, TransportType}, 5000).

%% @doc Get cipher suite usage distribution
-spec get_cipher_usage() -> {ok, map()}.
get_cipher_usage() ->
    gen_server:call(?SERVER, get_cipher_usage, 5000).

%% @doc Get certificate expiration status
-spec get_certificate_status() -> {ok, map()}.
get_certificate_status() ->
    gen_server:call(?SERVER, get_certificate_status, 5000).

%% @doc Reset all metrics (useful for testing)
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?SERVER, reset_metrics, 5000).

%% @doc Get handshake timing percentiles for a transport
-spec get_handshake_percentiles(transport_type()) -> {ok, #{percentile() => float()}}.
get_handshake_percentiles(TransportType) ->
    gen_server:call(?SERVER, {get_percentiles, TransportType}, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init([]) ->
    ?LOG_INFO("Starting TLS health monitor"),

    %% Create ETS tables for metrics
    HandshakeTab = ets:new(?TAB_HANDSHAKES, [set, public, {write_concurrency, true}]),
    CipherTab = ets:new(?TAB_CIPHERS, [bag, public, {write_concurrency, true}]),
    FailureTab = ets:new(?TAB_FAILURES, [bag, public, {write_concurrency, true}]),
    CertificateTab = ets:new(?TAB_CERTIFICATES, [set, public]),
    TimingTab = ets:new(?TAB_TIMING, [bag, public, {write_concurrency, true}]),

    %% Start periodic cleanup timer
    CleanupTimer = schedule_cleanup(),

    {ok, #state{handshake_tab = HandshakeTab,
                cipher_tab = CipherTab,
                failure_tab = FailureTab,
                certificate_tab = CertificateTab,
                timing_tab = TimingTab,
                cleanup_timer = CleanupTimer}}.

%% @private
handle_call(check_health, _From, State) ->
    {ok, HealthStatus, Info} = calculate_health(State),
    {reply, {ok, HealthStatus, Info}, State};

handle_call({get_stats, TransportType}, _From, State) ->
    Stats = calculate_transport_stats(TransportType, State),
    {reply, {ok, Stats}, State};

handle_call(get_cipher_usage, _From, #state{cipher_tab = Tab} = State) ->
    Usage = calculate_cipher_usage(Tab),
    {reply, {ok, Usage}, State};

handle_call(get_certificate_status, _From, #state{certificate_tab = Tab} = State) ->
    Status = check_certificate_status(Tab),
    {reply, {ok, Status}, State};

handle_call(reset_metrics, _From, State) ->
    clear_all_tables(State),
    {reply, ok, State};

handle_call({get_percentiles, TransportType}, _From, #state{timing_tab = Tab} = State) ->
    Percentiles = calculate_percentiles(TransportType, Tab),
    {reply, {ok, Percentiles}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({report_handshake, TransportType, success, Info}, State) ->
    record_handshake_success(TransportType, Info, State),
    {noreply, State, hibernate};

handle_cast({report_handshake, TransportType, failure, Info}, State) ->
    record_handshake_failure(TransportType, Info, State),
    {noreply, State, hibernate};

handle_cast({report_failure, TransportType, Reason, Info}, State) ->
    record_handshake_failure(TransportType, Reason, Info, State),
    {noreply, State, hibernate};

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

%% @private
handle_info(cleanup_old_metrics, State) ->
    cleanup_metrics(State),
    NewTimer = schedule_cleanup(),
    {noreply, State#state{cleanup_timer = NewTimer}, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

%% @private
terminate(_Reason, #state{cleanup_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Metrics Recording
%%====================================================================

%% @private Record successful handshake
record_handshake_success(TransportType, Info, #state{handshake_tab = HTab,
                                                     cipher_tab = CTab,
                                                     timing_tab = TTab}) ->
    Timestamp = erlang:system_time(millisecond),

    %% Update handshake counters
    ets:update_counter(HTab, {TransportType, success}, {2, 1}, {{TransportType, success}, 0}),
    ets:update_counter(HTab, {TransportType, total}, {2, 1}, {{TransportType, total}, 0}),

    %% Record cipher usage
    Cipher = maps:get(cipher, Info, "unknown"),
    ets:insert(CTab, {{TransportType, Cipher}, Timestamp}),

    %% Record handshake timing
    Timing = maps:get(timing, Info, 0),
    ets:insert(TTab, {TransportType, Timing, Timestamp}),

    %% Track certificate if provided
    case maps:get(cert_file, Info, undefined) of
        undefined ->
            ok;
        CertFile ->
            track_certificate(CertFile, HTab)
    end,

    ?LOG_DEBUG("Recorded TLS handshake success: transport=~p cipher=~p timing=~pms",
               [TransportType, Cipher, Timing]).

%% @private Record handshake failure
record_handshake_failure(TransportType, Info, #state{handshake_tab = HTab,
                                                     failure_tab = FTab}) ->
    %% Update failure counters
    ets:update_counter(HTab, {TransportType, failure}, {2, 1}, {{TransportType, failure}, 0}),
    ets:update_counter(HTab, {TransportType, total}, {2, 1}, {{TransportType, total}, 0}),

    %% Extract failure reason from info
    Reason = maps:get(reason, Info, unknown),
    ets:insert(FTab, {{TransportType, Reason}, erlang:system_time(millisecond)}),

    ?LOG_WARNING("Recorded TLS handshake failure: transport=~p reason=~p", [TransportType, Reason]).

%% @private Record handshake failure with explicit reason
record_handshake_failure(TransportType, Reason, Info, #state{handshake_tab = HTab,
                                                               failure_tab = FTab}) ->
    %% Update failure counters
    ets:update_counter(HTab, {TransportType, failure}, {2, 1}, {{TransportType, failure}, 0}),
    ets:update_counter(HTab, {TransportType, total}, {2, 1}, {{TransportType, total}, 0}),

    %% Record failure reason
    ets:insert(FTab, {{TransportType, Reason}, erlang:system_time(millisecond)}),

    ?LOG_WARNING("Recorded TLS handshake failure: transport=~p reason=~p info=~p",
                 [TransportType, Reason, Info]).

%% @private Track certificate expiration
track_certificate(CertFile, HTab) ->
    case ets:lookup(HTab, {certificate, CertFile}) of
        [{_, _}] ->
            %% Already tracked
            ok;
        [] ->
            %% Load and check certificate
            case load_certificate_expiration(CertFile) of
                {ok, Expiration} ->
                    ets:insert(HTab, {{certificate, CertFile}, Expiration}),
                    ?LOG_INFO("Tracked certificate: file=~p expires=~p", [CertFile, Expiration]);
                {error, Reason} ->
                    ?LOG_WARNING("Failed to load certificate: file=~p reason=~p", [CertFile, Reason])
            end
    end.

%% @private Load certificate expiration date
load_certificate_expiration(CertFile) ->
    try
        {ok, CertPem} = file:read_file(CertFile),
        [{'Certificate', CertDer, _}] = public_key:pem_decode(CertPem),
        #'OTPCertificate'{tbsCertificate =
                              #'OTPTBSCertificate'{validity =
                                                       #'Validity'{notAfter = NotAfter}}} =
            public_key:der_decode('OTPCertificate', CertDer),
        {ok, NotAfter}
    catch
        Class:Error:Stacktrace ->
            ?LOG_ERROR("Failed to parse certificate: ~p:~p~n~p", [Class, Error, Stacktrace]),
            {error, invalid_certificate}
    end.

%%====================================================================
%% Internal Functions - Health Calculation
%%====================================================================

%% @private Calculate overall health status
calculate_health(#state{handshake_tab = HTab, failure_tab = FTab, certificate_tab = CTab}) ->
    %% Get success rate across all transports
    TotalSuccess = sum_counter(HTab, success),
    TotalFailure = sum_counter(HTab, failure),
    TotalAttempts = TotalSuccess + TotalFailure,

    {SuccessRate, Status} =
        case TotalAttempts of
            0 ->
                {1.0, healthy};  %% No data yet
            _ ->
                Rate = TotalSuccess / TotalAttempts,
                Status1 =
                    if
                        Rate >= ?SUCCESS_RATE_THRESHOLD -> healthy;
                        Rate >= 0.80 -> degraded;
                        true -> critical
                    end,
                {Rate, Status1}
        end,

    %% Get certificate status
    CertStatus = check_certificate_status(CTab),

    %% Build info map
    Info = #{
        success_rate => SuccessRate,
        total_handshakes => TotalAttempts,
        successful_handshakes => TotalSuccess,
        failed_handshakes => TotalFailure,
        certificate_alerts => maps:get(alert_count, CertStatus, 0),
        transports_with_failures => get_failure_types(FTab)
    },

    {ok, Status, Info}.

%% @private Sum counter across all transport types
sum_counter(Tab, Type) ->
    TransportTypes = [tcp, http, ws, sse],
    lists:foldl(fun(TT, Acc) ->
        case ets:lookup(Tab, {TT, Type}) of
            [{_, Count}] -> Acc + Count;
            [] -> Acc
        end
    end, 0, TransportTypes).

%% @private Get failure reason types
get_failure_types(Tab) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - ?METRICS_WINDOW,

    %% Get unique failure reasons in the window
    Failures = ets:foldl(fun({{Transport, Reason}, Timestamp}, Acc) ->
        case Timestamp >= Cutoff of
            true -> sets:add_element({Transport, Reason}, Acc);
            false -> Acc
        end
    end, sets:new(), Tab),

    sets:to_list(Failures).

%%====================================================================
%% Internal Functions - Statistics Calculation
%%====================================================================

%% @private Calculate transport-specific statistics
calculate_transport_stats(TransportType, #state{handshake_tab = HTab,
                                                cipher_tab = CTab,
                                                failure_tab = FTab}) ->
    %% Get handshake counters
    Success = lookup_counter(HTab, {TransportType, success}),
    Failure = lookup_counter(HTab, {TransportType, failure}),
    Total = Success + Failure,

    SuccessRate =
        case Total of
            0 -> 0.0;
            _ -> Success / Total
        end,

    %% Get top ciphers
    TopCiphers = get_top_ciphers(TransportType, CTab, 5),

    %% Get failure reasons
    Failures = get_failure_reasons(TransportType, FTab),

    #{
        transport_type => TransportType,
        total_handshakes => Total,
        successful_handshakes => Success,
        failed_handshakes => Failure,
        success_rate => SuccessRate,
        top_ciphers => TopCiphers,
        failure_reasons => Failures
    }.

%% @private Lookup counter with default
lookup_counter(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [{_, Count}] -> Count;
        [] -> 0
    end.

%% @private Get top cipher suites
get_top_ciphers(TransportType, Tab, Limit) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - ?METRICS_WINDOW,

    %% Count cipher usage in window
    CipherCounts =
        ets:foldl(fun({{TT, Cipher}, Timestamp}, Acc) ->
            case {TT, Timestamp >= Cutoff} of
                {TransportType, true} ->
                    maps:update_with(Cipher, fun(V) -> V + 1 end, 1, Acc);
                _ ->
                    Acc
            end
        end, #{}, Tab),

    %% Sort by count and take top N
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, maps:to_list(CipherCounts)),
    lists:sublist(Sorted, Limit).

%% @private Get failure reasons
get_failure_reasons(TransportType, Tab) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - ?METRICS_WINDOW,

    %% Count failures in window
    FailureCounts =
        ets:foldl(fun({{TT, Reason}, Timestamp}, Acc) ->
            case {TT, Timestamp >= Cutoff} of
                {TransportType, true} ->
                    maps:update_with(Reason, fun(V) -> V + 1 end, 1, Acc);
                _ ->
                    Acc
            end
        end, #{}, Tab),

    maps:to_list(FailureCounts).

%%====================================================================
%% Internal Functions - Cipher Usage
%%====================================================================

%% @private Calculate cipher usage distribution
calculate_cipher_usage(Tab) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - ?METRICS_WINDOW,

    %% Count by cipher suite
    TotalCount =
        ets:foldl(fun({{_TT, _Cipher}, Timestamp}, Acc) ->
            case Timestamp >= Cutoff of
                true -> Acc + 1;
                false -> Acc
            end
        end, 0, Tab),

    CipherCounts =
        ets:foldl(fun({{_TT, Cipher}, Timestamp}, Acc) ->
            case Timestamp >= Cutoff of
                true ->
                    maps:update_with(Cipher, fun(V) -> V + 1 end, 1, Acc);
                false ->
                    Acc
            end
        end, #{}, Tab),

    %% Convert to percentages
    maps:map(fun(_Cipher, Count) ->
        case TotalCount of
            0 -> 0.0;
            _ -> (Count / TotalCount) * 100
        end
    end, CipherCounts).

%%====================================================================
%% Internal Functions - Certificate Status
%%====================================================================

%% @private Check certificate expiration status
check_certificate_status(Tab) ->
    Now = erlang:system_time(millisecond),
    AlertThreshold = ?CERT_EXPIRY_ALERT_DAYS * 24 * 60 * 60 * 1000,

    {AlertCount, Certs} =
        ets:foldl(fun({{certificate, CertFile}, NotAfter}, {AlertAcc, ListAcc}) ->
            ExpirationMs = calendar:datetime_to_gregorian_seconds(NotAfter) * 1000,
            TimeUntilExpiry = ExpirationMs - Now,
            DaysUntilExpiry = TimeUntilExpiry / (24 * 60 * 60 * 1000),

            Status =
                if
                    TimeUntilExpiry < 0 -> expired;
                    TimeUntilExpiry < AlertThreshold -> expiring_soon;
                    true -> valid
                end,

            NewAlertAcc =
                case Status of
                    expired -> AlertAcc + 1;
                    expiring_soon -> AlertAcc + 1;
                    valid -> AlertAcc
                end,

            CertInfo = #{
                file => CertFile,
                expiration => NotAfter,
                days_until_expiration => DaysUntilExpiry,
                status => Status
            },

            {NewAlertAcc, [CertInfo | ListAcc]}
        end, {0, []}, Tab),

    #{
        total_certificates => length(Certs),
        alert_count => AlertCount,
        certificates => Certs
    }.

%%====================================================================
%% Internal Functions - Percentile Calculation
%%====================================================================

%% @private Calculate timing percentiles
calculate_percentiles(TransportType, Tab) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - ?METRICS_WINDOW,

    %% Collect timing data in window
    Timings =
        ets:foldl(fun({TT, Timing, Timestamp}, Acc) ->
            case {TT, Timestamp >= Cutoff} of
                {TransportType, true} -> [Timing | Acc];
                _ -> Acc
            end
        end, [], Tab),

    case Timings of
        [] ->
            #{p50 => 0.0, p95 => 0.0, p99 => 0.0};
        _ ->
            Sorted = lists:sort(Timings),
            Len = length(Sorted),
            #{
                p50 => percentile(Sorted, Len, 0.50),
                p95 => percentile(Sorted, Len, 0.95),
                p99 => percentile(Sorted, Len, 0.99)
            }
    end.

%% @private Calculate percentile value
percentile(SortedList, Len, P) ->
    Index = max(1, min(Len, trunc(Len * P) + 1)),
    lists:nth(Index, SortedList).

%%====================================================================
%% Internal Functions - Maintenance
%%====================================================================

%% @private Schedule periodic cleanup
schedule_cleanup() ->
    Interval = 60000, %% 1 minute
    erlang:send_after(Interval, self(), cleanup_old_metrics).

%% @private Cleanup old metrics
cleanup_metrics(#state{cipher_tab = CTab, failure_tab = FTab, timing_tab = TTab}) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - ?METRICS_WINDOW,

    %% Clean old cipher entries
    ets:select_delete(CTab, [{{{'$1', '$2'}, '$3'}, [{ '<', '$3', Cutoff }], [true]}]),

    %% Clean old failure entries
    ets:select_delete(FTab, [{{{'$1', '$2'}, '$3'}, [{ '<', '$3', Cutoff }], [true]}]),

    %% Clean old timing entries
    ets:select_delete(TTab, [{{{ '$1', '$2', '$3'}}, [{ '<', '$3', Cutoff }], [true]}]),

    ?LOG_DEBUG("Cleaned up old TLS metrics").

%% @private Clear all tables
clear_all_tables(#state{handshake_tab = HTab,
                        cipher_tab = CTab,
                        failure_tab = FTab,
                        certificate_tab = CTab2,
                        timing_tab = TTab}) ->
    ets:delete_all_objects(HTab),
    ets:delete_all_objects(CTab),
    ets:delete_all_objects(FTab),
    ets:delete_all_objects(CTab2),
    ets:delete_all_objects(TTab),
    ?LOG_INFO("Cleared all TLS health metrics").
