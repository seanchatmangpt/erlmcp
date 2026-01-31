-module(erlmcp_elicitation).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    create_elicitation/2,
    get_elicitation_status/1,
    cancel_elicitation/1,
    complete_elicitation/2,
    list_elicitations/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type elicitation_id() :: binary().
-type elicitation_mode() :: inline | url | terminal.
-type elicitation_status() :: pending | active | completed | cancelled | timeout.

-record(elicitation_state, {
    id :: elicitation_id(),
    mode :: elicitation_mode(),
    status :: elicitation_status(),
    config :: map(),
    client_pid :: pid() | undefined,
    client_monitor :: reference() | undefined,
    result :: term() | undefined,
    created_at :: integer(),
    timeout_at :: integer(),
    size_limit :: pos_integer()
}).

-type elicitation_state() :: #elicitation_state{}.

%% Server state
-record(state, {
    elicitations = #{} :: #{elicitation_id() => elicitation_state()},
    rate_limits = #{} :: #{pid() => {integer(), integer()}},  % {count, window_start}
    max_elicitations_per_client = 10 :: pos_integer(),
    max_concurrent_elicitations = 100 :: pos_integer(),
    default_timeout = 30000 :: pos_integer(),  % 30 seconds
    default_size_limit = 1048576 :: pos_integer()  % 1MB
}).

%% Private IP ranges for SSRF protection
-define(PRIVATE_IP_RANGES, [
    {127, 0, 0, 0, 8},      % 127.0.0.0/8
    {10, 0, 0, 0, 8},       % 10.0.0.0/8
    {172, 16, 0, 0, 12},    % 172.16.0.0/12
    {192, 168, 0, 0, 16},   % 192.168.0.0/16
    {0, 0, 0, 0, 8},        % 0.0.0.0/8
    {169, 254, 0, 0, 16},   % 169.254.0.0/16 (link-local)
    {192, 0, 0, 0, 24},     % 192.0.0.0/24 (IANA IPv4 Special Purpose Address Registry)
    {192, 0, 2, 0, 24},     % 192.0.2.0/24 (TEST-NET-1)
    {198, 51, 100, 0, 24},  % 198.51.100.0/24 (TEST-NET-2)
    {203, 0, 113, 0, 24},   % 203.0.113.0/24 (TEST-NET-3)
    {224, 0, 0, 0, 4}       % 224.0.0.0/4 (multicast)
]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec create_elicitation(map(), pid() | undefined) ->
    {ok, elicitation_id(), map()} | {error, term()}.
create_elicitation(Config, ClientPid) ->
    gen_server:call(?MODULE, {create_elicitation, Config, ClientPid}, 5000).

-spec get_elicitation_status(elicitation_id()) ->
    {ok, map()} | {error, not_found | term()}.
get_elicitation_status(ElicitationId) ->
    gen_server:call(?MODULE, {get_elicitation_status, ElicitationId}, 5000).

-spec cancel_elicitation(elicitation_id()) -> ok | {error, not_found | term()}.
cancel_elicitation(ElicitationId) ->
    gen_server:call(?MODULE, {cancel_elicitation, ElicitationId}, 5000).

-spec complete_elicitation(elicitation_id(), term()) ->
    ok | {error, not_found | invalid_state | term()}.
complete_elicitation(ElicitationId, Result) ->
    gen_server:call(?MODULE, {complete_elicitation, ElicitationId, Result}, 5000).

-spec list_elicitations() -> {ok, [map()]}.
list_elicitations() ->
    gen_server:call(?MODULE, list_elicitations, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting elicitation server"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} |
    {noreply, #state{}} |
    {stop, term(), term(), #state{}}.

handle_call({create_elicitation, Config, ClientPid}, _From, State) ->
    case validate_elicitation_config(Config, State) of
        {ok, ValidatedConfig} ->
            case check_rate_limit(ClientPid, State) of
                {ok, StateAfterRateLimit} ->
                    case check_concurrent_limit(StateAfterRateLimit) of
                        {ok, StateAfterConcurrent} ->
                            {Result, FinalState} = do_create_elicitation(ValidatedConfig, ClientPid, StateAfterConcurrent),
                            {reply, Result, FinalState};
                        {error, _} = Error ->
                            {reply, Error, StateAfterRateLimit}
                    end;
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({get_elicitation_status, ElicitationId}, _From, State) ->
    case maps:get(ElicitationId, State#state.elicitations, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ElicitationState ->
            Response = #{
                id => ElicitationState#elicitation_state.id,
                mode => ElicitationState#elicitation_state.mode,
                status => ElicitationState#elicitation_state.status,
                created_at => ElicitationState#elicitation_state.created_at,
                timeout_at => ElicitationState#elicitation_state.timeout_at,
                config => sanitize_config(ElicitationState#elicitation_state.config)
            },
            {reply, {ok, Response}, State}
    end;

handle_call({cancel_elicitation, ElicitationId}, _From, State) ->
    case maps:get(ElicitationId, State#state.elicitations, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ElicitationState ->
            case ElicitationState#elicitation_state.status of
                completed ->
                    {reply, {error, already_completed}, State};
                cancelled ->
                    {reply, {error, already_cancelled}, State};
                timeout ->
                    {reply, {error, already_timeout}, State};
                _ ->
                    NewState = ElicitationState#elicitation_state{status = cancelled},
                    NewElicitations = maps:put(ElicitationId, NewState, State#state.elicitations),
                    {reply, ok, State#state{elicitations = NewElicitations}}
            end
    end;

handle_call({complete_elicitation, ElicitationId, Result}, _From, State) ->
    case maps:get(ElicitationId, State#state.elicitations, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ElicitationState ->
            case ElicitationState#elicitation_state.status of
                completed ->
                    {reply, {error, already_completed}, State};
                cancelled ->
                    {reply, {error, already_cancelled}, State};
                timeout ->
                    {reply, {error, already_timeout}, State};
                _ ->
                    case validate_result_size(Result, ElicitationState#elicitation_state.size_limit) of
                        ok ->
                            NewState = ElicitationState#elicitation_state{
                                status = completed,
                                result = Result
                            },
                            NewElicitations = maps:put(ElicitationId, NewState, State#state.elicitations),
                            {reply, ok, State#state{elicitations = NewElicitations}};
                        {error, _} = Error ->
                            {reply, Error, State}
                    end
            end
    end;

handle_call(list_elicitations, _From, State) ->
    List = [begin
        #{
            id => ES#elicitation_state.id,
            mode => ES#elicitation_state.mode,
            status => ES#elicitation_state.status,
            created_at => ES#elicitation_state.created_at,
            timeout_at => ES#elicitation_state.timeout_at
        }
    end || ES <- maps:values(State#state.elicitations)],
    {reply, {ok, List}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({elicitation_timeout, ElicitationId}, State) ->
    case maps:get(ElicitationId, State#state.elicitations, undefined) of
        undefined ->
            {noreply, State};
        ElicitationState ->
            case ElicitationState#elicitation_state.status of
                pending ->
                    NewStateRec = ElicitationState#elicitation_state{status = timeout},
                    NewElicitations = maps:put(ElicitationId, NewStateRec, State#state.elicitations),
                    {noreply, State#state{elicitations = NewElicitations}};
                _ ->
                    {noreply, State}
            end
    end;

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    %% Clean up elicitations for monitored client
    NewElicitations = maps:filter(fun(_Id, ES) ->
        ES#elicitation_state.client_monitor =/= MonitorRef
    end, State#state.elicitations),
    {noreply, State#state{elicitations = NewElicitations}};

handle_info({'EXIT', Pid, Reason}, State) ->
    logger:info("Elicitation client ~p exited: ~p", [Pid, Reason]),
    %% Clean up elicitations for this client (legacy, for linked processes)
    NewElicitations = maps:filter(fun(_Id, ES) ->
        ES#elicitation_state.client_pid =/= Pid
    end, State#state.elicitations),
    {noreply, State#state{elicitations = NewElicitations}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Elicitation server terminating"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec validate_elicitation_config(map(), #state{}) ->
    {ok, map()} | {error, term()}.
validate_elicitation_config(Config, _State) ->
    %% Validate mode
    Mode = maps:get(<<"mode">>, Config, <<"inline">>),
    ValidModes = [<<"inline">>, <<"url">>, <<"terminal">>],
    case lists:member(Mode, ValidModes) of
        false ->
            {error, {invalid_mode, Mode}};
        true ->
            %% Mode-specific validation
            case binary_to_atom(Mode, utf8) of
                url ->
                    validate_url_config(Config);
                _ ->
                    {ok, Config}
            end
    end.

-spec validate_url_config(map()) -> {ok, map()} | {error, term()}.
validate_url_config(Config) ->
    case maps:get(<<"url">>, Config, undefined) of
        undefined ->
            {error, url_required};
        Url when is_binary(Url) ->
            case validate_url_safety(Url) of
                ok ->
                    {ok, Config};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, invalid_url_type}
    end.

-spec validate_url_safety(binary()) -> ok | {error, term()}.
validate_url_safety(Url) ->
    %% Parse URL to check for SSRF vulnerabilities
    try uri_string:parse(Url) of
        #{host := HostBin} ->
            Host = binary_to_list(HostBin),
            case is_safe_host(Host) of
                true ->
                    ok;
                false ->
                    logger:warning("Blocked potentially unsafe URL: ~p", [Url]),
                    {error, {unsafe_url, Host}}
            end;
        _ ->
            logger:warning("Invalid URL format: ~p", [Url]),
            {error, {invalid_url, Url}}
    catch
        _:_ ->
            logger:warning("Failed to parse URL ~p", [Url]),
            {error, {invalid_url, Url}}
    end.

-spec is_safe_host(string()) -> boolean().
is_safe_host(Host) ->
    %% First check for dangerous hostnames
    LowerHost = string:to_lower(Host),
    DangerousHosts = ["localhost", "127.0.0.1", "0.0.0.0", "::1"],
    case lists:member(LowerHost, DangerousHosts) of
        true -> false;
        false ->
            %% Check against private IP ranges
            case inet:parse_address(Host) of
                {ok, {_, _, _, _} = IPAddr} ->
                    not is_private_ip(IPAddr);
                {ok, {A, _, _, _, _, _, _, _}} ->
                    %% For IPv6, block loopback and link-local
                    not (A =:= 0 orelse A =:= 16#fe80);
                _ ->
                    %% For hostnames, we could do DNS lookups, but for security,
                    %% we'll be conservative and only allow if not obviously private
                    true
            end
    end.

-spec is_private_ip({integer(), integer(), integer(), integer()}) -> boolean().
is_private_ip({A, B, C, D}) ->
    lists:any(fun({StartA, StartB, StartC, StartD, PrefixLen}) ->
        is_in_network({A, B, C, D}, {StartA, StartB, StartC, StartD}, PrefixLen)
    end, ?PRIVATE_IP_RANGES).

-spec is_in_network(
    {integer(), integer(), integer(), integer()},
    {integer(), integer(), integer(), integer()},
    integer()
) -> boolean().
is_in_network({A, B, C, D}, {StartA, StartB, StartC, StartD}, PrefixLen) ->
    <<IP:32>> = <<A, B, C, D>>,
    <<Start:32>> = <<StartA, StartB, StartC, StartD>>,
    Mask = bnot ((1 bsl (32 - PrefixLen)) - 1),
    (IP band Mask) =:= (Start band Mask).

-spec check_rate_limit(pid() | undefined, #state{}) ->
    {ok, #state{}} | {error, rate_limited}.
check_rate_limit(undefined, State) ->
    {ok, State};
check_rate_limit(ClientPid, State) ->
    Now = erlang:system_time(millisecond),
    Window = 60000,  % 1 minute window
    MaxRequests = 10,

    case maps:get(ClientPid, State#state.rate_limits, undefined) of
        undefined ->
            NewRateLimits = maps:put(ClientPid, {1, Now}, State#state.rate_limits),
            {ok, State#state{rate_limits = NewRateLimits}};
        {Count, WindowStart} when Now - WindowStart < Window ->
            if
                Count >= MaxRequests ->
                    {error, rate_limited};
                true ->
                    NewRateLimits = maps:put(ClientPid, {Count + 1, WindowStart}, State#state.rate_limits),
                    {ok, State#state{rate_limits = NewRateLimits}}
            end;
        _ ->
            %% Window expired, reset
            NewRateLimits = maps:put(ClientPid, {1, Now}, State#state.rate_limits),
            {ok, State#state{rate_limits = NewRateLimits}}
    end.

-spec check_concurrent_limit(#state{}) ->
    {ok, #state{}} | {error, too_many_elicitations}.
check_concurrent_limit(State) ->
    Count = maps:size(State#state.elicitations),
    if
        Count >= State#state.max_concurrent_elicitations ->
            {error, too_many_elicitations};
        true ->
            {ok, State}
    end.

-spec do_create_elicitation(map(), pid() | undefined, #state{}) ->
    {{ok, elicitation_id(), map()}, #state{}} |
    {{error, term()}, #state{}}.
do_create_elicitation(Config, ClientPid, State) ->
    %% Generate unique ID
    Id = generate_elicitation_id(),
    Mode = binary_to_atom(maps:get(<<"mode">>, Config, <<"inline">>), utf8),
    TimeoutMs = maps:get(<<"timeout">>, Config, State#state.default_timeout),
    CreatedAt = erlang:system_time(millisecond),
    TimeoutAt = CreatedAt + TimeoutMs,
    SizeLimit = maps:get(<<"size_limit">>, Config, State#state.default_size_limit),

    %% Monitor client process if provided
    ClientMonitor = case ClientPid of
        undefined -> undefined;
        _ -> monitor(process, ClientPid)
    end,

    ElicitationState = #elicitation_state{
        id = Id,
        mode = Mode,
        status = pending,
        config = Config,
        client_pid = ClientPid,
        client_monitor = ClientMonitor,
        created_at = CreatedAt,
        timeout_at = TimeoutAt,
        size_limit = SizeLimit
    },

    %% Schedule timeout
    erlang:send_after(TimeoutMs, self(), {elicitation_timeout, Id}),

    %% Store elicitation
    NewElicitations = maps:put(Id, ElicitationState, State#state.elicitations),
    NewState = State#state{elicitations = NewElicitations},

    Response = #{
        id => Id,
        mode => Mode,
        status => pending,
        created_at => CreatedAt,
        timeout_at => TimeoutAt
    },

    {{ok, Id, Response}, NewState}.

-spec generate_elicitation_id() -> binary().
generate_elicitation_id() ->
    UniqueId = erlang:unique_integer([positive]),
    Timestamp = erlang:system_time(millisecond),
    <<Timestamp:64, UniqueId:64>>.

-spec validate_result_size(term(), pos_integer()) -> ok | {error, term()}.
validate_result_size(Result, SizeLimit) ->
    try
        Size = byte_size(term_to_binary(Result)),
        if
            Size > SizeLimit ->
                {error, result_too_large};
            true ->
                ok
        end
    catch
        _:_ ->
            {error, result_serialization_failed}
    end.

-spec sanitize_config(map()) -> map().
sanitize_config(Config) ->
    %% Remove sensitive information from config before returning
    maps:without([<<"secret">>, <<"password">>, <<"token">>], Config).
