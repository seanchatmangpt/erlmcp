%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_transport_sup - CLI Transport Supervisor
%%%
%%% Manages CLI transport implementations with dynamic loading.
%%% Supports multiple transport types with proper isolation.
%%%
%%% Supervision Strategy: one_for_one
%%% - Transport crash: restart individual transport only
%%% - Protocol crash: restart individual protocol only
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_transport_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_transport/3, stop_transport/1, list_transports/0]).
%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the transport supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a transport instance
-spec start_transport(binary(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type, Config) ->
    ?LOG_INFO(#{what => cli_transport_supervisor_start_child,
                transport_id => TransportId,
                transport_type => Type,
                config => Config}),

    case resolve_transport_module(Type) of
        {ok, Module} ->
            ChildSpec = build_transport_child_spec(TransportId, Module, Type, Config),
            case supervisor:start_child(?SERVER, ChildSpec) of
                {ok, Pid} = Result ->
                    ?LOG_INFO(#{what => cli_transport_child_started,
                                transport_id => TransportId,
                                transport_type => Type,
                                pid => Pid}),
                    Result;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{what => cli_transport_child_start_failed,
                                 transport_id => TransportId,
                                 transport_type => Type,
                                 reason => Reason}),
                    Error
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{what => cli_transport_module_resolution_failed,
                         transport_id => TransportId,
                         transport_type => Type,
                         reason => Reason}),
            Error
    end.

%% @doc Stop a transport instance
-spec stop_transport(binary()) -> ok | {error, term()}.
stop_transport(TransportId) ->
    case supervisor:terminate_child(?SERVER, TransportId) of
        ok ->
            supervisor:delete_child(?SERVER, TransportId);
        {error, not_found} ->
            ok;  % Already terminated
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc List all active transports
-spec list_transports() -> [{binary(), pid(), atom()}].
list_transports() ->
    Children = supervisor:which_children(?SERVER),
    lists:map(fun({Id, Pid, _Type, _Modules}) ->
        case is_pid(Pid) andalso is_process_alive(Pid) of
            true ->
                Type = extract_transport_type(Id, Pid),
                {Id, Pid, Type};
            false ->
                {Id, undefined, unknown}
        end
    end, Children).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @doc Initialize the transport supervisor
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Opts) ->
    ?LOG_INFO(#{what => cli_transport_supervisor_init}),

    SupFlags =
        #{strategy => one_for_one,  % Transport failures are isolated
          intensity => 3,           % Max 3 restarts per transport
          period => 30},            % Within 30 seconds

    % Start with empty child specs - transports are added dynamically
    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Resolve transport type to module name
-spec resolve_transport_module(atom()) -> {ok, module()} | {error, unknown_transport_type}.
resolve_transport_module(stdio) ->
    {ok, erlmcp_cli_transport_stdio};
resolve_transport_module(tcp) ->
    {ok, erlmcp_cli_transport_tcp};
resolve_transport_module(http) ->
    {ok, erlmcp_cli_transport_http};
resolve_transport_module(ws) ->
    {ok, erlmcp_cli_transport_ws};
resolve_transport_module(file) ->
    {ok, erlmcp_cli_transport_file};
resolve_transport_module(Type) ->
    ?LOG_WARNING(#{what => unknown_cli_transport_type, transport_type => Type}),
    {error, unknown_transport_type}.

%% @doc Build transport child specification
-spec build_transport_child_spec(binary(), module(), atom(), map()) -> supervisor:child_spec().
build_transport_child_spec(TransportId, Module, Type, Config) ->
    #{
        id => TransportId,
        start => {Module, start_link, [TransportId, Config]},
        restart => transport_restart_strategy(Type),
        shutdown => transport_shutdown_timeout(Type),
        type => worker,
        modules => [Module]
    }.

%% @doc Determine transport restart strategy
-spec transport_restart_strategy(atom()) -> temporary | transient | permanent.
transport_restart_strategy(stdio) ->
    temporary;  % stdio is typically one-shot
transport_restart_strategy(tcp) ->
    transient;    % tcp should restart on failures
transport_restart_strategy(http) ->
    transient;   % http should restart on failures
transport_restart_strategy(ws) ->
    transient;     % ws should restart on failures
transport_restart_strategy(file) ->
    transient;    % file should restart on failures
transport_restart_strategy(_) ->
    temporary.      % default to temporary for unknown

%% @doc Determine transport shutdown timeout
-spec transport_shutdown_timeout(atom()) -> non_neg_integer().
transport_shutdown_timeout(stdio) ->
    2000;  % stdio should shut down quickly
transport_shutdown_timeout(tcp) ->
    5000;    % tcp needs time for connection cleanup
transport_shutdown_timeout(http) ->
    5000;   % http needs time for request completion
transport_shutdown_timeout(ws) ->
    5000;     % ws needs time for connection cleanup
transport_shutdown_timeout(file) ->
    3000;     % file needs time for I/O completion
transport_shutdown_timeout(_) ->
    5000.      % default to 5s for unknown

%% @doc Extract transport type from transport ID and PID
-spec extract_transport_type(binary(), pid()) -> atom().
extract_transport_type(TransportId, Pid) ->
    %% Try to get transport type from process name
    ProcessInfo = process_info(Pid),
    case ProcessInfo of
        {registered_name, Name} ->
            extract_type_from_name(Name);
        _ ->
            %% Fallback to ID-based detection
            case binary:split(TransportId, <<"_">>, [global]) of
                [<<"transport">>, TypeBin | _] ->
                    binary_to_atom(TypeBin, utf8);
                _ ->
                    unknown
            end
    end.

%% @doc Extract type from process name
-spec extract_type_from_name(atom()) -> atom().
extract_type_from_name(erlmcp_cli_transport_stdio) ->
    stdio;
extract_type_from_name(erlmcp_cli_transport_tcp) ->
    tcp;
extract_type_from_name(erlmcp_cli_transport_http) ->
    http;
extract_type_from_name(erlmcp_cli_transport_ws) ->
    ws;
extract_type_from_name(erlmcp_cli_transport_file) ->
    file;
extract_type_from_name(_) ->
    unknown.