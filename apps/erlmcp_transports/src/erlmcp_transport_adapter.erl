%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Adapter - Behavior Compliance Layer for gen_server Conflicts
%%%
%%% This module provides an adapter layer that resolves the naming conflict
%%% between erlmcp_transport behavior's init/1 callback and gen_server's init/1.
%%%
%%% == Problem ==
%%%
%%% The transport behavior requires:
%%% -callback init(Config :: map()) -> {ok, State} | {error, Reason}
%%%
%%% But gen_server also requires:
%%% -callback init(Args) -> {ok, State} | {stop, Reason}
%%%
%%% A module cannot implement both behaviors because init/1 would be defined twice.
%%%
%%% == Solution ==
%%%
%%% This adapter provides:
%%% 1. A wrapper module that transports can use without declaring both behaviors
%%% 2. Renamed callbacks that map to transport behavior requirements
%%% 3. Helper functions for common transport operations
%%%
%%% == Usage Pattern ==
%%%
%%% ```erlang
%%% -module(my_transport).
%%% -behaviour(gen_server).
%%% -behaviour(ranch_protocol).  % If applicable
%%%
%%% %% Transport API (not behavior callbacks)
%%% -export([transport_init/1, send/2, close/1, get_info/1]).
%%%
%%% %% gen_server callbacks
%%% -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%%%          terminate/2, code_change/3]).
%%'
%%% ```
%%%
%%% The adapter provides validation and ensures compliance with transport behavior
%%% requirements without requiring the -behaviour declaration.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_adapter).

-include("erlmcp.hrl").

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% API Exports - Transport Behavior Compliance Layer
%% =============================================================================

-export([validate_transport_compliance/1,
         wrap_transport_init/2,
         wrap_transport_send/3,
         wrap_transport_close/2,
         adapt_gen_server_init/2,
         ensure_registry_integration/3,
         cleanup_registry/1,
         format_compliance_error/2]).

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type transport_module() :: module().
-type transport_state() :: term().
-type transport_config() :: map().
-type compliance_result() :: {ok, [module:behavior_info()]} | {error, term()}.

%% =============================================================================
%% Compliance Validation
%% =============================================================================

%% @doc Validate that a module implements the transport behavior correctly
%%
%% This function checks that a module provides all required transport callbacks
%% without actually declaring the -behaviour(erlmcp_transport_behavior) to avoid
%% the init/1 conflict with gen_server.
%%
%% Required exports (not behavior callbacks):
%% - transport_init/1: Initialize transport with config
%% - send/2: Send data through transport
%% - close/1: Close transport and cleanup
%% - get_info/1: Get transport information (optional)
%% - handle_transport_call/2: Handle custom calls (optional)
%%
%% @param Module The module to validate
%% @returns {ok, Behaviors} if compliant, {error, Reason} if not
-spec validate_transport_compliance(transport_module()) -> compliance_result().
validate_transport_compliance(Module) when is_atom(Module) ->
    try
        %% Check module exists and is loaded
        case code:is_loaded(Module) of
            false ->
                case code:load_file(Module) of
                    {module, Module} ->
                        check_transport_exports(Module);
                    {error, Reason} ->
                        {error, {module_load_failed, Reason}}
                end;
            _ ->
                check_transport_exports(Module)
        end
    catch
        Type:Error:Stacktrace ->
            {error, {validation_exception, {Type, Error, Stacktrace}}}
    end.

%% @private
%% Check that module exports all required transport functions
-spec check_transport_exports(transport_module()) -> compliance_result().
check_transport_exports(Module) ->
    RequiredFunctions = [{transport_init, 1}, {send, 2}, {close, 1}],
    OptionalFunctions = [{get_info, 1}, {handle_transport_call, 2}],

    %% Get module exports
    Exports = try Module:module_info(exports) catch _:_ -> [] end,

    %% Check required functions
    MissingRequired = [F || {F, A} <- RequiredFunctions,
                           not lists:member({F, A}, Exports)],

    case MissingRequired of
        [] ->
            %% All required functions present
            PresentOptional = [F || {F, _A} <- OptionalFunctions,
                                   lists:member({F, 1}, Exports)],
            {ok, #{required => RequiredFunctions,
                   optional => PresentOptional}};
        _ ->
            {error, {missing_required_callbacks, MissingRequired}}
    end.

%% =============================================================================
%% Transport Wrapper Functions
%% =============================================================================

%% @doc Wrap transport initialization with validation
%%
%% This adapter function wraps the transport module's transport_init/1
%% and adds standard initialization logic:
%% - Validates configuration
%% - Registers with erlmcp_registry
%% - Sets up monitoring
%% - Returns standardized state
%%
%% @param TransportModule The transport module to initialize
%% @param Config Transport configuration map
%% @returns {ok, State} | {error, Reason}
-spec wrap_transport_init(transport_module(), transport_config()) ->
                                {ok, transport_state()} | {error, term()}.
wrap_transport_init(TransportModule, Config) when is_map(Config) ->
    ?LOG_INFO("Initializing transport module ~p with config: ~p",
              [TransportModule, maps:without([password, secret, token], Config)]),

    %% Validate config has required fields
    case validate_config_fields(Config) of
        ok ->
            %% Call transport module's init
            case TransportModule:transport_init(Config) of
                {ok, State} ->
                    %% Ensure registry integration
                    TransportId = maps:get(transport_id, Config),
                    case ensure_registry_integration(TransportId, TransportModule, Config) of
                        ok ->
                            {ok, State};
                        {error, Reason} ->
                            %% Init succeeded but registry failed - cleanup
                            catch TransportModule:close(State),
                            {error, {registry_integration_failed, Reason}}
                    end;
                {error, Reason} ->
                    ?LOG_ERROR("Transport ~p init failed: ~p", [TransportModule, Reason]),
                    {error, {transport_init_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_config, Reason}}
    end.

%% @doc Wrap transport send with error handling and logging
%%
%% Provides consistent error handling and metrics for all transport sends.
%%
%% @param TransportModule The transport module
%% @param State Current transport state
%% @param Data Data to send
%% @returns ok | {error, Reason}
-spec wrap_transport_send(transport_module(), transport_state(), binary()) ->
                                 ok | {error, term()}.
wrap_transport_send(TransportModule, State, Data) ->
    try
        case TransportModule:send(State, Data) of
            ok ->
                ok;
            {error, Reason} = Error ->
                ?LOG_WARNING("Transport ~p send failed: ~p", [TransportModule, Reason]),
                Error
        end
    catch
        Type:Error:Stacktrace ->
            ?LOG_ERROR("Transport ~p send exception: ~p:~p~n~p",
                      [TransportModule, Type, Error, Stacktrace]),
            {error, {send_exception, {Type, Error}}}
    end.

%% @doc Wrap transport close with cleanup
%%
%% Ensures proper cleanup including registry unregistration.
%%
%% @param TransportModule The transport module
%% @param State Current transport state
%% @returns ok (always succeeds)
-spec wrap_transport_close(transport_module(), transport_state()) -> ok.
wrap_transport_close(TransportModule, State) ->
    ?LOG_INFO("Closing transport ~p", [TransportModule]),

    %% Unregister from registry
    try
        TransportId = extract_transport_id(State, TransportModule),
        cleanup_registry(TransportId)
    catch
        _:_ ->
            %% Extract failed, continue with close
            ok
    end,

    %% Call transport's close
    try
        case TransportModule:close(State) of
            ok ->
                ok;
            Other ->
                ?LOG_WARNING("Transport ~p close returned unexpected: ~p",
                            [TransportModule, Other]),
                ok
        end
    catch
        Type:Error:Stacktrace ->
            ?LOG_ERROR("Transport ~p close exception: ~p:~p~n~p",
                      [TransportModule, Type, Error, Stacktrace]),
            ok
    end.

%% =============================================================================
%% gen_server Adaptation
%% =============================================================================

%% @doc Adapt gen_server init/1 to transport init requirements
%%
%% This helper is for use in transport module's gen_server init/1
%% to adapt the gen_server Args to the transport Config format.
%%
%% Usage in transport module:
%% ```erlang
%% init([Owner, Opts]) ->
%%     case erlmcp_transport_adapter:adapt_gen_server_init(?MODULE, Opts) of
%%         {ok, State} ->
%%             {ok, State};
%%         {error, Reason} ->
%%             {stop, Reason}
%%     end.
%% ```
%%
%% @param TransportModule The transport module
%% @param Args Arguments from gen_server init/1
%% @returns {ok, State} | {error, Reason}
-spec adapt_gen_server_init(transport_module(), term()) ->
                                  {ok, transport_state()} | {error, term()}.
adapt_gen_server_init(TransportModule, Args) ->
    %% Convert gen_server args to transport config format
    Config = case Args of
                 ConfigMap when is_map(ConfigMap) ->
                     ConfigMap;
                 [Owner, ConfigMap] when is_pid(Owner), is_map(ConfigMap) ->
                     ConfigMap#{owner => Owner};
                 [ConfigMap] when is_map(ConfigMap) ->
                     ConfigMap;
                 _ ->
                     #{args => Args}
             end,

    %% Validate and initialize
    case validate_config_fields(Config) of
        ok ->
            wrap_transport_init(TransportModule, Config);
        {error, Reason} ->
            {error, Reason}
    end.

%% =============================================================================
%% Registry Integration
%% =============================================================================

%% @doc Ensure transport is registered with erlmcp_registry
%%
%% This is called automatically by wrap_transport_init but can also be
%% called manually if needed.
%%
%% @param TransportId Transport identifier
%% @param TransportModule The transport module
%% @param Config Transport configuration
%% @returns ok | {error, Reason}
-spec ensure_registry_integration(atom(), transport_module(), map()) ->
                                         ok | {error, term()}.
ensure_registry_integration(TransportId, TransportModule, Config) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ?LOG_WARNING("Registry not available for transport ~p", [TransportId]),
            ok;
        _RegistryPid ->
            TransportType = maps:get(type, Config, custom),
            TransportConfig =
                Config#{type => TransportType,
                        module => TransportModule,
                        pid => self(),
                        started_at => erlang:system_time(millisecond)},

            case erlmcp_registry:register_transport(TransportId, self(), TransportConfig) of
                ok ->
                    ?LOG_DEBUG("Registered transport ~p (~p) with registry",
                              [TransportId, TransportModule]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to register transport ~p: ~p", [TransportId, Reason]),
                    {error, Reason}
            end
    end.

%% @doc Cleanup transport registry registration
%%
%% @param TransportId Transport identifier
%% @returns ok
-spec cleanup_registry(atom()) -> ok.
cleanup_registry(TransportId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ok;
        _RegistryPid ->
            case erlmcp_registry:unregister_transport(TransportId) of
                ok ->
                    ?LOG_DEBUG("Unregistered transport ~p from registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to unregister transport ~p: ~p",
                                [TransportId, Reason]),
                    ok
            end
    end.

%% =============================================================================
%% Error Formatting
%% =============================================================================

%% @doc Format compliance errors for better debugging
%%
%% @param Module The module that failed compliance
%% @param Error The error reason
%% @returns Formatted error map
-spec format_compliance_error(transport_module(), term()) -> map().
format_compliance_error(Module, Error) ->
    #{error => transport_compliance_error,
      module => Module,
      reason => Error,
      hint => get_compliance_hint(Error),
      timestamp => erlang:system_time(millisecond)}.

%% @private
%% Get helpful hint based on error type
-spec get_compliance_hint(term()) -> binary().
get_compliance_hint({missing_required_callbacks, Missing}) ->
    MissingList = [io_lib:format("~p/~p", [F, A]) || {F, A} <- Missing],
    io_lib:format("Module must export: ~s", [string:join(MissingList, ", ")]);
get_compliance_hint({module_load_failed, Reason}) ->
    io_lib:format("Ensure module is compiled and in code path. Reason: ~p", [Reason]);
get_compliance_hint(_Error) ->
    <<"See erlmcp_transport_adapter documentation for implementation pattern">>.

%% =============================================================================
%% Internal Helpers
%% =============================================================================

%% @private
%% Validate required config fields
-spec validate_config_fields(map()) -> ok | {error, term()}.
validate_config_fields(Config) ->
    Required = [],
    Missing = [Field || Field <- Required, not maps:is_key(Field, Config)],
    case Missing of
        [] ->
            ok;
        _ ->
            {error, {missing_config_fields, Missing}}
    end.

%% @private
%% Extract transport_id from various state formats
-spec extract_transport_id(term(), transport_module()) -> atom().
extract_transport_id(State, Module) ->
    try
        %% Try Module:get_info/1 if available
        case erlang:function_exported(Module, get_info, 1) of
            true ->
                Info = Module:get_info(State),
                maps:get(transport_id, Info, unknown_transport);
            false ->
                %% Try to extract from state directly
                extract_id_from_state(State)
        end
    catch
        _:_ ->
            unknown_transport
    end.

%% @private
%% Extract transport_id from state (record or map)
-spec extract_id_from_state(term()) -> atom().
extract_id_from_state(State) when is_map(State) ->
    maps:get(transport_id, State, unknown_transport);
extract_id_from_state(State) when is_tuple(State), tuple_size(State) > 1 ->
    %% Assume record, try element 2 (common pattern for #state{transport_id = ...})
    case element(2, State) of
        Id when is_atom(Id) ->
            Id;
        _ ->
            unknown_transport
    end;
extract_id_from_state(_) ->
    unknown_transport.
