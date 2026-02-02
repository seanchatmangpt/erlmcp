%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_registry - CLI Command Registry
%%%
%%% Manages CLI command registration, lookup, and execution with
 comprehensive OTEL tracing and metrics collection.
%%%
%%% == Key Features ==
%%%
%%% 1. **Command Registration**: Dynamic command registration and discovery
%%% 2. **OTEL Integration**: Tracing for all registry operations
%%% 3. **Metrics Collection**: Command usage and performance metrics
%%% 4. **Safety Validation**: Command safety and argument validation
%%% 5. **Dynamic Loading**: Hot-reloadable command modules
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, register_command/2, unregister_command/1,
         lookup_command/1, list_commands/0, execute_command/2,
         get_command_schema/1, reload_commands/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(command_info, {
    name :: binary(),
    module :: module(),
    arity :: integer(),
    description :: binary(),
    category :: binary(),
    safety_level :: low | medium | high,
    metrics :: map(),
    trace_enabled :: boolean(),
    schema :: map() | undefined,
    registered_at :: integer()
}).

-record(registry_state, {
    commands :: map(),          % Command name to command_info
    modules :: set(),           % Loaded modules
    metrics :: map(),           % Registry-level metrics
    reload_timer :: reference() | undefined
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_REGISTRY, #{
    commands => #{},
    modules => sets:new(),
    metrics => #{
        "commands.registered" => 0,
        "commands.unregistered" => 0,
        "commands.lookups" => 0,
        "commands.executed" => 0,
        "errors.total" => 0,
        "errors.lookup" => 0,
        "errors.execute" => 0
    },
    reload_timer => undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the registry
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a new command
-spec register_command(binary(), module()) -> ok | {error, term()}.
register_command(CommandName, Module) when is_binary(CommandName) ->
    gen_server:call(?SERVER, {register_command, CommandName, Module}).

%% @doc Unregister a command
-spec unregister_command(binary()) -> ok | {error, term()}.
unregister_command(CommandName) ->
    gen_server:call(?SERVER, {unregister_command, CommandName}).

%% @doc Look up a command
-spec lookup_command(binary()) -> {ok, #command_info{}} | {error, term()}.
lookup_command(CommandName) ->
    gen_server:call(?SERVER, {lookup_command, CommandName}).

%% @doc List all registered commands
-spec list_commands() -> [{binary(), #command_info{}}].
list_commands() ->
    gen_server:call(?SERVER, list_commands).

%% @doc Execute a command
-spec execute_command(binary(), list()) -> {ok, term()} | {error, term()}.
execute_command(CommandName, Args) ->
    gen_server:call(?SERVER, {execute_command, CommandName, Args}, 30000).

%% @doc Get command schema for validation
-spec get_command_schema(binary()) -> {ok, map()} | {error, term()}.
get_command_schema(CommandName) ->
    gen_server:call(?SERVER, {get_command_schema, CommandName}).

%% @doc Reload all commands (hot-reload)
-spec reload_commands() -> ok.
reload_commands() ->
    gen_server:cast(?SERVER, reload_commands).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the registry
-spec init(list()) -> {ok, #registry_state{}} | {stop, term()}.
init(_Opts) ->
    %% Create OTEL span for registry initialization
    SpanCtx = erlmcp_otel:with_span("cli.registry.init", #{
        <<"registry.init">> => true
    }, fun() ->
        ok
    end),

    %% Initialize registry state
    State = #registry_state{
        commands = ?DEFAULT_REGISTRY#{commands},
        modules = ?DEFAULT_REGISTRY#{modules},
        metrics = ?DEFAULT_REGISTRY#{metrics},
        reload_timer = undefined
    },

    %% Load built-in commands
    case load_builtin_commands() of
        {ok, NewState} ->
            %% Start auto-reload timer
            ReloadTimer = erlang:start_timer(30000, self(), reload_timer),
            FinalState = NewState#registry_state{reload_timer = ReloadTimer},

            %% Record initialization metrics
            erlmcp_metrics:record("cli.registry.initialized", 1),

            {ok, FinalState};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #registry_state{}) ->
    {reply, term(), #registry_state{}} | {stop, term(), #registry_state{}}.
handle_call({register_command, CommandName, Module}, _From, State) ->
    %% Create span for command registration
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.registry.register">>,
                                        make_request_id(),
                                        #{
                                            <<"command.name">> => CommandName,
                                            <<"command.module">> => atom_to_binary(Module)
                                        },
                                        undefined),

    %% Validate command module
    case validate_command_module(Module, CommandName) of
        {ok, CommandInfo} ->
            %% Register command
            Commands = maps:put(CommandName, CommandInfo, State#registry_state.commands),
            Modules = sets:add_element(Module, State#registry_state.modules),

            %% Update metrics
            Metrics = maps:update_with("commands.registered", fun(V) -> V + 1 end,
                                      State#registry_state.metrics),

            %% Record registration event
            erlmcp_otel:add_event(SpanCtx, <<"command.registered">>, #{
                <<"command.name">> => CommandName,
                <<"command.module">> => atom_to_binary(Module)
            }),

            %% Record metrics
            erlmcp_metrics:record("cli.commands.registered", 1),

            Reply = ok,
            NewState = State#registry_state{
                commands = Commands,
                modules = Modules,
                metrics = Metrics
            };

        {error, Reason} ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {registration_failed, Reason}),
            erlmcp_metrics:record("cli.errors.registration", 1),

            Reply = {error, Reason},
            NewState = State
    end,

    {reply, Reply, NewState};

handle_call({unregister_command, CommandName}, _From, State) ->
    %% Create span for command unregistration
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.registry.unregister">>,
                                        make_request_id(),
                                        #{
                                            <<"command.name">> => CommandName
                                        },
                                        undefined),

    case maps:find(CommandName, State#registry_state.commands) of
        {ok, CommandInfo} ->
            %% Find module
            Module = CommandInfo#command_info.module,

            %% Unregister command
            Commands = maps:remove(CommandName, State#registry_state.commands),
            Modules = case sets:size(State#registry_state.modules) > 1 of
                true ->
                    sets:del_element(Module, State#registry_state.modules);
                false ->
                    State#registry_state.modules
            end,

            %% Update metrics
            Metrics = maps:update_with("commands.unregistered", fun(V) -> V + 1 end,
                                      State#registry_state.metrics),

            %% Record unregistration event
            erlmcp_otel:add_event(SpanCtx, <<"command.unregistered">>, #{
                <<"command.name">> => CommandName,
                <<"command.module">> => atom_to_binary(Module)
            }),

            %% Record metrics
            erlmcp_metrics:record("cli.commands.unregistered", 1),

            Reply = ok,
            NewState = State#registry_state{
                commands = Commands,
                modules = Modules,
                metrics = Metrics
        };

        error ->
            Reply = {error, command_not_found},
            NewState = State
    end,

    {reply, Reply, NewState};

handle_call({lookup_command, CommandName}, _From, State) ->
    %% Create span for command lookup
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.registry.lookup">>,
                                        make_request_id(),
                                        #{
                                            <<"command.name">> => CommandName
                                        },
                                        undefined),

    case maps:find(CommandName, State#registry_state.commands) of
        {ok, CommandInfo} ->
            %% Update lookup metrics
            Metrics = maps:update_with("commands.lookups", fun(V) -> V + 1 end,
                                      State#registry_state.metrics),

            RecordCommandInfo = CommandInfo#command_info{
                metrics = update_command_metrics(CommandInfo#command_info.metrics,
                                                 lookup, ok)
            },

            %% Record successful lookup
            erlmcp_otel:add_event(SpanCtx, <<"command.found">>, #{
                <<"command.name">> => CommandName
            }),

            erlmcp_metrics:record("cli.commands.lookups", 1),

            Reply = {ok, RecordCommandInfo},
            NewState = State#registry_state{metrics = Metrics};

        error ->
            %% Update error metrics
            Metrics = maps:update_with("errors.lookup", fun(V) -> V + 1 end,
                                      State#registry_state.metrics),

            %% Record lookup failure
            erlmcp_otel:record_error(SpanCtx, {command_not_found, CommandName}),
            erlmcp_otel:add_event(SpanCtx, <<"command.not_found">>, #{
                <<"command.name">> => CommandName
            }),

            erlmcp_metrics:record("cli.errors.lookup", 1),

            Reply = {error, command_not_found},
            NewState = State#registry_state{metrics = Metrics}
    end,

    {reply, Reply, NewState};

handle_call(list_commands, _From, State) ->
    %% Create span for listing commands
    erlmcp_otel:inject_rpc_span(<<"cli.registry.list">>,
                               make_request_id(),
                               #{},
                               undefined),

    CommandsList = maps:to_list(State#registry_state.commands),
    Reply = {ok, CommandsList},

    {reply, Reply, State};

handle_call({execute_command, CommandName, Args}, _From, State) ->
    %% Create span for command execution
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.registry.execute">>,
                                        make_request_id(),
                                        #{
                                            <<"command.name">> => CommandName,
                                            <<"args.length">> => length(Args)
                                        },
                                        undefined),

    case maps:find(CommandName, State#registry_state.commands) of
        {ok, CommandInfo} ->
            %% Check if tracing is enabled
            TraceCtx = case CommandInfo#command_info.trace_enabled of
                true ->
                    erlmcp_otel:with_span("cli.command", #{
                        <<"command.name">> => CommandName,
                        <<"command.module">> => atom_to_binary(CommandInfo#command_info.module)
                    }, fun() ->
                        SpanCtx
                    end);
                false ->
                    SpanCtx
            end,

            %% Execute command
            try
                Result = CommandInfo#command_info.module:execute(Args),

                %% Record success metrics
                Metrics = update_command_metrics(CommandInfo#command_info.metrics,
                                                 execute, ok),

                UpdatedCommands = maps:put(CommandName,
                                         CommandInfo#command_info{metrics = Metrics},
                                         State#registry_state.commands),

                %% Record execution success
                erlmcp_otel:add_event(TraceCtx, <<"command.success">>, #{
                    <<"command.name">> => CommandName,
                    <<"result.size">> => term_size(Result)
                }),

                erlmcp_metrics:record("cli.commands.executed", 1),
                erlmcp_metrics:record("cli.commands.success", 1),

                Reply = {ok, Result},
                NewState = State#registry_state{commands = UpdatedCommands};

            catch
                Class:Reason:Stacktrace ->
                    %% Record error
                    erlmcp_otel:record_error(TraceCtx, {Class, Reason, Stacktrace}),
                    erlmcp_otel:add_event(TraceCtx, <<"command.failed">>, #{
                        <<"command.name">> => CommandName,
                        <<"error.type">> => atom_to_binary(Class),
                        <<"error.message">> => format_error(Reason)
                    }),

                    %% Update error metrics
                    RegistryMetrics = maps:update_with("errors.execute", fun(V) -> V + 1 end,
                                                      State#registry_state.metrics),
                    CommandMetrics = update_command_metrics(CommandInfo#command_info.metrics,
                                                           execute, {error, Reason}),

                    UpdatedCommands = maps:put(CommandName,
                                             CommandInfo#command_info{metrics = CommandMetrics},
                                             State#registry_state.commands),

                    erlmcp_metrics:record("cli.commands.executed", 1),
                    erlmcp_metrics:record("cli.commands.failed", 1),
                    erlmcp_metrics:record("cli.errors.execute", 1),

                    Reply = {error, {Class, Reason}},
                    NewState = State#registry_state{
                        commands = UpdatedCommands,
                        metrics = RegistryMetrics
                    }
            end;

        error ->
            %% Command not found
            erlmcp_otel:record_error(SpanCtx, {command_not_found, CommandName}),

            %% Update error metrics
            Metrics = maps:update_with("errors.execute", fun(V) -> V + 1 end,
                                      State#registry_state.metrics),

            erlmcp_metrics:record("cli.commands.executed", 1),
            erlmcp_metrics:record("cli.commands.failed", 1),
            erlmcp_metrics:record("cli.errors.execute", 1),

            Reply = {error, command_not_found},
            NewState = State#registry_state{metrics = Metrics}
    end,

    {reply, Reply, NewState};

handle_call({get_command_schema, CommandName}, _From, State) ->
    case maps:find(CommandName, State#registry_state.commands) of
        {ok, CommandInfo} ->
            case CommandInfo#command_info.schema of
                undefined ->
                    %% Generate default schema
                    Schema = generate_command_schema(CommandInfo),
                    Reply = {ok, Schema};
                Schema ->
                    Reply = {ok, Schema}
            end;
        error ->
            Reply = {error, command_not_found}
    end,

    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #registry_state{}) -> {noreply, #registry_state{}} | {stop, term(), #registry_state{}}.
handle_cast(reload_commands, State) ->
    %% Create span for command reload
    erlmcp_otel:inject_rpc_span(<<"cli.registry.reload">>,
                               make_request_id(),
                               #{},
                               undefined),

    case reload_command_modules() of
        {ok, NewState} ->
            %% Record reload success
            erlmcp_metrics:record("cli.commands.reloaded", 1),
            {noreply, NewState};
        {error, Reason} ->
            %% Record reload failure
            erlmcp_metrics:record("cli.commands.reload_failed", 1),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #registry_state{}) -> {noreply, #registry_state{}} | {stop, term(), #registry_state{}}.
handle_info({timeout, Timer, reload_timer}, State) ->
    %% Auto-reload timer expired
    case Timer of
        State#registry_state.reload_timer ->
            %% Reload commands
            case reload_command_modules() of
                {ok, NewState} ->
                    %% Restart timer
                    NewTimer = erlang:start_timer(30000, self(), reload_timer),
                    FinalState = NewState#registry_state{reload_timer = NewTimer},
                    {noreply, FinalState};
                {error, _} ->
                    KeepTimer = erlang:start_timer(30000, self(), reload_timer),
                    FinalState = State#registry_state{reload_timer = KeepTimer},
                    {noreply, FinalState}
            end;
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the registry
-spec terminate(term(), #registry_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Code change
-spec code_change(term(), #registry_state{}, term()) -> {ok, #registry_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Load built-in commands
-spec load_builtin_commands() -> {ok, #registry_state{}} | {error, term()}.
load_builtin_commands() ->
    %% Built-in commands
    BuiltinCommands = [
        {<<"list">>, erlmcp_cli_command_list},
        {<<"help">>, erlmcp_cli_command_help},
        {<<"status">>, erlmcp_cli_command_status},
        {<<"metrics">>, erlmcp_cli_command_metrics},
        {<<"config">>, erlmcp_cli_command_config}
    ],

    %% Load each command
    lists:foldl(fun({CommandName, Module}, {ok, State}) ->
        case validate_command_module(Module, CommandName) of
            {ok, CommandInfo} ->
                Commands = maps:put(CommandName, CommandInfo, State#registry_state.commands),
                Modules = sets:add_element(Module, State#registry_state.modules),
                {ok, State#registry_state{commands = Commands, modules = Modules}};
            {error, Reason} ->
                {error, Reason}
        end
    end, {ok, #registry_state{?DEFAULT_REGISTRY}}, BuiltinCommands).

%% @doc Validate command module
-spec validate_command_module(module(), binary()) -> {ok, #command_info{}} | {error, term()}.
validate_command_module(Module, CommandName) ->
    try
        %% Check if module exists and has execute function
        case erlang:function_exported(Module, execute, 1) of
            true ->
                %% Get command info
                Arity = 1,
                Description = get_command_description(Module),
                Category = get_command_category(Module),
                SafetyLevel = get_command_safety_level(Module),
                TraceEnabled = get_command_trace_enabled(Module),
                Schema = get_command_schema(Module, CommandName),

                %% Create command info
                CommandInfo = #command_info{
                    name = CommandName,
                    module = Module,
                    arity = Arity,
                    description = Description,
                    category = Category,
                    safety_level = SafetyLevel,
                    metrics = #{},
                    trace_enabled = TraceEnabled,
                    schema = Schema,
                    registered_at = erlang:system_time(millisecond)
                },

                {ok, CommandInfo};
            false ->
                {error, missing_execute_function}
        end
    catch
        Error:Reason:Stacktrace ->
            {error, {module_validation_failed, Error, Reason, Stacktrace}}
    end.

%% @doc Reload command modules
-spec reload_command_modules() -> {ok, #registry_state{}} | {error, term()}.
reload_command_modules() ->
    %% Reload all modules
    case code:purge_sets([?MODULE]) of
        [] ->
            case code:load_files([?MODULE]) of
                [] ->
                    {error, reload_failed};
                _ ->
                    %% Reinitialize
                    case load_builtin_commands() of
                        {ok, NewState} ->
                            {ok, NewState};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        _ ->
            {error, purge_failed}
    end.

%% @doc Update command metrics
-spec update_command_metrics(map(), atom(), term()) -> map().
update_command_metrics(Metrics, Operation, Result) ->
    Updated = Metrics,

    %% Update operation count
    OperationKey = case Operation of
        lookup -> "lookups";
        execute -> "executions"
    end,
    Updated1 = maps:update_with(OperationKey, fun(V) -> V + 1 end, 1, Updated),

    %% Update success/failure count
    ResultKey = case Result of
        ok -> "success";
        {error, _} -> "failures"
    end,
    Updated2 = maps:update_with(ResultKey, fun(V) -> V + 1 end, 1, Updated1),

    Updated2.

%% @doc Get command description
-spec get_command_description(module()) -> binary().
get_command_description(Module) ->
    case erlang:function_exported(Module, description, 0) of
        true ->
            Module:description();
        false ->
            <<"No description available">>
    end.

%% @doc Get command category
-spec get_command_category(module()) -> binary().
get_command_category(Module) ->
    case erlang:function_exported(Module, category, 0) of
        true ->
            Module:category();
        false ->
            <<"general">>
    end.

%% @doc Get command safety level
-spec get_command_safety_level(module()) -> low | medium | high.
get_command_safety_level(Module) ->
    case erlang:function_exported(Module, safety_level, 0) of
        true ->
            Module:safety_level();
        false ->
            medium
    end.

%% @doc Get command trace enabled flag
-spec get_command_trace_enabled(module()) -> boolean().
get_command_trace_enabled(Module) ->
    case erlang:function_exported(Module, trace_enabled, 0) of
        true ->
            Module:trace_enabled();
        false ->
            true
    end.

%% @doc Get command schema
-spec get_command_schema(module(), binary()) -> map().
get_command_schema(Module, CommandName) ->
    case erlang:function_exported(Module, schema, 0) of
        true ->
            Module:schema();
        false ->
            generate_command_schema(#command_info{name = CommandName})
    end.

%% @doc Generate default command schema
-spec generate_command_schema(#command_info{}) -> map().
generate_command_schema(CommandInfo) ->
    #{
        name => CommandInfo#command_info.name,
        type => "object",
        properties => #{
            args => #{
                type => "array",
                items => #{
                    type => "string"
                }
            }
        },
        required => ["args"],
        description => CommandInfo#command_info.description
    }.

%% @doc Format error message
-spec format_error(term()) -> binary().
format_error(Error) ->
    list_to_binary(io_lib:format("~p", [Error])).

%% @doc Get term size
-spec term_size(term()) -> integer().
term_size(Term) ->
    size(term_to_binary(Term)).

%% @doc Make request ID
-spec make_request_id() -> binary().
make_request_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).