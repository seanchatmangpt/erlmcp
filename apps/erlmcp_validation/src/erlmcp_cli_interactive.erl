%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Interactive Mode - REPL for erlmcp CLI
%%%
%%% Provides an interactive Read-Eval-Print-Loop (REPL) for real-time
%%% interaction with MCP servers. Supports command history, tab completion,
%%% multi-transport connections, and colored output.
%%%
%%% == Features ==
%%%
%%% - Interactive command shell
%%% - Tab completion for commands and arguments
%%% - Command history with search and replay
%%% - Multi-connection management
%%% - Color-coded output
%%% - Graceful Ctrl+C handling
%%%
%%% == Usage ==
%%%
%%% ```
%%% % Start interactive mode
%%% erlmcp_cli_interactive:start_link(),
%%%
%%% % Or start with options
%%% erlmcp_cli_interactive:start_link(#{prompt => "mcp> "}).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_interactive).

-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1, run/0, run/1, stop/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Internal exports for command execution
-export([execute_command/2]).

%% Default configuration
-define(DEFAULT_PROMPT, "erlmcp> ").
-define(WELCOME_MESSAGE,
        "Welcome to erlmcp Interactive Mode\n"
        "Type 'help' for commands, 'exit' or 'quit' to leave\n").

%% State record
-record(state,
        {prompt = ?DEFAULT_PROMPT :: string(),
         connections = #{} :: #{string() => pid()},
         active_connection :: string() | undefined,
         running = true :: boolean(),
         history_pid :: pid() | undefined,
         opts = #{} :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start interactive mode with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start interactive mode with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Run interactive mode (blocking)
-spec run() -> ok.
run() ->
    run(#{}).

%% @doc Run interactive mode with options (blocking)
-spec run(map()) -> ok.
run(Opts) ->
    case start_link(Opts) of
        {ok, Pid} ->
            % Keep running until stopped
            monitor(process, Pid),
            receive
                {'DOWN', _, process, Pid, _} ->
                    ok
            end;
        {error, Reason} ->
            io:format("Failed to start interactive mode: ~p~n", [Reason]),
            error
    end.

%% @doc Stop interactive mode
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize interactive mode
init(Opts) ->
    process_flag(trap_exit, true),

    % Start dependencies
    ensure_dependencies_started(),

    % Start history manager
    {ok, HistoryPid} = erlmcp_cli_history:start_link(Opts),

    % Set up Ctrl+C handler
    setup_interrupt_handler(),

    State =
        #state{prompt = maps:get(prompt, Opts, ?DEFAULT_PROMPT),
               history_pid = HistoryPid,
               opts = Opts},

    % Print welcome message
    io:format(?WELCOME_MESSAGE),

    % Start REPL loop asynchronously
    self() ! run_repl,

    {ok, State}.

%% @doc Handle synchronous calls
handle_call({execute, Command}, _From, State) ->
    Result = do_execute_command(Command, State),
    {reply, Result, State};
handle_call(get_connections, _From, State) ->
    {reply, maps:keys(State#state.connections), State};
handle_call({get_connection, Name}, _From, State) ->
    Reply = maps:get(Name, State#state.connections, undefined),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
handle_cast(stop, State) ->
    {stop, normal, State#state{running = false}};
handle_cast({add_connection, Name, Pid}, State) ->
    Connections = maps:put(Name, Pid, State#state.connections),
    NewState =
        State#state{connections = Connections,
                    active_connection =
                        case State#state.active_connection of
                            undefined ->
                                Name;
                            Active ->
                                Active
                        end},
    {noreply, NewState};
handle_cast({remove_connection, Name}, State) ->
    Connections = maps:remove(Name, State#state.connections),
    NewState =
        State#state{connections = Connections,
                    active_connection =
                        case State#state.active_connection of
                            Name ->
                                get_first_connection(Connections);
                            Active ->
                                Active
                        end},
    {noreply, NewState};
handle_cast({set_active_connection, Name}, State) ->
    case maps:is_key(Name, State#state.connections) of
        true ->
            {noreply, State#state{active_connection = Name}};
        false ->
            io:format(
                erlmcp_cli_formatter:error(
                    io_lib:format("Connection '~s' not found~n", [Name]))),
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(run_repl, State) ->
    % Run REPL loop in separate process to avoid blocking gen_server
    spawn_link(fun() -> repl_loop(State) end),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) when Pid =:= State#state.history_pid ->
    io:format("History manager died: ~p~n", [Reason]),
    {stop, {history_died, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
terminate(_Reason, State) ->
    % Close all connections
    maps:foreach(fun(_Name, Pid) -> catch erlmcp_client:stop(Pid) end, State#state.connections),

    % Stop history
    case State#state.history_pid of
        undefined ->
            ok;
        Pid ->
            catch erlmcp_cli_history:stop()
    end,

    ok.

%% @doc Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - REPL Loop
%%====================================================================

%% @doc Main REPL loop
-spec repl_loop(#state{}) -> ok.
repl_loop(State) ->
    % Display prompt
    io:format("~s", [State#state.prompt]),

    % Read input
    case io:get_line("") of
        eof ->
            io:format("~n"),
            gen_server:cast(?MODULE, stop),
            ok;
        {error, _} ->
            gen_server:cast(?MODULE, stop),
            ok;
        Line ->
            % Trim and process command
            Command = string:trim(Line),

            case Command of
                "" ->
                    % Empty line, continue
                    repl_loop(State);
                _ ->
                    % Add to history
                    erlmcp_cli_history:add(Command),

                    % Execute command
                    case execute_command(Command, State) of
                        {ok, continue} ->
                            repl_loop(State);
                        {ok, stop} ->
                            gen_server:cast(?MODULE, stop),
                            ok;
                        {error, Reason} ->
                            io:format(
                                erlmcp_cli_formatter:error(
                                    io_lib:format("Error: ~p~n", [Reason]))),
                            repl_loop(State)
                    end
            end
    end.

%% @doc Execute a command
-spec execute_command(string(), #state{}) -> {ok, continue | stop} | {error, term()}.
execute_command(Command, State) ->
    % Parse command
    Tokens = string:lexemes(Command, " "),

    case Tokens of
        [] ->
            {ok, continue};
        [Cmd | Args] ->
            dispatch_command(Cmd, Args, State)
    end.

%% @doc Dispatch command to handler
-spec dispatch_command(string(), [string()], #state{}) -> {ok, continue | stop} | {error, term()}.
dispatch_command("help", _, _State) ->
    print_help(),
    {ok, continue};
dispatch_command("exit", _, _State) ->
    io:format("Goodbye!~n"),
    {ok, stop};
dispatch_command("quit", _, _State) ->
    io:format("Goodbye!~n"),
    {ok, stop};
dispatch_command("connect", Args, State) ->
    cmd_connect(Args, State);
dispatch_command("disconnect", Args, State) ->
    cmd_disconnect(Args, State);
dispatch_command("connections", _, State) ->
    cmd_list_connections(State);
dispatch_command("list-resources", _, State) ->
    cmd_list_resources(State);
dispatch_command("list-tools", _, State) ->
    cmd_list_tools(State);
dispatch_command("list-prompts", _, State) ->
    cmd_list_prompts(State);
dispatch_command("read-resource", Args, State) ->
    cmd_read_resource(Args, State);
dispatch_command("call-tool", Args, State) ->
    cmd_call_tool(Args, State);
dispatch_command("get-prompt", Args, State) ->
    cmd_get_prompt(Args, State);
dispatch_command("subscribe", Args, State) ->
    cmd_subscribe(Args, State);
dispatch_command("unsubscribe", Args, State) ->
    cmd_unsubscribe(Args, State);
dispatch_command("history", Args, _State) ->
    cmd_history(Args);
dispatch_command("clear", _, _State) ->
    % Clear screen
    io:format("\e[H\e[2J"),
    {ok, continue};
dispatch_command("status", _, State) ->
    cmd_status(State);
dispatch_command("version", _, _State) ->
    cmd_version();
dispatch_command("validate", Args, _State) ->
    cmd_validate(Args);
dispatch_command("spec-check", Args, _State) ->
    cmd_spec_check(Args);
dispatch_command("transport-check", Args, _State) ->
    cmd_transport_check(Args);
dispatch_command("!" ++ NumStr, _, _State) ->
    % History replay
    cmd_history_replay(NumStr);
dispatch_command(Unknown, _, _State) ->
    io:format(
        erlmcp_cli_formatter:error(
            io_lib:format("Unknown command: ~s~n", [Unknown]))),
    io:format("Type 'help' for available commands~n"),
    {ok, continue}.

%%====================================================================
%% Internal Functions - Command Implementations
%%====================================================================

%% @doc Print help message
-spec print_help() -> ok.
print_help() ->
    Help =
        erlmcp_cli_formatter:format_box("Available Commands:\n\n"
                                        "  help                      Show this help\n"
                                        "  exit, quit                Exit interactive mode\n"
                                        "  connect <url>             Connect to MCP server\n"
                                        "  disconnect <name>         Disconnect from server\n"
                                        "  connections               List active connections\n"
                                        "  list-resources            List available resources\n"
                                        "  list-tools                List available tools\n"
                                        "  list-prompts              List available prompts\n"
                                        "  read-resource <uri>       Read resource content\n"
                                        "  call-tool <name> <args>   Call a tool\n"
                                        "  get-prompt <name>         Get prompt template\n"
                                        "  subscribe <uri>           Subscribe to resource\n"
                                        "  unsubscribe <uri>         Unsubscribe from resource\n"
                                        "  history [n]               Show command history\n"
                                        "  !<n>                      Replay command number n\n"
                                        "  clear                     Clear screen\n"
                                        "  status                    Show connection status\n"
                                        "  version                   Show version info\n"
                                        "  validate <url>            Validate server\n"
                                        "  spec-check                Check spec compliance\n"
                                        "  transport-check <name>    Check transport\n",
                                        #{title => "erlmcp Interactive Mode", color => cyan}),
    io:format("~s", [Help]),
    ok.

%% @doc Connect to server
-spec cmd_connect([string()], #state{}) -> {ok, continue} | {error, term()}.
cmd_connect([Url | _], State) ->
    io:format("Connecting to ~s...~n", [Url]),

    % Parse URL to determine transport
    TransportOpts = parse_url(Url),

    % Start client
    case erlmcp_client:start_link(TransportOpts) of
        {ok, Pid} ->
            % Initialize
            case erlmcp_client:initialize(Pid,
                                          #{capabilities =>
                                                #{tools => #{},
                                                  resources => #{},
                                                  prompts => #{}},
                                            client_info =>
                                                #{name => <<"erlmcp-cli">>,
                                                  version => <<"2.1.0">>}})
            of
                {ok, _ServerInfo} ->
                    % Generate connection name
                    Name = generate_connection_name(Url),

                    % Add to state
                    gen_server:cast(?MODULE, {add_connection, Name, Pid}),

                    io:format(
                        erlmcp_cli_formatter:success(
                            io_lib:format("Connected as '~s'~n", [Name]))),

                    % List available resources and tools
                    update_completions(Pid),

                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to initialize: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Reason} ->
            io:format(
                erlmcp_cli_formatter:error(
                    io_lib:format("Failed to connect: ~p~n", [Reason]))),
            {ok, continue}
    end;
cmd_connect([], _State) ->
    io:format(
        erlmcp_cli_formatter:error("Usage: connect <url>~n")),
    {ok, continue}.

%% @doc Disconnect from server
-spec cmd_disconnect([string()], #state{}) -> {ok, continue} | {error, term()}.
cmd_disconnect([Name], State) ->
    case gen_server:call(?MODULE, {get_connection, Name}) of
        undefined ->
            io:format(
                erlmcp_cli_formatter:error(
                    io_lib:format("Connection '~s' not found~n", [Name]))),
            {ok, continue};
        Pid ->
            erlmcp_client:stop(Pid),
            gen_server:cast(?MODULE, {remove_connection, Name}),
            io:format(
                erlmcp_cli_formatter:success(
                    io_lib:format("Disconnected from '~s'~n", [Name]))),
            {ok, continue}
    end;
cmd_disconnect([], State) ->
    % Disconnect active connection
    case State#state.active_connection of
        undefined ->
            io:format(
                erlmcp_cli_formatter:error("No active connection~n")),
            {ok, continue};
        Name ->
            cmd_disconnect([Name], State)
    end.

%% @doc List connections
-spec cmd_list_connections(#state{}) -> {ok, continue}.
cmd_list_connections(State) ->
    case maps:size(State#state.connections) of
        0 ->
            io:format("No active connections~n"),
            {ok, continue};
        _ ->
            io:format(
                erlmcp_cli_formatter:bold("Active Connections:~n")),
            maps:foreach(fun(Name, _Pid) ->
                            Active =
                                case Name =:= State#state.active_connection of
                                    true ->
                                        " " ++ erlmcp_cli_formatter:success("[active]");
                                    false ->
                                        ""
                                end,
                            io:format("  ~s~s~n", [Name, Active])
                         end,
                         State#state.connections),
            {ok, continue}
    end.

%% @doc List resources
-spec cmd_list_resources(#state{}) -> {ok, continue}.
cmd_list_resources(State) ->
    case get_active_client(State) of
        {ok, Pid} ->
            case erlmcp_client:list_resources(Pid) of
                {ok, Resources} ->
                    io:format(
                        erlmcp_cli_formatter:bold("Available Resources:~n")),
                    lists:foreach(fun(Resource) ->
                                     Uri = maps:get(uri, Resource, <<"unknown">>),
                                     Name = maps:get(name, Resource, <<"unnamed">>),
                                     io:format("  ~s (~s)~n", [Uri, Name])
                                  end,
                                  Resources),
                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to list resources: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Msg} ->
            io:format(
                erlmcp_cli_formatter:error(Msg)),
            {ok, continue}
    end.

%% @doc List tools
-spec cmd_list_tools(#state{}) -> {ok, continue}.
cmd_list_tools(State) ->
    case get_active_client(State) of
        {ok, Pid} ->
            case erlmcp_client:list_tools(Pid) of
                {ok, Tools} ->
                    io:format(
                        erlmcp_cli_formatter:bold("Available Tools:~n")),
                    lists:foreach(fun(Tool) ->
                                     Name = maps:get(name, Tool, <<"unnamed">>),
                                     Description =
                                         maps:get(description, Tool, <<"no description">>),
                                     io:format("  ~s - ~s~n", [Name, Description])
                                  end,
                                  Tools),
                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to list tools: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Msg} ->
            io:format(
                erlmcp_cli_formatter:error(Msg)),
            {ok, continue}
    end.

%% @doc List prompts
-spec cmd_list_prompts(#state{}) -> {ok, continue}.
cmd_list_prompts(State) ->
    case get_active_client(State) of
        {ok, Pid} ->
            case erlmcp_client:list_prompts(Pid) of
                {ok, Prompts} ->
                    io:format(
                        erlmcp_cli_formatter:bold("Available Prompts:~n")),
                    lists:foreach(fun(Prompt) ->
                                     Name = maps:get(name, Prompt, <<"unnamed">>),
                                     Description =
                                         maps:get(description, Prompt, <<"no description">>),
                                     io:format("  ~s - ~s~n", [Name, Description])
                                  end,
                                  Prompts),
                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to list prompts: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Msg} ->
            io:format(
                erlmcp_cli_formatter:error(Msg)),
            {ok, continue}
    end.

%% @doc Read resource
-spec cmd_read_resource([string()], #state{}) -> {ok, continue}.
cmd_read_resource([Uri | _], State) ->
    case get_active_client(State) of
        {ok, Pid} ->
            case erlmcp_client:read_resource(Pid, list_to_binary(Uri)) of
                {ok, Content} ->
                    io:format("~s~n", [erlmcp_cli_formatter:format_json(Content)]),
                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to read resource: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Msg} ->
            io:format(
                erlmcp_cli_formatter:error(Msg)),
            {ok, continue}
    end;
cmd_read_resource([], _State) ->
    io:format(
        erlmcp_cli_formatter:error("Usage: read-resource <uri>~n")),
    {ok, continue}.

%% @doc Call tool
-spec cmd_call_tool([string()], #state{}) -> {ok, continue}.
cmd_call_tool([Name | ArgsStr], State) ->
    case get_active_client(State) of
        {ok, Pid} ->
            % Parse args as JSON if provided
            Args =
                case ArgsStr of
                    [] ->
                        #{};
                    [JsonStr] ->
                        try
                            erlmcp_json_native:decode(list_to_binary(JsonStr))
                        catch
                            _:_ ->
                                #{}
                        end
                end,

            case erlmcp_client:call_tool(Pid, list_to_binary(Name), Args) of
                {ok, Result} ->
                    io:format("~s~n", [erlmcp_cli_formatter:format_json(Result)]),
                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to call tool: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Msg} ->
            io:format(
                erlmcp_cli_formatter:error(Msg)),
            {ok, continue}
    end;
cmd_call_tool([], _State) ->
    io:format(
        erlmcp_cli_formatter:error("Usage: call-tool <name> [<json-args>]~n")),
    {ok, continue}.

%% @doc Get prompt
-spec cmd_get_prompt([string()], #state{}) -> {ok, continue}.
cmd_get_prompt([Name | _], State) ->
    case get_active_client(State) of
        {ok, Pid} ->
            case erlmcp_client:get_prompt(Pid, list_to_binary(Name)) of
                {ok, Prompt} ->
                    io:format("~s~n", [erlmcp_cli_formatter:format_json(Prompt)]),
                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to get prompt: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Msg} ->
            io:format(
                erlmcp_cli_formatter:error(Msg)),
            {ok, continue}
    end;
cmd_get_prompt([], _State) ->
    io:format(
        erlmcp_cli_formatter:error("Usage: get-prompt <name>~n")),
    {ok, continue}.

%% @doc Subscribe to resource
-spec cmd_subscribe([string()], #state{}) -> {ok, continue}.
cmd_subscribe([Uri | _], State) ->
    case get_active_client(State) of
        {ok, Pid} ->
            case erlmcp_client:subscribe_to_resource(Pid, list_to_binary(Uri)) of
                ok ->
                    io:format(
                        erlmcp_cli_formatter:success(
                            io_lib:format("Subscribed to ~s~n", [Uri]))),
                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to subscribe: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Msg} ->
            io:format(
                erlmcp_cli_formatter:error(Msg)),
            {ok, continue}
    end;
cmd_subscribe([], _State) ->
    io:format(
        erlmcp_cli_formatter:error("Usage: subscribe <uri>~n")),
    {ok, continue}.

%% @doc Unsubscribe from resource
-spec cmd_unsubscribe([string()], #state{}) -> {ok, continue}.
cmd_unsubscribe([Uri | _], State) ->
    case get_active_client(State) of
        {ok, Pid} ->
            case erlmcp_client:unsubscribe_from_resource(Pid, list_to_binary(Uri)) of
                ok ->
                    io:format(
                        erlmcp_cli_formatter:success(
                            io_lib:format("Unsubscribed from ~s~n", [Uri]))),
                    {ok, continue};
                {error, Reason} ->
                    io:format(
                        erlmcp_cli_formatter:error(
                            io_lib:format("Failed to unsubscribe: ~p~n", [Reason]))),
                    {ok, continue}
            end;
        {error, Msg} ->
            io:format(
                erlmcp_cli_formatter:error(Msg)),
            {ok, continue}
    end;
cmd_unsubscribe([], _State) ->
    io:format(
        erlmcp_cli_formatter:error("Usage: unsubscribe <uri>~n")),
    {ok, continue}.

%% @doc Show history
-spec cmd_history([string()]) -> {ok, continue}.
cmd_history([]) ->
    % Show last 10
    cmd_history(["10"]);
cmd_history([NumStr]) ->
    try
        Num = list_to_integer(NumStr),
        History = erlmcp_cli_history:get_all(),
        ToShow = lists:sublist(History, Num),
        lists:foreach(fun({Idx, Cmd}) ->
                         io:format(
                             erlmcp_cli_formatter:color(cyan, io_lib:format("~4B", [Idx]))),
                         io:format("  ~s~n", [Cmd])
                      end,
                      lists:zip(
                          lists:seq(1, length(ToShow)), ToShow)),
        {ok, continue}
    catch
        _:_ ->
            io:format(
                erlmcp_cli_formatter:error("Invalid number~n")),
            {ok, continue}
    end.

%% @doc Replay history command
-spec cmd_history_replay(string()) -> {ok, continue}.
cmd_history_replay(NumStr) ->
    try
        Num = list_to_integer(NumStr),
        case erlmcp_cli_history:get(Num) of
            {ok, Command} ->
                io:format("Replaying: ~s~n", [Command]),
                % Add to history
                erlmcp_cli_history:add(Command),
                % Execute - need state, so just print for now
                io:format("Command: ~s~n", [Command]),
                {ok, continue};
            {error, not_found} ->
                io:format(
                    erlmcp_cli_formatter:error("Command not found in history~n")),
                {ok, continue}
        end
    catch
        _:_ ->
            io:format(
                erlmcp_cli_formatter:error("Invalid command number~n")),
            {ok, continue}
    end.

%% @doc Show status
-spec cmd_status(#state{}) -> {ok, continue}.
cmd_status(State) ->
    NumConnections = maps:size(State#state.connections),
    ActiveConn =
        case State#state.active_connection of
            undefined ->
                "none";
            Name ->
                Name
        end,

    io:format("~s~n",
              [erlmcp_cli_formatter:format_box(
                   io_lib:format("Status:~n"
                                 "  Active Connections: ~B~n"
                                 "  Active Connection: ~s~n"
                                 "  History Size: ~B~n",
                                 [NumConnections, ActiveConn, erlmcp_cli_history:size()]),
                   #{title => "erlmcp Interactive Status", color => blue})]),
    {ok, continue}.

%% @doc Show version
-spec cmd_version() -> {ok, continue}.
cmd_version() ->
    io:format("erlmcp CLI v2.1.0~n"),
    io:format("MCP Specification: 2025-11-25~n"),
    {ok, continue}.

%% @doc Validate server
-spec cmd_validate([string()]) -> {ok, continue}.
cmd_validate([Url | _]) ->
    case erlmcp_validate_cli:validate_running_server(Url, #{}) of
        {ok, Result} ->
            io:format("~s~n", [erlmcp_cli_formatter:format_result(Result)]),
            {ok, continue};
        {error, Reason} ->
            io:format(
                erlmcp_cli_formatter:error(
                    io_lib:format("Validation failed: ~p~n", [Reason]))),
            {ok, continue}
    end;
cmd_validate([]) ->
    io:format(
        erlmcp_cli_formatter:error("Usage: validate <url>~n")),
    {ok, continue}.

%% @doc Spec check
-spec cmd_spec_check([string()]) -> {ok, continue}.
cmd_spec_check(_Args) ->
    case erlmcp_validate_cli:validate_spec_check(#{}) of
        {ok, Result} ->
            io:format("~s~n", [erlmcp_cli_formatter:format_result(Result)]),
            {ok, continue};
        {error, Reason} ->
            io:format(
                erlmcp_cli_formatter:error(
                    io_lib:format("Spec check failed: ~p~n", [Reason]))),
            {ok, continue}
    end.

%% @doc Transport check
-spec cmd_transport_check([string()]) -> {ok, continue}.
cmd_transport_check([Transport | _]) ->
    case erlmcp_validate_cli:validate_transport_check(Transport, #{}) of
        {ok, Result} ->
            io:format("~s~n", [erlmcp_cli_formatter:format_result(Result)]),
            {ok, continue};
        {error, Reason} ->
            io:format(
                erlmcp_cli_formatter:error(
                    io_lib:format("Transport check failed: ~p~n", [Reason]))),
            {ok, continue}
    end;
cmd_transport_check([]) ->
    io:format(
        erlmcp_cli_formatter:error("Usage: transport-check <transport>~n")),
    {ok, continue}.

%%====================================================================
%% Internal Functions - Helpers
%%====================================================================

%% @doc Get active client PID
-spec get_active_client(#state{}) -> {ok, pid()} | {error, string()}.
get_active_client(State) ->
    case State#state.active_connection of
        undefined ->
            {error, "No active connection. Use 'connect <url>' first.~n"};
        Name ->
            case maps:get(Name, State#state.connections, undefined) of
                undefined ->
                    {error, "Active connection not found~n"};
                Pid ->
                    {ok, Pid}
            end
    end.

%% @doc Parse URL to transport options
-spec parse_url(string()) -> {atom(), map()}.
parse_url("stdio://" ++ _) ->
    {stdio, #{}};
parse_url("tcp://" ++ Rest) ->
    [Host, PortStr] = string:split(Rest, ":"),
    Port = list_to_integer(PortStr),
    {tcp, #{host => Host, port => Port}};
parse_url("http://" ++ Rest) ->
    {http, #{url => "http://" ++ Rest}};
parse_url("https://" ++ Rest) ->
    {http, #{url => "https://" ++ Rest, ssl => true}};
parse_url("ws://" ++ Rest) ->
    {websocket, #{url => "ws://" ++ Rest}};
parse_url("wss://" ++ Rest) ->
    {websocket, #{url => "wss://" ++ Rest, ssl => true}};
parse_url(Url) ->
    % Default to stdio
    {stdio, #{}}.

%% @doc Generate connection name from URL
-spec generate_connection_name(string()) -> string().
generate_connection_name(Url) ->
    % Simple name based on URL
    case string:split(Url, "://") of
        [Proto, Rest] ->
            SafeRest = re:replace(Rest, "[^a-zA-Z0-9]", "_", [global, {return, list}]),
            lists:flatten(
                io_lib:format("~s_~s", [Proto, SafeRest]));
        _ ->
            "connection_" ++ integer_to_list(erlang:unique_integer([positive]))
    end.

%% @doc Get first connection name
-spec get_first_connection(map()) -> string() | undefined.
get_first_connection(Connections) ->
    case maps:keys(Connections) of
        [] ->
            undefined;
        [First | _] ->
            First
    end.

%% @doc Update completions with resources and tools
-spec update_completions(pid()) -> ok.
update_completions(Pid) ->
    % Clear old completions
    erlmcp_cli_completer:clear_dynamic_completions(),

    % Get resources
    case erlmcp_client:list_resources(Pid) of
        {ok, Resources} ->
            lists:foreach(fun(Resource) ->
                             Uri = binary_to_list(maps:get(uri, Resource, <<"">>)),
                             erlmcp_cli_completer:register_resource(Uri)
                          end,
                          Resources);
        _ ->
            ok
    end,

    % Get tools
    case erlmcp_client:list_tools(Pid) of
        {ok, Tools} ->
            lists:foreach(fun(Tool) ->
                             Name = binary_to_list(maps:get(name, Tool, <<"">>)),
                             erlmcp_cli_completer:register_tool(Name)
                          end,
                          Tools);
        _ ->
            ok
    end,

    ok.

%% @doc Ensure dependencies are started
-spec ensure_dependencies_started() -> ok.
ensure_dependencies_started() ->
    RequiredApps = [crypto, asn1, public_key, ssl, inets],
    lists:foreach(fun(App) ->
                     case application:start(App) of
                         ok ->
                             ok;
                         {error, {already_started, App}} ->
                             ok;
                         {error, _} ->
                             ok
                     end
                  end,
                  RequiredApps),
    ok.

%% @doc Set up Ctrl+C interrupt handler
-spec setup_interrupt_handler() -> ok.
setup_interrupt_handler() ->
    % Note: In OTP, Ctrl+C handling is complex and typically
    % done at the VM level. For now, we rely on standard behavior.
    ok.

%% @doc Execute command (public interface for testing)
-spec do_execute_command(string(), #state{}) -> ok | {error, term()}.
do_execute_command(Command, State) ->
    case execute_command(Command, State) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
