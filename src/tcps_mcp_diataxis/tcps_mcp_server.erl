%%%-----------------------------------------------------------------------------
%%% @doc TCPS Diataxis MCP Server
%%%
%%% Production-grade MCP server implementing the Toyota Code Production System
%%% (TCPS) Diataxis Documentation Framework simulator.
%%%
%%% This server provides AI tools to interact with a simulated TCPS environment,
%%% allowing exploration and learning of lean software engineering principles
%%% through the Diataxis documentation quadrants:
%%% - Tutorial: Step-by-step learning experiences
%%% - How-to: Task-oriented recipes for specific goals
%%% - Explanation: Understanding-oriented concept clarification
%%% - Reference: Information-oriented technical specifications
%%%
%%% Features:
%%% - Full TCPS simulation with quality gates, Andon, and Kanban
%%% - Diataxis-aligned navigation and learning paths
%%% - Real-time state queries and visualizations
%%% - Production-ready error handling and validation
%%% - Stdio and HTTP transport support
%%% - Comprehensive telemetry and logging
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_mcp_server).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,
    get_server_pid/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal state
-record(state, {
    mcp_server :: pid(),
    server_id :: atom(),
    simulator_state :: map(),
    session_id :: binary(),
    telemetry_enabled :: boolean()
}).

-type state() :: #state{}.

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Start the TCPS Diataxis MCP server with default configuration.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the TCPS Diataxis MCP server with custom configuration.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Stop the TCPS Diataxis MCP server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get the server process ID.
-spec get_server_pid() -> pid() | undefined.
get_server_pid() ->
    whereis(?MODULE).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

%% @private
-spec init(map()) -> {ok, state()}.
init(Config) ->
    process_flag(trap_exit, true),

    ServerId = maps:get(server_id, Config, tcps_diataxis),
    TelemetryEnabled = maps:get(telemetry, Config, true),

    logger:info("Starting TCPS Diataxis MCP server ~p", [ServerId]),

    % Initialize MCP server capabilities
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true},
        logging = #mcp_capability{enabled = false}
    },

    % Start underlying MCP server
    {ok, McpServer} = erlmcp_server:start_link(ServerId, Capabilities),

    % Register all tools
    register_tools(McpServer),

    % Register prompts
    register_prompts(McpServer),

    % Initialize simulator state
    SessionId = generate_session_id(),
    SimulatorState = initialize_simulator_state(SessionId),

    State = #state{
        mcp_server = McpServer,
        server_id = ServerId,
        simulator_state = SimulatorState,
        session_id = SessionId,
        telemetry_enabled = TelemetryEnabled
    },

    logger:info("TCPS Diataxis MCP server ~p started with session ~s",
                [ServerId, SessionId]),

    {ok, State}.

%% @private
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(Reason, #state{server_id = ServerId, session_id = SessionId}) ->
    logger:info("TCPS Diataxis MCP server ~p terminating (session ~s): ~p",
                [ServerId, SessionId, Reason]),
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Tool Registration
%%%=============================================================================

%% @private
%% @doc Register all TCPS Diataxis tools with the MCP server.
-spec register_tools(pid()) -> ok.
register_tools(McpServer) ->
    % Import tool definitions and schemas
    Tools = tcps_mcp_tools:get_all_tools(),

    % Register each tool with its schema and handler
    lists:foreach(fun({Name, Schema, Handler}) ->
        ok = erlmcp_server:add_tool_with_schema(McpServer, Name, Handler, Schema)
    end, Tools),

    logger:info("Registered ~p TCPS Diataxis tools", [length(Tools)]),
    ok.

%%%=============================================================================
%%% Prompt Registration
%%%=============================================================================

%% @private
%% @doc Register all TCPS Diataxis prompts with the MCP server.
-spec register_prompts(pid()) -> ok.
register_prompts(McpServer) ->
    % Import prompt definitions
    Prompts = tcps_mcp_prompts:get_all_prompts(),

    % Register each prompt with its arguments and handler
    lists:foreach(fun({Name, Arguments, Handler}) ->
        case Arguments of
            [] ->
                ok = erlmcp_server:add_prompt(McpServer, Name, Handler);
            Args ->
                ok = erlmcp_server:add_prompt_with_args(McpServer, Name, Handler, Args)
        end
    end, Prompts),

    logger:info("Registered ~p TCPS Diataxis prompts", [length(Prompts)]),
    ok.

%%%=============================================================================
%%% Simulator State Management
%%%=============================================================================

%% @private
%% @doc Initialize the TCPS simulator state.
-spec initialize_simulator_state(binary()) -> map().
initialize_simulator_state(SessionId) ->
    #{
        session_id => SessionId,
        started_at => erlang:timestamp(),
        current_step => 0,
        max_steps => 100,
        status => ready,

        % Diataxis navigation state
        current_quadrant => tutorial,
        learning_path => [],
        completed_tutorials => [],

        % TCPS components
        kanban_state => #{
            reliability => #{wip => 0, limit => 5, items => []},
            security => #{wip => 0, limit => 5, items => []},
            cost => #{wip => 0, limit => 5, items => []},
            compliance => #{wip => 0, limit => 5, items => []}
        },

        quality_gates => #{
            test_pass_rate => 0.0,
            coverage => 0.0,
            shacl_valid => true,
            deterministic => true
        },

        andon_events => [],

        % Metrics
        metrics => #{
            steps_executed => 0,
            quality_gate_passes => 0,
            quality_gate_failures => 0,
            andon_triggers => 0,
            work_orders_created => 0,
            work_orders_completed => 0
        }
    }.

%% @private
%% @doc Generate a unique session ID for the simulator instance.
-spec generate_session_id() -> binary().
generate_session_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(999999),
    list_to_binary(io_lib:format("tcps_sim_~p_~p", [Timestamp, Random])).
