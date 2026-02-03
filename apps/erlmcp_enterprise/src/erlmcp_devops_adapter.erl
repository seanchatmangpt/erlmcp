%% @doc DevOps Tools Adapter
%% Integrates with Jenkins, GitLab CI for CI/CD pipeline management
-module(erlmcp_devops_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    tool :: jenkins | gitlab_ci,
    config :: map(),
    connection :: pid() | undefined,
    pipelines :: map(),
    builds :: queue:queue(),
    artifacts :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    Tool = maps:get(tool, Config),
    gen_server:start_link({local, devops_name(Tool)}, ?MODULE, [Tool, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([Tool, Config]) ->
    process_flag(trap_exit, true),
    State = #state{tool = Tool, config = Config, pipelines = #{}, builds = queue:new(), artifacts = #{}},

    %% Initialize connection
    case init_devops_connection(Tool, Config) of
        {ok, Connection} ->
            {ok, State#state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(start_pipeline, {From, PipelineId, Parameters}, State) ->
    case start_pipeline(State, PipelineId, Parameters) of
        {ok, BuildId} ->
            Metrics = update_metric(State#state.metrics, pipelines_started, 1),
            {reply, {ok, BuildId}, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_pipeline_status, {From, PipelineId}, State) ->
    case get_pipeline_status(State, PipelineId) of
        {ok, Status} ->
            {reply, {ok, Status}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_build_status, {From, BuildId}, State) ->
    case get_build_status(State, BuildId) of
        {ok, Status} ->
            {reply, {ok, Status}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(cancel_build, {From, BuildId}, State) ->
    case cancel_build(State, BuildId) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_build_log, {From, BuildId}, State) ->
    case get_build_log(State, BuildId) of
        {ok, Log} ->
            {reply, {ok, Log}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_artifacts, {From, BuildId}, State) ->
    case get_artifacts(State, BuildId) of
        {ok, Artifacts} ->
            {reply, {ok, Artifacts}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_pipelines, _From, State) ->
    {reply, {ok, maps:keys(State#state.pipelines)}, State};

handle_call(get_build_history, {From, PipelineId}, State) ->
    case get_build_history(State, PipelineId) of
        {ok, History} ->
            {reply, {ok, History}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(create_webhook, {From, WebhookConfig}, State) ->
    case create_webhook(State, WebhookConfig) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({build_completed, Build}, State) ->
    %% Process completed build
    case process_build(Build) of
        ok ->
            Metrics = update_metric(State#state.metrics, builds_completed, 1),
            Builds = queue:in(Build, State#state.builds),
            Artifacts = maps:put(Build#id, Build#artifacts, State#state.artifacts),
            {noreply, State#state{builds = Builds, artifacts = Artifacts, metrics = Metrics}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to process build: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast({pipeline_status, Status}, State) ->
    %% Update pipeline status
    PipelineId = Status#pipeline_id,
    Pipelines = maps:put(PipelineId, Status, State#state.pipelines),
    {noreply, State#state{pipelines = Pipelines}};

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_devops(State, NewConfig) of
        {ok, NewState} ->
            {noreply, NewState#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconnect: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(build_timeout, State) ->
    %% Handle timed out builds
    case process_timeout_builds(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to process timeout builds: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(connection_lost, Tool) ->
    ?LOG_WARNING("Lost connection to ~p, attempting reconnect", [Tool]),
    case reconnect_devops(State, State#state.config) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Reconnect failed: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.connection of
        undefined -> ok;
        Connection -> erlmcp_devops_connection:close(Connection)
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec devops_name(atom()) -> atom().
devops_name(Tool) ->
    list_to_atom("devops_" ++ atom_to_list(Tool) ++ "_adapter").

-spec init_devops_connection(atom(), map()) -> {ok, pid()} | {error, term()}.
init_devops_connection(jenkins, Config) ->
    erlmcp_jenkins_client:start(Config);
init_devops_connection(gitlab_ci, Config) ->
    erlmcp_gitlab_client:start(Config).

-spec start_pipeline(#state{}, binary(), map()) -> {ok, binary()} | {error, term()}.
start_pipeline(State, PipelineId, Parameters) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.tool of
                jenkins -> erlmcp_jenkins_client:start_pipeline(Connection, PipelineId, Parameters);
                gitlab_ci -> erlmcp_gitlab_client:start_pipeline(Connection, PipelineId, Parameters)
            end
    end.

-spec get_pipeline_status(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_pipeline_status(State, PipelineId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.tool of
                jenkins -> erlmcp_jenkins_client:get_pipeline_status(Connection, PipelineId);
                gitlab_ci -> erlmcp_gitlab_client:get_pipeline_status(Connection, PipelineId)
            end
    end.

-spec get_build_status(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_build_status(State, BuildId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.tool of
                jenkins -> erlmcp_jenkins_client:get_build_status(Connection, BuildId);
                gitlab_ci -> erlmcp_gitlab_client.get_build_status(Connection, BuildId)
            end
    end.

-spec cancel_build(#state{}, binary()) -> ok | {error, term()}.
cancel_build(State, BuildId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.tool of
                jenkins -> erlmcp_jenkins_client:cancel_build(Connection, BuildId);
                gitlab_ci -> erlmcp_gitlab_client:cancel_build(Connection, BuildId)
            end
    end.

-spec get_build_log(#state{}, binary()) -> {ok, binary()} | {error, term()}.
get_build_log(State, BuildId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.tool of
                jenkins -> erlmcp_jenkins_client:get_build_log(Connection, BuildId);
                gitlab_ci -> erlmcp_gitlab_client:get_build_log(Connection, BuildId)
            end
    end.

-spec get_artifacts(#state{}, binary()) -> {ok, [map()]} | {error, term()}.
get_artifacts(State, BuildId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.tool of
                jenkins -> erlmcp_jenkins_client:get_artifacts(Connection, BuildId);
                gitlab_ci -> erlmcp_gitlab_client:get_artifacts(Connection, BuildId)
            end
    end.

-spec get_build_history(#state{}, binary()) -> {ok, [map()]} | {error, term()}.
get_build_history(State, PipelineId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.tool of
                jenkins -> erlmcp_jenkins_client:get_build_history(Connection, PipelineId);
                gitlab_ci -> erlmcp_gitlab_client.get_build_history(Connection, PipelineId)
            end
    end.

-spec create_webhook(#state{}, map()) -> ok | {error, term()}.
create_webhook(State, WebhookConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.tool of
                jenkins -> erlmcp_jenkins_client:create_webhook(Connection, WebhookConfig);
                gitlab_ci -> erlmcp_gitlab_client:create_webhook(Connection, WebhookConfig)
            end
    end.

-spec process_build(map()) -> ok | {error, term()}.
process_build(Build) ->
    %% Process build artifacts and notifications
    case Build#status of
        success ->
            erlmcp_devops_notifier:notify(Build#id, "success", Build#artifacts);
        failure ->
            erlmcp_devops_notifier:notify(Build#id, "failure", Build#artifacts);
        _ ->
            ok
    end.

-spec process_timeout_builds(#state{}) -> {ok, #state{}} | {error, term()}.
process_timeout_builds(State) ->
    %% Remove timeout builds from queue
    Timeout = 3600000, % 1 hour
    CurrentTime = erlang:system_time(millisecond),
    FilteredBuilds = queue:filter(fun(Build) ->
        (CurrentTime - Build#timestamp) < Timeout
    end, State#state.builds),
    {ok, State#state{builds = FilteredBuilds}}.

-spec reconnect_devops(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_devops(State, NewConfig) ->
    case State#state.connection of
        undefined ->
            case init_devops_connection(State#state.tool, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end;
        OldConnection ->
            erlmcp_devops_connection:close(OldConnection),
            case init_devops_connection(State#state.tool, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec update_metric(map(), atom(), integer()) -> map().
update_metric(Metrics, Key, Inc) ->
    Current = maps:get(Key, Metrics, 0),
    Metrics#{Key => Current + Inc}.