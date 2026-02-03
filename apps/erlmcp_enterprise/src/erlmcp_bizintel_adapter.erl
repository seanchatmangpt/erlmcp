%% @doc Business Intelligence Adapter
%% Integrates with Tableau, Power BI for data visualization
-module(erlmcp_bizintel_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    tool :: tableau | power_bi,
    config :: map(),
    connection :: pid() | undefined,
    cache :: ets:tid() | undefined,
    refresh_tokens :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    Tool = maps:get(tool, Config),
    gen_server:start_link({local, bizintel_name(Tool)}, ?MODULE, [Tool, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([Tool, Config]) ->
    process_flag(trap_exit, true),
    State = #state{tool = Tool, config = Config},

    %% Initialize cache
    Cache = ets:new(bizintel_cache, [set, public, {write_concurrency, true}]),
    RefreshTokens = #{},

    %% Initialize connection
    case init_bizintel_connection(Tool, Config) of
        {ok, Connection} ->
            {ok, State#state{connection = Connection, cache = Cache, refresh_tokens = RefreshTokens}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_workbook, {From, WorkbookId}, State) ->
    case get_workbook_info(State, WorkbookId) of
        {ok, Workbook} ->
            {reply, {ok, Workbook}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_workbooks, _From, State) ->
    case list_workbooks(State) of
        {ok, Workbooks} ->
            {reply, {ok, Workbooks}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_datasources, _From, State) ->
    case list_datasources(State) of
        {ok, Datasources} ->
            {reply, {ok, Datasources}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(extract_data, {From, ExtractConfig}, State) ->
    case extract_data(State, ExtractConfig) of
        {ok, Data} ->
            {reply, {ok, Data}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(publish_report, {From, ReportConfig}, State) ->
    case publish_report(State, ReportConfig) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(create_view, {From, ViewConfig}, State) ->
    case create_view(State, ViewConfig) of
        {ok, ViewId} ->
            {reply, {ok, ViewId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_permissions, {From, ResourceId}, State) ->
    case get_resource_permissions(State, ResourceId) of
        {ok, Permissions} ->
            {reply, {ok, Permissions}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({refresh_token, Token}, State) ->
    RefreshTokens = State#state.refresh_tokens,
    UpdatedTokens = maps:put(self(), Token, RefreshTokens),
    {noreply, State#state{refresh_tokens = UpdatedTokens}};

handle_cast({invalidate_cache, Key}, State) ->
    ets:delete(State#state.cache, Key),
    {noreply, State};

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_bizintel(State, NewConfig) of
        {ok, NewState} ->
            {noreply, NewState#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconnect: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({token_expired, Token}, State) ->
    case refresh_access_token(State, Token) of
        {ok, NewToken} ->
            RefreshTokens = maps:put(self(), NewToken, State#state.refresh_tokens),
            {noreply, State#state{refresh_tokens = RefreshTokens}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to refresh token: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.connection of
        undefined -> ok;
        Connection -> erlmcp_bizintel_connection:close(Connection)
    end,
    case State#state.cache of
        undefined -> ok;
        Cache -> ets:delete(Cache)
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec bizintel_name(atom()) -> atom().
bizintel_name(Tool) ->
    list_to_atom("bizintel_" ++ atom_to_list(Tool) ++ "_adapter").

-spec init_bizintel_connection(atom(), map()) -> {ok, pid()} | {error, term()}.
init_bizintel_connection(tableau, Config) ->
    erlmcp_tableau_client:start(Config);
init_bizintel_connection(power_bi, Config) ->
    erlmcp_powerbi_client:start(Config).

-spec get_workbook_info(#state(), binary()) -> {ok, map()} | {error, term()}.
get_workbook_info(State, WorkbookId) ->
    case State#state.tool of
        tableau -> erlmcp_tableau_client:get_workbook(State#state.connection, WorkbookId);
        power_bi -> erlmcp_powerbi_client:get_report(State#state.connection, WorkbookId)
    end.

-spec list_workbooks(#state{}) -> {ok, [map()]} | {error, term()}.
list_workbooks(State) ->
    case State#state.tool of
        tableau -> erlmcp_tableau_client:list_workbooks(State#state.connection);
        power_bi -> erlmcp_powerbi_client:list_reports(State#state.connection)
    end.

-spec list_datasources(#state{}) -> {ok, [map()]} | {error, term()}.
list_datasources(State) ->
    case State#state.tool of
        tableau -> erlmcp_tableau_client:list_datasources(State#state.connection);
        power_bi -> erlmcp_powerbi_client:list_datasets(State#state.connection)
    end.

-spec extract_data(#state{}, map()) -> {ok, map()} | {error, term()}.
extract_data(State, ExtractConfig) ->
    case State#state.tool of
        tableau -> erlmcp_tableau_client:extract_data(State#state.connection, ExtractConfig);
        power_bi -> erlmcp_powerbi_client:execute_query(State#state.connection, ExtractConfig)
    end.

-spec publish_report(#state{}, map()) -> ok | {error, term()}.
publish_report(State, ReportConfig) ->
    case State#state.tool of
        tableau -> erlmcp_tableau_client:publish_workbook(State#state.connection, ReportConfig);
        power_bi -> erlmcp_powerbi_client:publish_report(State#state.connection, ReportConfig)
    end.

-spec create_view(#state{}, map()) -> {ok, binary()} | {error, term()}.
create_view(State, ViewConfig) ->
    case State#state.tool of
        tableau -> erlmcp_tableau_client:create_view(State#state.connection, ViewConfig);
        power_bi -> erlmcp_powerbi_client:create_visualization(State#state.connection, ViewConfig)
    end.

-spec get_resource_permissions(#state{}, binary()) -> {ok, [map()]} | {error, term()}.
get_resource_permissions(State, ResourceId) ->
    case State#state.tool of
        tableau -> erlmcp_tableau_client:get_permissions(State#state.connection, ResourceId);
        power_bi -> erlmcp_powerbi_client:get_permissions(State#state.connection, ResourceId)
    end.

-spec refresh_access_token(#state{}, binary()) -> {ok, binary()} | {error, term()}.
refresh_access_token(State, Token) ->
    case State#state.tool of
        tableau -> erlmcp_tableau_client:refresh_token(State#state.connection, Token);
        power_bi -> erlmcp_powerbi_client:refresh_token(State#state.connection, Token)
    end.

-spec reconnect_bizintel(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_bizintel(State, NewConfig) ->
    case State#state.connection of
        undefined ->
            case init_bizintel_connection(State#state.tool, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end;
        OldConnection ->
            erlmcp_bizintel_connection:close(OldConnection),
            case init_bizintel_connection(State#state.tool, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.