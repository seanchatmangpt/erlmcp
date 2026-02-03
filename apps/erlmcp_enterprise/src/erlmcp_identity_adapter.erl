%% @doc Enterprise Identity Provider Adapter
%% Integrates with Okta, Azure AD, ADFS for authentication
-module(erlmcp_identity_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    provider :: okta | azure_ad | adfs,
    config :: map(),
    connection :: pid() | undefined,
    cache :: ets:tid() | undefined
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    Provider = maps:get(provider, Config),
    gen_server:start_link({local, identity_name(Provider)}, ?MODULE, [Provider, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([Provider, Config]) ->
    process_flag(trap_exit, true),
    State = #state{provider = Provider, config = Config},

    %% Initialize cache
    Cache = ets:new(identity_cache, [set, public, {write_concurrency, true}]),

    %% Initialize provider connection
    case init_provider_connection(Provider, Config) of
        {ok, Connection} ->
            {ok, State#state{connection = Connection, cache = Cache}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(authenticate, {From, _}, State) ->
    Token = get_token(From),
    case authenticate_user(State, Token) of
        {ok, User} ->
            {reply, {ok, User}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_user, {From, UserId}, State) ->
    case get_user_info(State, UserId) of
        {ok, User} ->
            {reply, {ok, User}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_groups, {From, UserId}, State) ->
    case get_user_groups(State, UserId) of
        {ok, Groups} ->
            {reply, {ok, Groups}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_applications, _From, State) ->
    case get_applications(State) of
        {ok, Apps} ->
            {reply, {ok, Apps}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(refresh_token, {From, RefreshToken}, State) ->
    case refresh_access_token(State, RefreshToken) of
        {ok, NewToken} ->
            {reply, {ok, NewToken}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({invalidate_cache, Key}, State) ->
    ets:delete(State#state.cache, Key),
    {noreply, State};

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_provider(State, NewConfig) of
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
            ets:insert(State#state.cache, {token, NewToken}),
            {noreply, State};
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
        Connection -> erlmcp_identity_connection:close(Connection)
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

-spec identity_name(atom()) -> atom().
identity_name(Provider) ->
    list_to_atom("identity_" ++ atom_to_list(Provider) ++ "_adapter").

-spec init_provider_connection(atom(), map()) -> {ok, pid()} | {error, term()}.
init_provider_connection(okta, Config) ->
    erlmcp_okta_client:start(Config);
init_provider_connection(azure_ad, Config) ->
    erlmcp_azure_client:start(Config);
init_provider_connection(adfs, Config) ->
    erlmcp_adfs_client:start(Config).

-spec authenticate_user(#state{}, binary()) -> {ok, map()} | {error, term()}.
authenticate_user(State, Token) ->
    %% Check cache first
    case ets:lookup(State#state.cache, Token) of
        [{Token, User}] ->
            {ok, User};
        _ ->
            case State#state.provider of
                okta -> authenticate_okta(State, Token);
                azure_ad -> authenticate_azure(State, Token);
                adfs -> authenticate_adfs(State, Token)
            end
    end.

-spec get_user_info(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_user_info(State, UserId) ->
    case State#state.provider of
        okta -> erlmcp_okta_client:get_user(State#state.connection, UserId);
        azure_ad -> erlmcp_azure_client:get_user(State#state.connection, UserId);
        adfs -> erlmcp_adfs_client:get_user(State#state.connection, UserId)
    end.

-spec get_user_groups(#state{}, binary()) -> {ok, [map()]} | {error, term()}.
get_user_groups(State, UserId) ->
    case State#state.provider of
        okta -> erlmcp_okta_client:get_groups(State#state.connection, UserId);
        azure_ad -> erlmcp_azure_client:get_groups(State#state.connection, UserId);
        adfs -> erlmcp_adfs_client:get_groups(State#state.connection, UserId)
    end.

-spec get_applications(#state{}) -> {ok, [map()]} | {error, term()}.
get_applications(State) ->
    case State#state.provider of
        okta -> erlmcp_okta_client:get_applications(State#state.connection);
        azure_ad -> erlmcp_azure_client:get_applications(State#state.connection);
        adfs -> erlmcp_adfs_client:get_applications(State#state.connection)
    end.

-spec refresh_access_token(#state{}, binary()) -> {ok, binary()} | {error, term()}.
refresh_access_token(State, RefreshToken) ->
    case State#state.provider of
        okta -> erlmcp_okta_client:refresh_token(State#state.connection, RefreshToken);
        azure_ad -> erlmcp_azure_client:refresh_token(State#state.connection, RefreshToken);
        adfs -> erlmcp_adfs_client:refresh_token(State#state.connection, RefreshToken)
    end.

-spec reconnect_provider(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_provider(State, NewConfig) ->
    case State#state.connection of
        undefined ->
            case init_provider_connection(State#state.provider, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end;
        OldConnection ->
            erlmcp_identity_connection:close(OldConnection),
            case init_provider_connection(State#state.provider, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec get_token(pid()) -> binary().
get_pid_token(Pid) ->
    %% Extract token from process metadata or context
    case process_info(Pid, dictionary) of
        undefined -> <<>>;
        Dict ->
            case proplists:get_value(erlmcp_token, Dict) of
                undefined -> <<>>;
                Token -> Token
            end
    end.