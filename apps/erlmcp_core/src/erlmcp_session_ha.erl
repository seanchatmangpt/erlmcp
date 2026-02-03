%% @doc High Availability Session Management for erlmcp
%% Implements session replication and failover across regions
-module(erlmcp_session_ha).
-behaviour(gen_server).

%% API
-export([start_link/0, start_session/2, replicate_session/2,
         failover_session/2, get_session/1, get_active_region/1,
         store_session/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

-record(session_info, {
    session_id :: binary(),
    primary_node :: node(),
    secondary_nodes :: [node()],
    session_data :: map(),
    last_updated :: integer(),
    replication_status :: 'synced' | 'pending' | 'failed' | 'syncing'
}).

-record(state, {
    session_store :: ets:tid(),
    region :: 'primary' | 'secondary' | 'tertiary',
    replication_timeout :: pos_integer(),
    max_retry_attempts :: pos_integer()
}).

-define(TAB, erlmcp_session_ha).
-define(REPLICATION_TIMEOUT, 5000). % 5 seconds
-define(MAX_RETRY_ATTEMPTS, 3).
-define(SESSION_CLEANUP_INTERVAL, 300000). % 5 minutes

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_session(ClientId, SessionData) ->
    gen_server:call(?MODULE, {start_session, ClientId, SessionData}).

replicate_session(SessionId, SessionData) ->
    gen_server:call(?MODULE, {replicate_session, SessionId, SessionData}).

failover_session(SessionId, NewPrimaryNode) ->
    gen_server:call(?MODULE, {failover_session, SessionId, NewPrimaryNode}).

get_session(SessionId) ->
    gen_server:call(?MODULE, {get_session, SessionId}).

get_active_region(SessionId) ->
    gen_server:call(?MODULE, {get_active_region, SessionId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS table for session storage
    SessionStore = ets:new(?TAB, [
        set,
        public,
        named_table,
        {keypos, #session_info.session_id}
    ]),

    %% Determine current region
    Region = get_current_region(),

    State = #state{
        session_store = SessionStore,
        region = Region,
        replication_timeout = ?REPLICATION_TIMEOUT,
        max_retry_attempts = ?MAX_RETRY_ATTEMPTS
    },

    %% Start session cleanup process
    spawn_session_cleanup(State),

    %% Start replication monitor
    spawn_replication_monitor(State),

    {ok, State}.

handle_call({start_session, _ClientId, SessionData}, _From, State) ->
    SessionId = generate_session_id(),
    PrimaryNode = get_primary_node(),
    SecondaryNodes = get_secondary_nodes(),

    Session = #session_info{
        session_id = SessionId,
        primary_node = PrimaryNode,
        secondary_nodes = SecondaryNodes,
        session_data = SessionData,
        last_updated = erlang:system_time(millisecond),
        replication_status = pending
    },

    %% Store locally
    true = ets:insert(State#state.session_store, Session),

    %% Start replication process
    replicate_session_async(SessionId, Session),

    {reply, {ok, SessionId}, State};

handle_call({replicate_session, SessionId, SessionData}, _From, State) ->
    case get_session_internal(SessionId, State) of
        {ok, Session} ->
            %% Update session data
            UpdatedSession = Session#session_info{
                session_data = SessionData,
                last_updated = erlang:system_time(millisecond),
                replication_status = syncing
            },

            %% Store updated session
            true = ets:insert(State#state.session_store, UpdatedSession),

            %% Replicate to secondary nodes
            ReplicationResult = replicate_to_secondaries(UpdatedSession, State),

            case ReplicationResult of
                ok ->
                    %% Update status to synced
                    FinalSession = UpdatedSession#session_info{
                        replication_status = synced
                    },
                    true = ets:insert(State#state.session_store, FinalSession),
                    {reply, ok, State};
                {error, Reason} ->
                    logger:error("Session replication failed for ~p: ~p", [SessionId, Reason]),
                    {reply, {error, Reason}, State}
            end;
        {error, not_found} ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({failover_session, SessionId, NewPrimaryNode}, _From, State) ->
    case get_session_internal(SessionId, State) of
        {ok, Session} ->
            %% Create new session info with new primary
            NewSession = Session#session_info{
                primary_node = NewPrimaryNode,
                replication_status = pending
            },

            %% Store on new primary
            case rpc:call(NewPrimaryNode, erlmcp_session_ha, store_session, [NewSession]) of
                ok ->
                    %% Replicate to other nodes
                    NewSecondaryNodes = [N || N <- Session#session_info.secondary_nodes, N =/= NewPrimaryNode],

                    ReplicationResults = [
                        begin
                            case rpc:call(Node, erlmcp_session_ha, store_session, [NewSession]) of
                                ok -> ok;
                                Error -> {node_error, Node, Error}
                            end
                        end || Node <- NewSecondaryNodes
                    ],

                    %% Check if replication succeeded
                    FailedNodes = [R || R <- ReplicationResults, R =/= ok],
                    case FailedNodes of
                        [] ->
                            %% Update local session
                            FinalSession = NewSession#session_info{
                                replication_status = synced
                            },
                            true = ets:insert(State#state.session_store, FinalSession),

                            {reply, ok, State};
                        _ ->
                            logger:error("Session failover had failures: ~p", [FailedNodes]),
                            {reply, {error, replication_failed}, State}
                    end;
                Error ->
                    logger:error("Failed to store session on new primary ~p: ~p", [NewPrimaryNode, Error]),
                    {reply, {error, new_primary_store_failed}, State}
            end;
        {error, not_found} ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({get_session, SessionId}, _From, State) ->
    case get_session_internal(SessionId, State) of
        {ok, Session} ->
            {reply, {ok, Session}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_active_region, SessionId}, _From, State) ->
    case get_session_internal(SessionId, State) of
        {ok, Session} ->
            %% Check if primary node is available
            case is_node_healthy(Session#session_info.primary_node) of
                true ->
                    {reply, {ok, primary}, State};
                false ->
                    %% Find first healthy secondary node
                    HealthySecondaries = [N || N <- Session#session_info.secondary_nodes, is_node_healthy(N)],
                    case HealthySecondaries of
                        [Node | _] ->
                            {reply, {ok, node_to_region(Node)}, State};
                        [] ->
                            {reply, {error, no_active_nodes}, State}
                    end
            end;
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(session_cleanup, State) ->
    cleanup_expired_sessions(State),
    spawn_session_cleanup(State),
    {noreply, State};

handle_info(replication_monitor, State) ->
    check_replication_status(State),
    spawn_replication_monitor(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Cleanup ETS table (named table, delete by name)
    ets:delete(?TAB),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

get_session_internal(SessionId, State) ->
    case ets:lookup(State#state.session_store, SessionId) of
        [Session] -> {ok, Session};
        [] -> {error, not_found}
    end.

generate_session_id() ->
    %% Generate unique session ID
    crypto:strong_rand_bytes(16).

get_current_region() ->
    %% Determine current node's region
    case application:get_env(erlmcp, region) of
        undefined -> primary;
        Region -> Region
    end.

get_primary_node() ->
    %% Get the primary node for the current region
    case get_current_region() of
        primary -> 'us-east-1-node1@10.0.1.10';
        secondary -> 'eu-west-1-node1@10.0.2.10';
        tertiary -> 'ap-southeast-1-node1@10.0.3.10'
    end.

get_secondary_nodes() ->
    %% Get secondary nodes for replication
    case get_current_region() of
        primary -> ['eu-west-1-node1@10.0.2.10', 'eu-west-1-node2@10.0.2.11', 'eu-west-1-node3@10.0.2.12'];
        secondary -> ['us-east-1-node1@10.0.1.10', 'us-east-1-node2@10.0.1.11', 'us-east-1-node3@10.0.1.12'];
        tertiary -> ['us-east-1-node1@10.0.1.10', 'eu-west-1-node1@10.0.2.10']
    end.

replicate_session_async(SessionId, Session) ->
    %% Start asynchronous replication
    spawn(fun() ->
        try
            ReplicationResult = replicate_to_secondaries(Session, #state{}),
            case ReplicationResult of
                ok ->
                    FinalSession = Session#session_info{replication_status = synced},
                    true = ets:insert(?TAB, FinalSession);
                {error, Reason} ->
                    logger:error("Async replication failed for ~p: ~p", [SessionId, Reason]),
                    FinalSession = Session#session_info{replication_status = failed},
                    true = ets:insert(?TAB, FinalSession)
            end
        catch
            Kind:Payload ->
                logger:error("Async replication crashed for ~p: ~p:~p", [SessionId, Kind, Payload])
        end
    end),
    ok.

replicate_to_secondaries(Session, State) ->
    %% Replicate session to all secondary nodes
    SecondaryNodes = Session#session_info.secondary_nodes,

    Results = [
        begin
            case rpc:call(Node, erlmcp_session_ha, store_session, [Session], State#state.replication_timeout) of
                ok -> ok;
                Error -> {error, {Node, Error}}
            end
        end || Node <- SecondaryNodes
    ],

    %% Check if all replications succeeded
    Failed = [R || R <- Results, R =/= ok],
    case Failed of
        [] -> ok;
        _ -> {error, Failed}
    end.

store_session(Session) ->
    true = ets:insert(?TAB, Session),
    ok.

is_node_healthy(Node) ->
    %% Check if node is reachable and responsive
    case net_adm:ping(Node) of
        pong -> true;
        _ -> false
    end.

node_to_region(Node) ->
    %% Convert node name to region
    case Node of
        _ when Node =:= 'us-east-1-node1@10.0.1.10'; Node =:= 'us-east-1-node2@10.0.1.11'; Node =:= 'us-east-1-node3@10.0.1.12' ->
            primary;
        _ when Node =:= 'eu-west-1-node1@10.0.2.10'; Node =:= 'eu-west-1-node2@10.0.2.11'; Node =:= 'eu-west-1-node3@10.0.2.12' ->
            secondary;
        _ when Node =:= 'ap-southeast-1-node1@10.0.3.10'; Node =:= 'ap-southeast-1-node2@10.0.3.11' ->
            tertiary
    end.

spawn_session_cleanup(_State) ->
    spawn(fun() ->
        timer:sleep(?SESSION_CLEANUP_INTERVAL),
        gen_server:cast(?MODULE, session_cleanup)
    end).

spawn_replication_monitor(_State) ->
    spawn(fun() ->
        timer:sleep(?SESSION_CLEANUP_INTERVAL),
        gen_server:cast(?MODULE, replication_monitor)
    end).

cleanup_expired_sessions(State) ->
    %% Remove sessions that haven't been updated in 24 hours
    Now = erlang:system_time(millisecond),
    ExpiryTime = 24 * 60 * 60 * 1000, % 24 hours

    SessionsToCleanup = ets:foldl(
        fun(Session, Acc) when Session#session_info.last_updated < Now - ExpiryTime ->
            [Session | Acc];
        (_, Acc) ->
            Acc
        end,
        [],
        State#state.session_store
    ),

    %% Clean up expired sessions
    lists:foreach(fun(Session) ->
        true = ets:delete(State#state.session_store, Session#session_info.session_id)
    end, SessionsToCleanup).

check_replication_status(State) ->
    %% Check sessions with pending replication status
    SessionsToCheck = ets:match_object(State#state.session_store,
        #session_info{replication_status = pending, _ = '_'}),

    lists:foreach(fun(Session) ->
        RetryCount = get_retry_count(Session#session_info.session_id),

        case RetryCount < ?MAX_RETRY_ATTEMPTS of
            true ->
                %% Retry replication
                replicate_to_secondaries(Session, State),
                increment_retry_count(Session#session_info.session_id);
            false ->
                %% Mark as failed
                logger:error("Max retries reached for session ~p", [Session#session_info.session_id]),
                FinalSession = Session#session_info{replication_status = failed},
                true = ets:insert(State#state.session_store, FinalSession)
        end
    end, SessionsToCheck).

get_retry_count(SessionId) ->
    case ets:lookup(?TAB, {retry_count, SessionId}) of
        [{_, Count}] -> Count;
        [] -> 0
    end.

increment_retry_count(SessionId) ->
    CurrentCount = get_retry_count(SessionId),
    true = ets:insert(?TAB, {{retry_count, SessionId}, CurrentCount + 1}).