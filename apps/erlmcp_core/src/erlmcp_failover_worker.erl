%%%====================================================================
%%% @doc Session Failover Worker
%%%
%%% Supervised gen_server for async failover operations (replication, notifications).
%%% Replaces unsupervised spawn/1 calls with proper OTP supervision.
%%%
%%% Supervision: Managed by erlmcp_failover_worker_sup
%%% Strategy: simple_one_for_one - one worker per operation
%%%
%%% @end
%%%====================================================================
-module(erlmcp_failover_worker).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type work_type() ::
    {replicate, SessionId :: binary(), FailoverState :: map(), BackupNode :: node()} |
    {notify, SessionId :: binary(), NewPrimary :: node(), BackupNode :: node()}.

-record(state, {work :: work_type()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a failover worker
-spec start_link(work_type()) -> {ok, pid()} | {error, term()}.
start_link(Work) ->
    gen_server:start_link(?MODULE, [Work], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Work]) ->
    %% Trigger async work immediately
    gen_server:cast(self(), execute_work),
    {ok, #state{work = Work}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(execute_work, State) ->
    case State#state.work of
        {replicate, SessionId, FailoverState, BackupNode} ->
            execute_replication(SessionId, FailoverState, BackupNode);
        {notify, SessionId, NewPrimary, BackupNode} ->
            execute_notification(SessionId, NewPrimary, BackupNode)
    end,
    %% Stop worker after work completion
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Execute session replication to backup node
execute_replication(SessionId, _FailoverState, BackupNode) ->
    try
        %% Retrieve session data from Mnesia
        TableName = erlmcp_session,
        case mnesia:transaction(fun() -> mnesia:read(TableName, SessionId) end) of
            {atomic, [SessionRec]} ->
                %% Extract session data from record
                SessionData = element(3, SessionRec),  % Assuming 3rd field is session_data

                %% Replicate to backup node via RPC
                case rpc:call(BackupNode,
                              erlmcp_session_mnesia,
                              store,
                              [SessionId, SessionData, #{table_name => TableName}])
                of
                    {ok, _State} ->
                        logger:debug("Replicated session ~s to backup ~p", [SessionId, BackupNode]),
                        ok;
                    ok ->
                        logger:debug("Replicated session ~s to backup ~p", [SessionId, BackupNode]),
                        ok;
                    {error, Reason} ->
                        logger:error("Failed to replicate session ~s to backup ~p: ~p",
                                     [SessionId, BackupNode, Reason]),
                        {error, Reason};
                    {badrpc, Reason} ->
                        logger:error("RPC failed replicating session ~s to backup ~p: ~p",
                                     [SessionId, BackupNode, Reason]),
                        {error, {badrpc, Reason}}
                end;
            {atomic, []} ->
                logger:warning("Session ~s not found for replication", [SessionId]),
                {error, session_not_found};
            {aborted, Reason} ->
                logger:error("Mnesia transaction failed for session ~s replication: ~p",
                             [SessionId, Reason]),
                {error, {mnesia_aborted, Reason}}
        end
    catch
        Type:Error:Stacktrace ->
            logger:error("Exception replicating session ~s: ~p:~p~n~p",
                         [SessionId, Type, Error, Stacktrace]),
            {error, {exception, {Type, Error}}}
    end.

%% @doc Execute failover notification to backup node
execute_notification(SessionId, NewPrimary, BackupNode) ->
    try
        case rpc:call(BackupNode,
                      erlmcp_session_failover,
                      notify_failover_local,
                      [SessionId, NewPrimary])
        of
            ok ->
                logger:info("Notified backup ~p of failover for session ~s",
                            [BackupNode, SessionId]),
                ok;
            {error, Reason} ->
                logger:error("Failed to notify backup ~p of failover: ~p", [BackupNode, Reason]),
                {error, Reason};
            {badrpc, Reason} ->
                logger:error("RPC failed notifying backup ~p: ~p", [BackupNode, Reason]),
                {error, {badrpc, Reason}}
        end
    catch
        Type:Error:Stacktrace ->
            logger:error("Exception notifying backup ~p: ~p:~p~n~p",
                         [BackupNode, Type, Error, Stacktrace]),
            {error, {exception, {Type, Error}}}
    end.
