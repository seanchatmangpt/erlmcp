%%%====================================================================
%%% Enhanced code_change/3 for erlmcp_rate_limiter
%%%====================================================================
%%% Replace the existing code_change/3 in erlmcp_rate_limiter.erl
%%% with this implementation after adding the version field to state record
%%%====================================================================

%% State version
-type state_version() :: v1 | v2.

%% Update state record definition:
%% -record(state, {
%%     version = v1 :: state_version(),
%%     config :: #{atom() => any()},
%%     clients :: ets:table(),
%%     global_bucket :: token_bucket(),
%%     violations :: ets:table(),
%%     last_cleanup :: integer(),
%%     %% Reserved for future expansion
%%     _v2_reserved = undefined :: term()
%% }).

%% @doc Enhanced code_change/3 with proper migration logic
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(OldVsn, State, Extra) ->
    try
        logger:info("Rate limiter: Code change from ~p", [OldVsn]),
        NewState = migrate_rate_limiter_state(OldVsn, State, Extra),
        logger:info("Rate limiter: Code change completed successfully"),
        {ok, NewState}
    catch
        Class:Reason:Stack ->
            logger:error("Rate limiter: Code change failed: ~p:~p~n~p",
                        [Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.

%% @private Migrate rate limiter state based on version
-spec migrate_rate_limiter_state(term(), #state{}, term()) -> #state{}.
migrate_rate_limiter_state(_OldVsn, #state{version = v1} = State, _Extra) ->
    %% Already at current version (v1)
    State;
migrate_rate_limiter_state({down, _FromVsn}, #state{} = State, _Extra) ->
    %% Downgrade migration - ensure version field exists
    case State#state.version of
        undefined -> State#state{version = v1};
        _ -> State
    end;
migrate_rate_limiter_state(OldVsn, #state{version = undefined} = State, _Extra)
  when is_list(OldVsn); is_atom(OldVsn) ->
    %% Legacy state (pre-versioning) - upgrade to v1
    logger:info("Rate limiter: Upgrading legacy state to v1"),
    migrate_v0_to_v1(State);
migrate_rate_limiter_state(OldVsn, State, _Extra) ->
    logger:warning("Rate limiter: Unknown code_change from version ~p", [OldVsn]),
    State.

%% @private Migrate v0 (legacy) state to v1
%% This handles states created before versioning was added
-spec migrate_v0_to_v1(#state{}) -> #state{}.
migrate_v0_to_v1(#state{} = State) ->
    %% Add version field, preserve all other fields
    State#state{version = v1}.

%% @private Future migration: v1 to v2
%% This would be called when upgrading to v2
%% Uncomment and implement when v2 is needed
%% -spec migrate_v1_to_v2(#state{}) -> #state{}.
%% migrate_v1_to_v2(#state{version = v1} = State) ->
%%     %% Transform v1 state to v2
%%     %% Example: Add new fields, transform data structures
%%     NewState = State#state{
%%         version = v2,
%%         _v2_reserved = some_new_value
%%     },
%%     %% Migrate ETS tables if schema changed
%%     migrate_clients_ets_v1_to_v2(State#state.clients),
%%     NewState.

%% @private Future ETS table migration for clients table
%% -spec migrate_clients_ets_v1_to_v2(ets:tid()) -> ok.
%% migrate_clients_ets_v1_to_v2(Table) ->
%%     %% Transform client state records in ETS table
%%     TransformFun = fun({ClientId, ClientState}) ->
%%         %% Transform ClientState from v1 to v2 format
%%         NewClientState = transform_client_state_v1_to_v2(ClientState),
%%         {ClientId, NewClientState}
%%     end,
%%     erlmcp_state_migration:migrate_ets_table(
%%         Table, ?MODULE, v1, v2, TransformFun).

%% @private Future client state transformation
%% -spec transform_client_state_v1_to_v2(client_state()) -> client_state_v2().
%% transform_client_state_v1_to_v2(ClientStateV1) ->
%%     %% Add v2 fields, transform existing fields
%%     ClientStateV1#{
%%         new_v2_field => default_value
%%     }.
