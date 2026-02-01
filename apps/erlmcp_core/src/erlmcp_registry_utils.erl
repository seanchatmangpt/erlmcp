-module(erlmcp_registry_utils).

%%%-----------------------------------------------------------------
%%% @doc Registry utilities for race condition prevention
%%%
%%% This module provides synchronization utilities to prevent race
%%% conditions in concurrent registry operations.
%%% @end
%%%-----------------------------------------------------------------
-export([try_register_with_retry/5, ensure_gproc_started/0, clear_test_registrations/0,
         generate_unique_id/1]).

-define(MAX_RETRIES, 5).
-define(RETRY_DELAY_BASE, 10).
-define(REGISTRY_LOCK_TIMEOUT, 5000).

%%--------------------------------------------------------------------
%% @doc Try to register with gproc with retry logic and exponential backoff
%%
%% This prevents race conditions where multiple processes try to register
%% the same key concurrently. Only one will succeed, others retry with
%% unique identifiers.
%% @end
%%--------------------------------------------------------------------
-spec try_register_with_retry(gproc:key() | fun(() -> gproc:key()),
                              pid(),
                              map(),
                              pos_integer(),
                              atom()) ->
                                 {ok, gproc:key()} | {error, term()}.
try_register_with_retry(KeyFun, Pid, Value, MaxRetries, Prefix) when is_function(KeyFun, 0) ->
    try_register_with_retry(KeyFun(), Pid, Value, MaxRetries, Prefix);
try_register_with_retry(Key, Pid, Value, MaxRetries, Prefix) ->
    try_register_with_retry(Key, Pid, Value, MaxRetries, Prefix, 0).

try_register_with_retry(_KeyFun, _Pid, _Value, MaxRetries, _Prefix, Attempt)
    when Attempt >= MaxRetries ->
    {error, {max_retries_exceeded, MaxRetries}};
try_register_with_retry(KeyFun, Pid, Value, MaxRetries, Prefix, Attempt)
    when is_function(KeyFun, 0) ->
    % Generate unique key for each attempt
    UniqueId = generate_unique_id(Prefix),
    Key = KeyFun(UniqueId),
    try_register_with_retry(Key, Pid, Value, MaxRetries, Prefix, Attempt);
try_register_with_retry(Key, Pid, Value, MaxRetries, Prefix, Attempt) ->
    case gproc:where(Key) of
        undefined ->
            try
                gproc:reg_other(Key, Pid, Value),
                gproc:monitor(Key),
                {ok, Key}
            catch
                error:badarg ->
                    % Race condition: another process registered just now
                    logger:debug("Registration race on attempt ~p for key ~p, retrying...",
                                 [Attempt, Key]),
                    RetryDelay = calculate_backoff(Attempt),
                    timer:sleep(RetryDelay),
                    try_register_with_retry(Key, Pid, Value, MaxRetries, Prefix, Attempt + 1)
            end;
        ExistingPid when ExistingPid =:= Pid ->
            % Already registered by same process - this is OK
            {ok, Key};
        ExistingPid ->
            logger:warning("Key ~p already registered by ~p (we are ~p)", [Key, ExistingPid, Pid]),
            {error, {already_registered, ExistingPid}}
    end.

%%--------------------------------------------------------------------
%% @doc Calculate exponential backoff delay
%% @private
%%--------------------------------------------------------------------
-spec calculate_backoff(non_neg_integer()) -> pos_integer().
calculate_backoff(Attempt) ->
    min(?RETRY_DELAY_BASE * trunc(math:pow(2, Attempt)), 1000).

%%--------------------------------------------------------------------
%% @doc Ensure gproc is started, handling already_started case
%% @end
%%--------------------------------------------------------------------
-spec ensure_gproc_started() -> ok.
ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok ->
            ok;
        {error, {already_started, gproc}} ->
            ok;
        {error, Reason} ->
            logger:error("Failed to start gproc: ~p", [Reason]),
            error({gproc_start_failed, Reason})
    end.

%%--------------------------------------------------------------------
%% @doc Clear all test registrations from gproc
%% Use with caution in production!
%% @end
%%--------------------------------------------------------------------
-spec clear_test_registrations() -> ok.
clear_test_registrations() ->
    ok = ensure_gproc_started(),

    % Clear local servers
    ServerPattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    ServerEntries = gproc:select(ServerPattern),
    lists:foreach(fun({Id, Pid}) -> catch gproc:unreg_other({n, l, {mcp, server, Id}}, Pid) end,
                  ServerEntries),

    % Clear local transports
    TransportPattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    TransportEntries = gproc:select(TransportPattern),
    lists:foreach(fun({Id, Pid}) -> catch gproc:unreg_other({n, l, {mcp, transport, Id}}, Pid) end,
                  TransportEntries),

    % Clear global registrations
    GlobalServerPattern = [{{{n, g, {mcp_global, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    GlobalServerEntries = gproc:select(GlobalServerPattern),
    lists:foreach(fun({Id, Pid}) -> catch gproc:unreg_other({n, g, {mcp_global, server, Id}}, Pid)
                  end,
                  GlobalServerEntries),

    GlobalTransportPattern =
        [{{{n, g, {mcp_global, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    GlobalTransportEntries = gproc:select(GlobalTransportPattern),
    lists:foreach(fun({Id, Pid}) ->
                     catch gproc:unreg_other({n, g, {mcp_global, transport, Id}}, Pid)
                  end,
                  GlobalTransportEntries),

    ok.

%%--------------------------------------------------------------------
%% @doc Generate a unique identifier using timestamp and process info
%% @end
%%--------------------------------------------------------------------
-spec generate_unique_id(atom()) -> atom().
generate_unique_id(Prefix) ->
    Timestamp = erlang:system_time(microsecond),
    UniqueInt = erlang:unique_integer([positive, monotonic]),
    list_to_atom(atom_to_list(Prefix)
                 ++ "_"
                 ++ integer_to_list(Timestamp)
                 ++ "_"
                 ++ integer_to_list(UniqueInt)).
