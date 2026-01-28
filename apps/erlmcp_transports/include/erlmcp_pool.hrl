%%%====================================================================
%%% erlmcp_pool.hrl - Shared Pool Definitions
%%%====================================================================

-ifndef(ERLMCP_POOL_HRL).
-define(ERLMCP_POOL_HRL, true).

-record(connection, {
    pid :: pid(),
    monitor_ref :: reference(),
    created_at :: integer(),
    last_used :: integer(),
    health_status = healthy :: healthy | unhealthy,
    failure_count = 0 :: non_neg_integer(),
    request_count = 0 :: non_neg_integer()
}).

-endif.
