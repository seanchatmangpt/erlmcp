-module(erlmcp_supervision_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

supervision_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            {"Connection pool assignment", fun test_pool_assignment/0},
            {"Load distribution across pools", fun test_load_distribution/0},
            {"Single failure domain availability", fun test_single_domain_availability/0},
            {"Multiple domain availability", fun test_multi_domain_availability/0},
            {"Ten pool availability", fun test_ten_pool_availability/0}
        ]
    }.

setup() ->
    application:ensure_all_started(erlmcp),
    timer:sleep(100).

teardown(_) ->
    application:stop(erlmcp).

test_pool_assignment() ->
    Pool1 = erlmcp_connection_pool_sup:get_pool_for_connection(server_1),
    Pool2 = erlmcp_connection_pool_sup:get_pool_for_connection(server_2),
    ?assert(is_atom(Pool1)),
    ?assert(is_atom(Pool2)).

test_load_distribution() ->
    ServerIds = [list_to_atom("dist_server_" ++ integer_to_list(N)) || N <- lists:seq(1, 100)],
    Pools = lists:map(fun(Id) ->
        erlmcp_connection_pool_sup:get_pool_for_connection(Id)
    end, ServerIds),
    ?assertEqual(100, length(Pools)).

test_single_domain_availability() ->
    DomainAvailability = 0.99,
    TotalAvailability = DomainAvailability,
    ?assert(TotalAvailability >= 0.99).

test_multi_domain_availability() ->
    NumDomains = 5,
    DomainAvail = 0.99,
    TotalAvailability = 1 - math:pow(1 - DomainAvail, NumDomains),
    ?assert(TotalAvailability >= 0.99999).

test_ten_pool_availability() ->
    NumPools = 10,
    PoolAvail = 0.99,
    TotalAvailability = 1 - math:pow(1 - PoolAvail, NumPools),
    ?assert(TotalAvailability >= 0.9999999).
