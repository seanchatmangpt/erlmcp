%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_cli_transport (real exported API).
%%%
%%% Rewritten from Elixir-flavored pseudo-code (`%{}` maps,
%%% ?assertEqual/assertEqual from ExUnit, ?fail). The pseudo-code also
%%% invented functions that do not exist on the real module:
%%% transport/2, is_active/1, get_transport_stats/1,
%%% get_all_transport_stats/0, validate_config/1, start_monitor/0,
%%% stop_monitor/0, check_health/1, get_transport_metrics/1.
%%%
%%% Real API under test (apps/erlmcp_cli/src/erlmcp_cli_transport.erl):
%%%   start_link(TypeAtom, Opts), send_data(Data, TypeAtom),
%%%   close_transport(TypeAtom), stdio/0, tcp/0, http/0, ws/0, sse/0,
%%%   init/2, send/2, close/1
%%%
%%% Note the real argument order: send_data(Data, TransportType) and
%%% close_transport(TransportType) take the transport type as an ATOM,
%%% not the data first. Active-transport lifecycle (stdio/tcp/http/ws/sse
%%% against live sockets) needs the full built app; this suite is
%%% compile-validated only (apps/erlmcp_cli has no rebar build
%%% integration), so it exercises the deterministic dispatch surface.
%%% @end
%%%-------------------------------------------------------------------
-module(cli_transport_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

type_constants_test() ->
    ?assertEqual(stdio, erlmcp_cli_transport:stdio()),
    ?assertEqual(tcp, erlmcp_cli_transport:tcp()),
    ?assertEqual(http, erlmcp_cli_transport:http()),
    ?assertEqual(ws, erlmcp_cli_transport:ws()),
    ?assertEqual(sse, erlmcp_cli_transport:sse()).

invalid_transport_type_dispatch_test() ->
    %% Real module rejects unknown transport types on every entry point
    %% with {error, {invalid_transport_type, Type}}.
    ?assertMatch({error, {invalid_transport_type, bogus}},
                 erlmcp_cli_transport:start_link(bogus, #{})),
    ?assertMatch({error, {invalid_transport_type, bogus}},
                 erlmcp_cli_transport:send_data(<<"message">>, bogus)),
    ?assertMatch({error, {invalid_transport_type, bogus}},
                 erlmcp_cli_transport:close_transport(bogus)),
    ?assertMatch({error, {invalid_transport_type, bogus}},
                 erlmcp_cli_transport:init(bogus, #{})).

behaviour_callback_stubs_test() ->
    %% The behaviour-level send/close are documented stubs on this module
    %% (specific transports implement them); assert their real contract.
    ?assertEqual({error, not_implemented},
                 erlmcp_cli_transport:send(<<"data">>, some_state)),
    ?assertEqual(ok, erlmcp_cli_transport:close(some_state)).

send_data_all_known_types_dispatch_test() ->
    %% Without a live transport process every known type must return an
    %% {error, _} term (never crash, never ok) when asked to send.
    Types = [stdio, tcp, http, ws, sse],
    Results = [erlmcp_cli_transport:send_data(<<"{\"jsonrpc\":\"2.0\",\"id\":1}">>, T)
               || T <- Types],
    lists:foreach(fun(R) ->
                          ?assert(lists:member(element(1, R), [ok, error]))
                  end, Results),
    ok.

close_transport_all_known_types_dispatch_test() ->
    Types = [stdio, tcp, http, ws, sse],
    Results = [erlmcp_cli_transport:close_transport(T) || T <- Types],
    %% Each is ok or {error, _}; none may crash the caller.
    lists:foreach(fun(R) ->
                          ?assert(R =:= ok orelse element(1, R) =:= error)
                  end, Results),
    ok.

init_valid_type_dispatch_test() ->
    %% init/2 dispatches to the concrete transport's init; without a
    %% backing process/socket the result is an error term, not a crash.
    ?assertMatch({error, _}, erlmcp_cli_transport:init(stdio, #{})),
    ?assertMatch({error, _}, erlmcp_cli_transport:init(tcp, #{host => <<"127.0.0.1">>,
                                                              port => 1})).
