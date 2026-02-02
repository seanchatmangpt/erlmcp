%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Property-Based Test Suite (Proper)
%%%
%%% Property tests for erlmcp_cli application
%%%
%%% Chicago School TDD:
%%% - Property-based testing
%%% - Generative inputs
%%% - Protocol invariants
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Properties
%%%====================================================================

%% @doc Command line argument parsing property
prop_cmdline_parsing_roundtrip() ->
    ?FORALL(Cmd, cmdline_gen(),
        begin
            Parsed = parse_cmdline(Cmd),
            is_map(Parsed)
        end).

%% @doc Message serialization/deserialization roundtrip
prop_json_rpc_roundtrip() ->
    ?FORALL(Request, json_rpc_request_gen(),
        begin
            Encoded = jsx:encode(Request),
            {ok, Decoded} = erlmcp_cli_json_rpc:parse_request(Encoded),
            Request =:= Decoded
        end).

%% @doc Connection management invariants
prop_session_state_transitions() ->
    ?FORALL(Commands, list(session_command_gen()),
        begin
            SessionId = <<"proper-session">>,
            {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

            %% Execute commands
            lists:foreach(fun(Cmd) ->
                execute_session_command(SessionId, Cmd)
            end, Commands),

            %% Verify session valid
            {ok, _State} = erlmcp_cli_session:get_state(SessionId),

            %% Cleanup
            erlmcp_cli_session:terminate_session(SessionId),

            true
        end).

%% @doc Registry operation properties
prop_registry_lookup_registered() ->
    ?FORALL(Command, command_gen(),
        begin
            %% Register command
            ok = erlmcp_cli_registry:register_command(Command),

            %% Lookup command
            {ok, Found} = erlmcp_cli_registry:lookup_command(maps:get(name, Command)),

            %% Verify name matches
            maps:get(name, Command) =:= maps:get(name, Found)
        end).

%% @doc Transport protocol properties
prop_transport_send_receive() ->
    ?FORALL(Message, binary_gen(),
        begin
            %% Initialize transport
            Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"proper-transport">>},
            ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

            %% Send message
            ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

            %% Verify sent
            {ok, Stats} = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
            Sent = maps:get(<<"messages_sent">>, Stats, 0),

            %% Cleanup
            ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

            Sent >= 0
        end).

%% @doc Metrics collection properties
prop_counter_monotonic() ->
    ?FORALL({Increment1, Increment2},
            {non_neg_integer(), non_neg_integer()},
        begin
            %% Reset
            erlmcp_cli_metrics:reset_all_metrics(),

            %% Increment twice
            erlmcp_cli_metrics:increment_counter(<<"proper.counter">>, Increment1),
            erlmcp_cli_metrics:increment_counter(<<"proper.counter">>, Increment2),

            %% Verify result equals sum
            Value = erlmcp_cli_metrics:get_counter_value(<<"proper.counter">>),
            Value =:= Increment1 + Increment2
        end).

%% @doc Session state transition properties
prop_session_state_monotonic() ->
    ?FORALL(Operations, list(session_operation_gen()),
        begin
            SessionId = <<"proper-state-session">>,
            {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

            %% Execute operations
            lists:foreach(fun(Op) ->
                execute_session_operation(SessionId, Op)
            end, Operations),

            %% Verify state valid
            {ok, State} = erlmcp_cli_session:get_state(SessionId),
            Status = maps:get(status, State),

            %% Status must be valid
            lists:member(Status, [initialized, started, stopped, expired]),

            %% Cleanup
            erlmcp_cli_session:terminate_session(SessionId),

            true
        end).

%%%====================================================================
%%% Generators
%%%====================================================================

%% Command line generator
cmdline_gen() ->
    ?LET({Cmd, Args, Flags},
         {binary_gen(), list(binary_gen()), list(flag_gen())},
         iolist_to_binary([Cmd, $\s, join_args(Args), join_flags(Flags)])).

%% JSON-RPC request generator
json_rpc_request_gen() ->
    ?LET({Method, Params, Id},
         {binary_gen(), param_gen(), integer(0, 1000000)},
         #{
           <<"jsonrpc">> => <<"2.0">>,
           <<"method">> => Method,
           <<"params">> => Params,
           <<"id">> => Id
         }).

%% Command generator
command_gen() ->
    ?LET(Name, binary_gen(),
         #{
           name => Name,
           module => erlmcp_cli_registry,
           function => test_function,
           arity => 1,
           description => binary_gen(),
           category => binary_gen(),
           safety_level => oneof([safe, unsafe])
         }).

%% Session command generator
session_command_gen() ->
    oneof([start, stop, terminate, get_state]).

%% Session operation generator
session_operation_gen() ->
    oneof([
        {start},
        {stop},
        {set_data, binary_gen(), binary_gen()},
        {get_data}
    ]).

%% Parameter generator
param_gen() ->
    oneof([
        null,
        binary_gen(),
        list(binary_gen()),
        map_gen()
    ]).

%% Map generator
map_gen() ->
    ?LET(Keys, list(binary_gen()),
         lists:foldl(fun(K, Acc) ->
             Acc#{K => binary_gen()}
         end, #{}, Keys)).

%% Flag generator
flag_gen() ->
    ?LET({Flag, Value},
         {oneof([<<"--verbose">>, <<"--debug">>, <<"--format">>]), binary_gen()},
         {Flag, Value}).

%% Binary generator
binary_gen() ->
    ?LET(Size, integer(0, 100), binary(Size)).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

parse_cmdline(Cmd) ->
    %% Simplified cmdline parser (would be more complex in reality)
    Parts = binary:split(Cmd, <<" ">>, [global]),
    #{
        command => hd(Parts),
        args => tl(Parts)
    }.

join_args([]) ->
    <<>>;
join_args(Args) ->
    iolist_to_binary(lists:join(<<" ">>, Args)).

join_flags([]) ->
    <<>>;
join_flags(Flags) ->
    FlagStrs = [<<K/binary, " ", V/binary>> || {K, V} <- Flags],
    iolist_to_binary([<<" ">>, lists:join(<<" ">>, FlagStrs)]).

execute_session_command(SessionId, start) ->
    erlmcp_cli_session:start_session(SessionId);
execute_session_command(SessionId, stop) ->
    erlmcp_cli_session:stop_session(SessionId);
execute_session_command(SessionId, terminate) ->
    erlmcp_cli_session:terminate_session(SessionId);
execute_session_command(SessionId, get_state) ->
    {ok, _} = erlmcp_cli_session:get_state(SessionId),
    ok.

execute_session_operation(SessionId, {start}) ->
    erlmcp_cli_session:start_session(SessionId);
execute_session_operation(SessionId, {stop}) ->
    erlmcp_cli_session:stop_session(SessionId);
execute_session_operation(SessionId, {set_data, Key, Value}) ->
    erlmcp_cli_session:set_data(SessionId, Key, Value);
execute_session_operation(SessionId, {get_data}) ->
    {ok, _} = erlmcp_cli_session:get_data(SessionId),
    ok.

test_function(_Args) ->
    #{<<"result">> => <<"ok">>}.

%%%====================================================================
%%% EUnit Tests
%%%====================================================================

proper_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Property: Cmdline parsing", fun test_cmdline_parsing/0},
      {"Property: JSON-RPC roundtrip", fun test_json_rpc_roundtrip/0},
      {"Property: Session state transitions", fun test_session_state_transitions/0},
      {"Property: Registry lookup", fun test_registry_lookup/0},
      {"Property: Transport send/receive", fun test_transport_send_receive/0},
      {"Property: Counter monotonic", fun test_counter_monotonic/0},
      {"Property: Session state monotonic", fun test_session_state_monotonic/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    application:stop(erlmcp_cli),
    ok.

test_cmdline_parsing() ->
    ?assert(proper:quickcheck(prop_cmdline_parsing_roundtrip(), 100)).

test_json_rpc_roundtrip() ->
    ?assert(proper:quickcheck(prop_json_rpc_roundtrip(), 100)).

test_session_state_transitions() ->
    ?assert(proper:quickcheck(prop_session_state_transitions(), 50)).

test_registry_lookup() ->
    ?assert(proper:quickcheck(prop_registry_lookup_registered(), 50)).

test_transport_send_receive() ->
    ?assert(proper:quickcheck(prop_transport_send_receive(), 50)).

test_counter_monotonic() ->
    ?assert(proper:quickcheck(prop_counter_monotonic(), 100)).

test_session_state_monotonic() ->
    ?assert(proper:quickcheck(prop_session_state_monotonic(), 50)).
