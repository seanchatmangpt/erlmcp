%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_timeline_viz
%%%
%%% Covers generate_flamegraph/1 (folded-stack format) and
%%% generate_interaction_graph/1 (DOT-language directed graph).
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_timeline_viz_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

%% Minimal Timeline map accepted by generate_flamegraph/1
sample_timeline() ->
    #{
        start_time => 1000,
        end_time   => 5000,
        events     => [
            #{timestamp => 1000, type => call,   duration_us => 500,
              details   => #{pid => self(), function => foo}},
            #{timestamp => 2000, type => call,   duration_us => 300,
              details   => #{pid => self(), function => bar}},
            #{timestamp => 3000, type => gc,     duration_us => 100,
              details   => #{}},
            #{timestamp => 4000, type => send,   duration_us => 50,
              details   => #{pid => self(), to => '<0.99.0>'}},
            #{timestamp => 4100, type => send,   duration_us => 50,
              details   => #{pid => self(), to => '<0.99.0>'}}
        ]
    }.

%%====================================================================
%% Flamegraph Tests
%%====================================================================

flamegraph_returns_ok_test() ->
    Timeline = sample_timeline(),
    Result = erlmcp_timeline_viz:generate_flamegraph(Timeline),
    ?assertMatch({ok, _}, Result).

flamegraph_contains_call_frame_test() ->
    Timeline = sample_timeline(),
    {ok, Output} = erlmcp_timeline_viz:generate_flamegraph(Timeline),
    %% Output must contain at least one "call" folded-stack line
    ?assert(binary:match(Output, <<"call">>) =/= nomatch).

flamegraph_contains_count_test() ->
    Timeline = sample_timeline(),
    {ok, Output} = erlmcp_timeline_viz:generate_flamegraph(Timeline),
    %% Each line is "stack count\n"; check there's at least one numeric count
    Lines = binary:split(Output, <<"\n">>, [global, trim]),
    HasCount = lists:any(fun(Line) ->
        Parts = binary:split(Line, <<" ">>, [global, trim]),
        case lists:last(Parts) of
            N -> re:run(N, <<"^[0-9]+$">>) =/= nomatch
        end
    end, Lines),
    ?assert(HasCount).

flamegraph_empty_timeline_test() ->
    Timeline = #{start_time => 0, end_time => 0, events => []},
    Result = erlmcp_timeline_viz:generate_flamegraph(Timeline),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% Interaction Graph (DOT) Tests
%%====================================================================

interaction_graph_returns_ok_test() ->
    Timeline = sample_timeline(),
    Result = erlmcp_timeline_viz:generate_interaction_graph(Timeline),
    ?assertMatch({ok, _}, Result).

interaction_graph_is_dot_format_test() ->
    %% Build a timeline with send events that have pid/to fields
    Pid1 = self(),
    Pid2 = list_to_pid("<0.1.0>"),
    Timeline = #{
        start_time => 0,
        end_time   => 1000,
        events     => [
            #{timestamp => 100, type => send, duration_us => 10,
              details   => #{pid => Pid1, to => Pid2}},
            #{timestamp => 200, type => send, duration_us => 10,
              details   => #{pid => Pid1, to => Pid2}},
            #{timestamp => 300, type => send, duration_us => 10,
              details   => #{pid => Pid2, to => Pid1}}
        ]
    },
    {ok, Output} = erlmcp_timeline_viz:generate_interaction_graph(Timeline),
    %% Must start with digraph declaration
    ?assert(binary:match(Output, <<"digraph interaction_graph">>) =/= nomatch),
    %% Must contain arrow notation
    ?assert(binary:match(Output, <<" -> ">>) =/= nomatch),
    %% Must contain a message-count label
    ?assert(binary:match(Output, <<"label=">>) =/= nomatch).

interaction_graph_empty_timeline_test() ->
    Timeline = #{start_time => 0, end_time => 0, events => []},
    {ok, Output} = erlmcp_timeline_viz:generate_interaction_graph(Timeline),
    %% Should still produce a valid (empty) digraph
    ?assert(binary:match(Output, <<"digraph interaction_graph">>) =/= nomatch).

interaction_graph_edge_count_test() ->
    %% Two sends from A→B, one from B→A — should yield two distinct edges
    Pid1 = self(),
    Pid2 = list_to_pid("<0.1.0>"),
    Timeline = #{
        start_time => 0,
        end_time   => 1000,
        events     => [
            #{timestamp => 100, type => send, duration_us => 0,
              details   => #{pid => Pid1, to => Pid2}},
            #{timestamp => 200, type => send, duration_us => 0,
              details   => #{pid => Pid1, to => Pid2}},
            #{timestamp => 300, type => send, duration_us => 0,
              details   => #{pid => Pid2, to => Pid1}}
        ]
    },
    {ok, Output} = erlmcp_timeline_viz:generate_interaction_graph(Timeline),
    %% Count occurrences of " -> "
    Parts = binary:split(Output, <<" -> ">>, [global]),
    EdgeCount = length(Parts) - 1,
    ?assert(EdgeCount >= 2).
