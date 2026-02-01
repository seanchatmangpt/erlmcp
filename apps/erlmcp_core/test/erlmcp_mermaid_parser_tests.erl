%%%====================================================================
%%% @doc Comprehensive Test Suite for erlmcp_mermaid_parser
%%%
%%% Chicago School TDD: Real parser processes, state-based verification
%%%
%%% Tests:
%%% - Flowchart diagram parsing
%%% - Sequence diagram parsing
%%% - State diagram parsing
%%% - Entity relationship parsing
%%% - User journey parsing
%%% - Gantt chart parsing
%%% - Pie chart parsing
%%% - Mindmap parsing
%%% - Timeline parsing
%%% - Syntax error detection
%%% - Token validation
%%% - Edge case handling
%%% - Malformed input recovery
%%%
%%% Target: 85%+ coverage (Core module)
%%% @end
%%%====================================================================
-module(erlmcp_mermaid_parser_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Generators
%%%====================================================================

mermaid_parser_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Flowchart Parsing", {spawn, fun flowchart_tests/0}},
          {"Sequence Diagram Parsing", {spawn, fun sequence_tests/0}},
          {"State Diagram Parsing", {spawn, fun state_tests/0}},
          {"ER Diagram Parsing", {spawn, fun er_tests/0}},
          {"User Journey Parsing", {spawn, fun journey_tests/0}},
          {"Gantt Chart Parsing", {spawn, fun gantt_tests/0}},
          {"Pie Chart Parsing", {spawn, fun pie_tests/0}},
          {"Mindmap Parsing", {spawn, fun mindmap_tests/0}},
          {"Timeline Parsing", {spawn, fun timeline_tests/0}},
          {"Syntax Error Detection", {spawn, fun syntax_error_tests/0}},
          {"Token Validation", {spawn, fun token_tests/0}},
          {"Edge Cases", {spawn, fun edge_case_tests/0}},
          {"Malformed Input Recovery", {spawn, fun malformed_tests/0}},
          {"Property-Based Tests", {spawn, fun property_tests/0}}
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Flowchart Diagram Tests
%%%====================================================================

flowchart_tests() ->
    %% Simple flowchart
    SimpleFlow = <<"flowchart TD\n    A[Start] --> B[End]">>,
    {ok, FlowAst1} = erlmcp_mermaid_parser:parse(SimpleFlow),
    ?assertEqual(<<"flowchart">>, maps:get(<<"type">>, FlowAst1)),
    ?assertEqual(<<"TD">>, maps:get(<<"direction">>, FlowAst1)),
    ?assert(maps:is_key(<<"nodes">>, FlowAst1)),

    %% Complex flowchart with multiple edges
    ComplexFlow = <<"flowchart LR\n    A[Start] --> B{Decision}\n    B -->|Yes| C[Action]\n    B -->|No| D[End]">>,
    {ok, FlowAst2} = erlmcp_mermaid_parser:parse(ComplexFlow),
    Nodes = maps:get(<<"nodes">>, FlowAst2),
    ?assertEqual(4, length(Nodes)),
    Edges = maps:get(<<"edges">>, FlowAst2),
    ?assertEqual(3, length(Edges)),

    %% Flowchart with styles
    StyledFlow = <<"flowchart TD\n    A[Start] --> B[End]\n    style A fill:#f9f,stroke:#333">>,
    {ok, FlowAst3} = erlmcp_mermaid_parser:parse(StyledFlow),
    ?assert(maps:is_key(<<"styles">>, FlowAst3)),

    %% Flowchart with subgraphs
    SubgraphFlow = <<"flowchart TD\n    subgraph Sub1\n        A[Start]\n        B[End]\n    end">>,
    {ok, FlowAst4} = erlmcp_mermaid_parser:parse(SubgraphFlow),
    ?assert(maps:is_key(<<"subgraphs">>, FlowAst4)),

    ok.

%%%====================================================================
%%% Sequence Diagram Tests
%%%====================================================================

sequence_tests() ->
    %% Basic sequence diagram
    BasicSeq = <<"sequenceDiagram\n    Alice->>Bob: Hello\n    Bob-->>Alice: Hi">>,
    {ok, SeqAst1} = erlmcp_mermaid_parser:parse(BasicSeq),
    ?assertEqual(<<"sequenceDiagram">>, maps:get(<<"type">>, SeqAst1)),
    Participants = maps:get(<<"participants">>, SeqAst1),
    ?assertEqual(2, length(Participants)),
    Messages = maps:get(<<"messages">>, SeqAst1),
    ?assertEqual(2, length(Messages)),

    %% Sequence with self-referential message
    SelfSeq = <<"sequenceDiagram\n    Alice->>Alice: Self reflection">>,
    {ok, SeqAst2} = erlmcp_mermaid_parser:parse(SelfSeq),
    ?assertEqual(1, length(maps:get(<<"messages">>, SeqAst2))),

    %% Sequence with loops
    LoopSeq = <<"sequenceDiagram\n    Alice->>Bob: Request\n    loop Every minute\n        Bob->>Bob: Process\n    end">>,
    {ok, SeqAst3} = erlmcp_mermaid_parser:parse(LoopSeq),
    ?assert(maps:is_key(<<"blocks">>, SeqAst3)),

    %% Sequence with alt/opt/par
    AltSeq = <<"sequenceDiagram\n    Alice->>Bob: Request\n    alt Success\n        Bob-->>Alice: OK\n    else Failure\n        Bob-->>Alice: Error\n    end">>,
    {ok, SeqAst4} = erlmcp_mermaid_parser:parse(AltSeq),
    Blocks = maps:get(<<"blocks">>, SeqAst4),
    ?assertEqual(1, length(Blocks)),

    ok.

%%%====================================================================
%%% State Diagram Tests
%%%====================================================================

state_tests() ->
    %% Basic state diagram
    BasicState = <<"stateDiagram-v2\n    [*] --> Idle\n    Idle --> Processing\n    Processing --> [*]">>,
    {ok, StateAst1} = erlmcp_mermaid_parser:parse(BasicState),
    ?assertEqual(<<"stateDiagram-v2">>, maps:get(<<"type">>, StateAst1)),
    States = maps:get(<<"states">>, StateAst1),
    ?assertEqual(3, length(States)),

    %% State with transitions
    TransitionState = <<"stateDiagram-v2\n    [*] --> Still\n    Still --> [*]\n    Still --> Moving\n    Moving --> Still\n    Moving --> Crash\n    Crash --> [*]">>,
    {ok, StateAst2} = erlmcp_mermaid_parser:parse(TransitionState),
    Transitions = maps:get(<<"transitions">>, StateAst2),
    ?assertEqual(6, length(Transitions)),

    %% State with composite states
    CompositeState = <<"stateDiagram-v2\n    [*] --> Active\n    state Active {\n        [*] --> Running\n        Running --> Paused\n        Paused --> Running\n    }">>,
    {ok, StateAst3} = erlmcp_mermaid_parser:parse(CompositeState),
    ?assert(maps:is_key(<<"composite_states">>, StateAst3)),

    %% State with concurrent states
    ConcurrentState = <<"stateDiagram-v2\n    [*] --> Active\n    Active --> Region1\n    Active --> Region2\n    Region1 --> [*]\n    Region2 --> [*]">>,
    {ok, StateAst4} = erlmcp_mermaid_parser:parse(ConcurrentState),

    ok.

%%%====================================================================
%%% Entity Relationship Tests
%%%====================================================================

er_tests() ->
    %% Basic ER diagram
    BasicER = <<"erDiagram\n    CUSTOMER ||--o{ ORDER : places\n    ORDER ||--|{ LINE_ITEM : contains">>,
    {ok, ERAst1} = erlmcp_mermaid_parser:parse(BasicER),
    ?assertEqual(<<"erDiagram">>, maps:get(<<"type">>, ERAst1)),
    Entities = maps:get(<<"entities">>, ERAst1),
    ?assertEqual(4, length(Entities)),
    Relationships = maps:get(<<"relationships">>, ERAst1),
    ?assertEqual(2, length(Relationships)),

    %% ER with attributes
    AttributedER = <<"erDiagram\n    CUSTOMER {\n        string name\n        string email\n    }\n    CUSTOMER ||--o{ ORDER : places">>,
    {ok, ERAst2} = erlmcp_mermaid_parser:parse(AttributedER),
    ?assert(maps:is_key(<<"attributes">>, ERAst2)),

    %% ER with all relationship types
    AllRelER = <<"erDiagram\n    A ||--|| B : one_to_one\n    A ||--|{ C : one_to_many\n    A }|--|{ D : many_to_many">>,
    {ok, ERAst3} = erlmcp_mermaid_parser:parse(AllRelER),

    ok.

%%%====================================================================
%%% User Journey Tests
%%%====================================================================

journey_tests() ->
    %% Basic user journey
    BasicJourney = <<"journey\n    title My Journey\n    section First Step\n      Task: 5: Me\n    section Second Step\n      Task: 3: Me">>,
    {ok, JourneyAst1} = erlmcp_mermaid_parser:parse(BasicJourney),
    ?assertEqual(<<"journey">>, maps:get(<<"type">>, JourneyAst1)),
    ?assertEqual(<<"My Journey">>, maps:get(<<"title">>, JourneyAst1)),

    %% Journey with multiple tasks
    MultiJourney = <<"journey\n    title Daily Routine\n    section Morning\n      Wake up: 5: Me\n      Breakfast: 3: Me\n    section Evening\n      Dinner: 4: Me\n      Sleep: 5: Me">>,
    {ok, JourneyAst2} = erlmcp_mermaid_parser:parse(MultiJourney),
    Sections = maps:get(<<"sections">>, JourneyAst2),
    ?assertEqual(2, length(Sections)),

    ok.

%%%====================================================================
%%% Gantt Chart Tests
%%%====================================================================

gantt_tests() ->
    %% Basic Gantt chart
    BasicGantt = <<"gantt\n    title Project Timeline\n    dateFormat YYYY-MM-DD\n    section Design\n    Prototype: 2024-01-01, 5d">>,
    {ok, GanttAst1} = erlmcp_mermaid_parser:parse(BasicGantt),
    ?assertEqual(<<"gantt">>, maps:get(<<"type">>, GanttAst1)),
    ?assertEqual(<<"Project Timeline">>, maps:get(<<"title">>, GanttAst1)),

    %% Gantt with milestones
    MilestoneGantt = <<"gantt\n    title Project\n    milestone: m1, 2024-01-01, 0d">>,
    {ok, GanttAst2} = erlmcp_mermaid_parser:parse(MilestoneGantt),
    ?assert(maps:is_key(<<"milestones">>, GanttAst2)),

    ok.

%%%====================================================================
%%% Pie Chart Tests
%%%====================================================================

pie_tests() ->
    %% Basic pie chart
    BasicPie = <<"pie title Pets\n    \"Dogs\" : 386\n    \"Cats\" : 85">>,
    {ok, PieAst1} = erlmcp_mermaid_parser:parse(BasicPie),
    ?assertEqual(<<"pie">>, maps:get(<<"type">>, PieAst1)),
    Data = maps:get(<<"data">>, PieAst1),
    ?assertEqual(2, length(Data)),

    %% Pie with multiple segments
    MultiPie = <<"pie title Spending\n    \"Rent\" : 1000\n    \"Food\" : 500\n    \"Utilities\" : 200">>,
    {ok, PieAst2} = erlmcp_mermaid_parser:parse(MultiPie),
    ?assertEqual(3, length(maps:get(<<"data">>, PieAst2))),

    ok.

%%%====================================================================
%%% Mindmap Tests
%%%====================================================================

mindmap_tests() ->
    %% Basic mindmap
    BasicMind = <<"mindmap\n    Root((Mindmap))\n      Branch1\n      Branch2">>,
    {ok, MindAst1} = erlmcp_mermaid_parser:parse(BasicMind),
    ?assertEqual(<<"mindmap">>, maps:get(<<"type">>, MindAst1)),
    ?assert(maps:is_key(<<"root">>, MindAst1)),

    %% Nested mindmap
    NestedMind = <<"mindmap\n    Root((Central))\n      Branch1\n        Leaf1\n        Leaf2\n      Branch2\n        Leaf3">>,
    {ok, MindAst2} = erlmcp_mermaid_parser:parse(NestedMind),
    ?assert(maps:is_key(<<"branches">>, MindAst2)),

    ok.

%%%====================================================================
%%% Timeline Tests
%%%====================================================================

timeline_tests() ->
    %% Basic timeline
    BasicTimeline = <<"timeline\n    title History\n    2020 : Event 1\n    2021 : Event 2">>,
    {ok, TimelineAst1} = erlmcp_mermaid_parser:parse(BasicTimeline),
    ?assertEqual(<<"timeline">>, maps:get(<<"type">>, TimelineAst1)),
    Events = maps:get(<<"events">>, TimelineAst1),
    ?assertEqual(2, length(Events)),

    ok.

%%%====================================================================
%%% Syntax Error Detection Tests
%%%====================================================================

syntax_error_tests() ->
    %% Unclosed block
    Unclosed = <<"flowchart TD\n    subgraph Sub1\n        A[Start]\n">>,
    {error, {unclosed_block, _}} = erlmcp_mermaid_parser:parse(Unclosed),

    %% Invalid arrow syntax
    InvalidArrow = <<"flowchart TD\n    A ==> B">>,
    {error, {invalid_syntax, _}} = erlmcp_mermaid_parser:parse(InvalidArrow),

    %% Unknown diagram type
    UnknownType = <<"unknownType\n    A[Start]">>,
    {error, {unknown_diagram_type, _}} = erlmcp_mermaid_parser:parse(UnknownType),

    %% Duplicate node IDs
    DuplicateID = <<"flowchart TD\n    A[Start]\n    A[End]">>,
    {error, {duplicate_id, _}} = erlmcp_mermaid_parser:parse(DuplicateID),

    ok.

%%%====================================================================
%%% Token Validation Tests
%%%====================================================================

token_tests() ->
    %% Valid identifiers
    ValidIdentifiers = [
        <<"A">>, <<"node1">>, <<"my_node">>, <<"node123">>,
        <<"_node">>, <<"Node-1">>, <<"node_1_2">>
    ],
    lists:foreach(fun(Id) ->
        Input = <<"flowchart TD\n    ", Id/binary, "[Test] --> B[End]">>,
        ?assertMatch({ok, _}, erlmcp_mermaid_parser:parse(Input))
    end, ValidIdentifiers),

    %% Invalid identifiers
    InvalidIdentifiers = [
        <<"123node">>, <<"node!">>, <<"node@">>, <<"node#">>,
        <<"node ">>, <<"-node">>, <<"node-">>
    ],
    lists:foreach(fun(Id) ->
        Input = <<"flowchart TD\n    ", Id/binary, "[Test] --> B[End]">>,
        ?assertMatch({error, _}, erlmcp_mermaid_parser:parse(Input))
    end, InvalidIdentifiers),

    ok.

%%%====================================================================
%%% Edge Case Tests
%%%====================================================================

edge_case_tests() ->
    %% Empty diagram
    Empty = <<"flowchart TD">>,
    {ok, EmptyAst} = erlmcp_mermaid_parser:parse(Empty),
    ?assertEqual(0, length(maps:get(<<"nodes">>, EmptyAst, []))),

    %% Single node
    SingleNode = <<"flowchart TD\n    A[Only]">>,
    {ok, SingleAst} = erlmcp_mermaid_parser:parse(SingleNode),
    ?assertEqual(1, length(maps:get(<<"nodes">>, SingleAst))),

    %% Maximum depth nesting
    DeepNest = <<"flowchart TD\n    A --> B\n    B --> C\n    C --> D\n    D --> E\n    E --> F">>,
    {ok, DeepAst} = erlmcp_mermaid_parser:parse(DeepNest),
    ?assertEqual(6, length(maps:get(<<"nodes">>, DeepAst))),

    %% Unicode characters
    UnicodeFlow = <<"flowchart TD\n    A[开始] --> B[終了]">>,
    {ok, UnicodeAst} = erlmcp_mermaid_parser:parse(UnicodeFlow),
    ?assertEqual(2, length(maps:get(<<"nodes">>, UnicodeAst))),

    %% Very long identifiers
    LongID = list_to_binary([<<"flowchart TD\n    ">>, binary:copy(<<"a">>, 1000), <<"[Long] --> B[End]">>]),
    ?assertMatch({ok, _}, erlmcp_mermaid_parser:parse(LongID)),

    ok.

%%%====================================================================
%%% Malformed Input Recovery Tests
%%%====================================================================

malformed_tests() ->
    %% Missing newline
    NoNewline = <<"flowchart TD\n    A[Start] --> B[End]    C[Extra] --> D[Another]">>,
    {ok, _} = erlmcp_mermaid_parser:parse(NoNewline),

    %% Mixed line endings
    MixedEndings = <<"flowchart TD\n    A[Start] --> B[End]\r\n    C[Extra] --> D[Another]\n">>,
    {ok, _} = erlmcp_mermaid_parser:parse(MixedEndings),

    %% Trailing whitespace
    TrailingWS = <<"flowchart TD\n    A[Start] --> B[End]   \n    C[Extra] --> D[Another]\t\n">>,
    {ok, _} = erlmcp_mermaid_parser:parse(TrailingWS),

    %% Extra blank lines
    BlankLines = <<"flowchart TD\n\n    A[Start] --> B[End]\n\n\n    C[Extra] --> D[Another]\n">>,
    {ok, _} = erlmcp_mermaid_parser:parse(BlankLines),

    ok.

%%%====================================================================
%%% Property-Based Tests (Proper)
%%%====================================================================

property_tests() ->
    %% Round-trip property: Parse -> AST -> String -> Parse -> AST'
    prop_roundtrip(),
    prop_parse_idempotent(),
    prop_ast_well_formed().

prop_roundtrip() ->
    ?FORALL(Diagram, valid_flowchart(),
        begin
            {ok, Ast1} = erlmcp_mermaid_parser:parse(Diagram),
            {ok, Diagram2} = erlmcp_mermaid_parser:ast_to_string(Ast1),
            {ok, Ast2} = erlmcp_mermaid_parser:parse(Diagram2),
            equals_ast(Ast1, Ast2)
        end).

prop_parse_idempotent() ->
    ?FORALL(Diagram, valid_flowchart(),
        begin
            {ok, Ast1} = erlmcp_mermaid_parser:parse(Diagram),
            {ok, Ast2} = erlmcp_mermaid_parser:parse(Diagram),
            equals_ast(Ast1, Ast2)
        end).

prop_ast_well_formed() ->
    ?FORALL(Diagram, valid_flowchart(),
        begin
            {ok, Ast} = erlmcp_mermaid_parser:parse(Diagram),
            has_required_fields(Ast)
        end).

%%%====================================================================
%%% Property Generators
%%%====================================================================

valid_flowchart() ->
    ?LET({Nodes, Edges}, gen_nodes_and_edges(),
        begin
            NodeLines = [<<(N)/binary, "[", (atom_to_list(N))/binary, "]">> || N <- Nodes],
            EdgeLines = [<<(N1)/binary, " --> ", (N2)/binary>> || {N1, N2} <- Edges],
            Header = <<"flowchart TD">>,
            Body = list_to_binary(lists:join(<<"\n    ">>, [Header | NodeLines ++ EdgeLines])),
            Body
        end).

gen_nodes_and_edges() ->
    ?LET(Nodes, list(non_empty(atom_range($A, $Z))),
        begin
            Edges = list_to_tuples(Nodes),
            {Nodes, Edges}
        end).

gen_atom_range(Start, End) ->
    oneof([list_to_atom([C]) || C <- lists:seq(Start, End)]).

list_to_tuples([_]) -> [];
list_to_tuples([A, B | Rest]) -> [{A, B} | list_to_tuples([B | Rest])].

%%%====================================================================
%%% Property Helpers
%%%====================================================================

equals_ast(Ast1, Ast2) ->
    %% Compare key fields (ignore metadata)
    maps:get(<<"type">>, Ast1) =:= maps:get(<<"type">>, Ast2)
    andalso maps:get(<<"direction">>, Ast1, undefined) =:= maps:get(<<"direction">>, Ast2, undefined)
    andalso length(maps:get(<<"nodes">>, Ast1, [])) =:= length(maps:get(<<"nodes">>, Ast2, []))
    andalso length(maps:get(<<"edges">>, Ast1, [])) =:= length(maps:get(<<"edges">>, Ast2, [])).

has_required_fields(Ast) ->
    %% Check AST has required fields
    maps:is_key(<<"type">>, Ast)
    andalso (maps:get(<<"type">>, Ast) =:= <<"flowchart">> orelse
             maps:get(<<"type">>, Ast) =:= <<"sequenceDiagram">> orelse
             maps:get(<<"type">>, Ast) =:= <<"stateDiagram-v2">>).
