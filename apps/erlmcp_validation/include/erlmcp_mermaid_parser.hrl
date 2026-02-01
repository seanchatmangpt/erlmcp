%%%-------------------------------------------------------------------
%%% @doc erlmcp_mermaid_parser - Mermaid Diagram Parser Records
%%%
%%% This header file defines records and types for parsing Mermaid diagram syntax.
%%% Supports all major Mermaid diagram types with comprehensive validation.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_MERMAID_PARSER_HRL).
-define(ERLMCP_MERMAID_PARSER_HRL, 1).

%%% Mermaid Diagram Types
-define(MERMAID_FLOWCHART, flowchart).
-define(MERMAID_SEQUENCE, sequenceDiagram).
-define(MERMAID_CLASS, classDiagram).
-define(MERMAID_STATE, stateDiagram).
-define(MERMAID_ERD, erDiagram).
-define(MERMAID_USER_JOURNEY, journey).
-define(MERMAID_GANTT, gantt).
-define(MERMAID_PIE, pie).
-define(MERMAID_QUADRANT, quadrantChart).
-define(MERMAID_GITGRAPH, gitGraph).
-define(MERMAID_C4, C4).
-define(MERMAID_MINDMAP, mindmap).
-define(MERMAID_TIMELINE, timeline).
-define(MERMAID_ZENUML, zenUML).
-define(MERMAID_SANKEY, sankey).
-define(MERMAID_XY, xychart).
-define(MERMAID_BLOCK, block).
-define(MERMAID_PACKET, packet).
-define(MERMAID_KANBAN, kanban).
-define(MERMAID_ARCHITECTURE, architecture).
-define(MERMAID_RADAR, radar).
-define(MERMAID_TREEMAP, treemap).

%%% Parser Error Types
-define(ERR_SYNTAX, syntax_error).
-define(ERR_STRUCTURE, structure_error).
-define(ERR_VALIDATION, validation_error).
-define(ERR_REFERENCE, reference_error).
-define(ERR_UNSUPPORTED, unsupported_feature).

%%% Parser Position
-record(mermaid_pos, {
    line = 1 :: pos_integer(),
    column = 1 :: pos_integer(),
    offset = 0 :: non_neg_integer()
}).

%%% Parser State
-record(mermaid_parser_state, {
    input :: binary(),
    pos :: #mermaid_pos{},
    diagram_type :: atom() | undefined,
    nodes = [] :: [binary()],
    edges = [] :: [tuple()],
    nested_level = 0 :: non_neg_integer(),
    references = #{} :: #{binary() => #mermaid_pos{}},
    errors = [] :: [tuple()]
}).

%%% Diagram AST Nodes
-record(mermaid_node, {
    id :: binary(),
    label :: binary() | undefined,
    shape = box :: box | circle | diamond | stadium | hexagon | parallelogram | rhombus | trapezoid | cylinder | rectangle | subroutine | rounded,
    styles = [] :: [{binary(), binary()}],
    pos :: #mermaid_pos{}
}).

-record(mermaid_edge, {
    from :: binary(),
    to :: binary(),
    type :: arrow | line | dotted_arrow | dotted_line,
    label :: binary() | undefined,
    pos :: #mermaid_pos{}
}).

-record(mermaid_subgraph, {
    id :: binary(),
    label :: binary() | undefined,
    nodes = [] :: [binary()],
    pos :: #mermaid_pos{}
}).

%%% Flowchart Types
-record(mermaid_flowchart, {
    direction = TD :: TD | DT | LR | RL | TB | BT,
    nodes = [] :: [#mermaid_node{}],
    edges = [] :: [#mermaid_edge{}],
    subgraphs = [] :: [#mermaid_subgraph{}],
    styles = [] :: [{binary(), [{binary(), binary()}]}]
}).

%%% Sequence Diagram Types
-record(mermaid_actor, {
    id :: binary(),
    label :: binary(),
    type = actor :: actor | participant,
    pos :: #mermaid_pos{}
}).

-record(mermaid_message, {
    from :: binary(),
    to :: binary(),
    label :: binary(),
    type :: sync | async | reply | self,
    pos :: #mermaid_pos{}
}).

-record(mermaid_sequence, {
    actors = [] :: [#mermaid_actor{}],
    messages = [] :: [#mermaid_message{}],
    notes = [] :: [tuple()],
    loops = [] :: [tuple()],
    alts = [] :: [tuple()]
}).

%%% Class Diagram Types
-record(mermaid_class, {
    id :: binary(),
    attributes = [] :: [binary()],
    methods = [] :: [binary()],
    relationships = [] :: [tuple()],
    pos :: #mermaid_pos{}
}).

-record(mermaid_class_diagram, {
    classes = [] :: [#mermaid_class{}],
    relationships = [] :: [tuple()]
}).

%%% State Diagram Types
-record(mermaid_state, {
    id :: binary(),
    label :: binary() | undefined,
    type = normal :: normal | start | end | choice | fork | join,
    pos :: #mermaid_pos{}
}).

-record(mermaid_transition, {
    from :: binary(),
    to :: binary(),
    label :: binary() | undefined,
    pos :: #mermaid_pos{}
}).

-record(mermaid_state_diagram, {
    states = [] :: [#mermaid_state{}],
    transitions = [] :: [#mermaid_transition{}],
    compositions = [] :: [tuple()]
}).

%%% ERD Types
-record(mermaid_entity, {
    id :: binary(),
    attributes = [] :: [{binary(), atom()}],
    pos :: #mermaid_pos{}
}).

-record(mermaid_relationship, {
    from :: binary(),
    to :: binary(),
    cardinality :: binary(),
    pos :: #mermaid_pos{}
}).

-record(mermaid_erd, {
    entities = [] :: [#mermaid_entity{}],
    relationships = [] :: [#mermaid_relationship{}]
}).

%%% Gantt Chart Types
-record(mermaid_task, {
    id :: binary(),
    title :: binary(),
    start :: binary(),
    duration :: binary(),
    status = done :: done | active | crit | milestone,
    pos :: #mermaid_pos{}
}).

-record(mermaid_gantt, {
    title :: binary() | undefined,
    sections = [] :: [tuple()],
    tasks = [] :: [#mermaid_task{}],
    milestones = [] :: [tuple()]
}).

%%% User Journey Types
-record(mermaid_task_step, {
    id :: binary(),
    label :: binary(),
    score :: pos_integer(),
    pos :: #mermaid_pos{}
}).

-record(mermaid_journey, {
    title :: binary(),
    actors = [] :: [binary()],
    tasks = [] :: [#mermaid_task_step{}]
}).

%%% Pie Chart Types
-record(mermaid_pie, {
    title :: binary() | undefined,
    show_data = false :: boolean(),
    segments = [] :: [{binary(), number()}]
}).

%%% Quadrant Chart Types
-record(mermaid_quadrant, {
    title :: binary(),
    x_axis :: binary(),
    y_axis :: binary(),
    quadrant_1 :: binary(),
    quadrant_2 :: binary(),
    quadrant_3 :: binary(),
    quadrant_4 :: binary(),
    points = [] :: [{binary(), {number(), number()}}]
}).

%%% Git Graph Types
-record(mermaid_commit, {
    id :: binary(),
    type = commit :: commit | merge | branch | checkout,
    branch :: binary(),
    pos :: #mermaid_pos{}
}).

-record(mermaid_gitgraph, {
    branches = [] :: [binary()],
    commits = [] :: [#mermaid_commit{}]
}).

%%% C4 Context Types
-record(mermaid_c4_element, {
    id :: binary(),
    type = person :: person | system | container | database,
    label :: binary(),
    pos :: #mermaid_pos{}
}).

-record(mermaid_c4_diagram, {
    title :: binary(),
    elements = [] :: [#mermaid_c4_element{}],
    relationships = [] :: [tuple()]
}).

%%% Mindmap Types
-record(mermaid_mindmap_node, {
    id :: binary(),
    label :: binary(),
    level = 0 :: non_neg_integer(),
    children = [] :: [binary()],
    pos :: #mermaid_pos{}
}).

-record(mermaid_mindmap, {
    root :: #mermaid_mindmap_node{},
    nodes = [] :: [#mermaid_mindmap_node{}]
}).

%%% Timeline Types
-record(mermaid_event, {
    date :: binary(),
    title :: binary(),
    pos :: #mermaid_pos{}
}).

-record(mermaid_timeline, {
    title :: binary() | undefined,
    events = [] :: [#mermaid_event{}]
}).

%%% Sankey Diagram Types
-record(mermaid_sankey_link, {
    from :: binary(),
    to :: binary(),
    value :: number(),
    pos :: #mermaid_pos{}
}).

-record(mermaid_sankey, {
    nodes = [] :: [binary()],
    links = [] :: [#mermaid_sankey_link{}]
}).

%%% XY Chart Types
-record(mermaid_series, {
    name :: binary(),
    data = [] :: [{number(), number()}]
}).

-record(mermaid_xy, {
    title :: binary() | undefined,
    x_axis :: binary() | undefined,
    y_axis :: binary() | undefined,
    series = [] :: [#mermaid_series{}]
}).

%%% Block Diagram Types
-record(mermaid_block_diagram, {
    blocks = [] :: [tuple()],
    connections = [] :: [tuple()]
}).

%%% Kanban Types
-record(mermaid_kanban_item, {
    id :: binary(),
    title :: binary(),
    status :: binary(),
    pos :: #mermaid_pos{}
}).

-record(mermaid_kanban, {
    title :: binary(),
    columns = [] :: [binary()],
    items = [] :: [#mermaid_kanban_item{}]
}).

%%% Architecture Diagram Types
-record(mermaid_arch_service, {
    id :: binary(),
    type = service :: service | database | queue | cache | external,
    label :: binary(),
    pos :: #mermaid_pos{}
}).

-record(mermaid_arch_diagram, {
    title :: binary(),
    services = [] :: [#mermaid_arch_service{}],
    connections = [] :: [tuple()]
}).

%%% Radar Chart Types
-record(mermaid_radar, {
    title :: binary() | undefined,
    axes = [] :: [binary()],
    datasets = [] :: [{binary(), [number()]}]
}).

%%% Treemap Types
-record(mermaid_treemap_node, {
    id :: binary(),
    value :: number(),
    children = [] :: [#mermaid_treemap_node{}],
    pos :: #mermaid_pos{}
}).

-record(mermaid_treemap, {
    title :: binary() | undefined,
    root :: #mermaid_treemap_node{}
}).

%%% Generic AST Wrapper
-record(mermaid_ast, {
    type :: atom(),
    diagram :: term(),
    metadata :: map()
}).

%%% Parser Result
-record(mermaid_parse_result, {
    success :: boolean(),
    ast :: #mermaid_ast{} | undefined,
    errors = [] :: [tuple()],
    warnings = [] :: [tuple()],
    pos :: #mermaid_pos{}
}).

%%% Validation Result
-record(mermaid_validation_result, {
    valid :: boolean(),
    errors = [] :: [tuple()],
    warnings = [] :: [tuple()],
    suggestions = [] :: [binary()]
}).

%%% Export types
-export_type([
    mermaid_pos/0,
    mermaid_parser_state/0,
    mermaid_node/0,
    mermaid_edge/0,
    mermaid_ast/0,
    mermaid_parse_result/0,
    mermaid_validation_result/0
]).

-endif.
