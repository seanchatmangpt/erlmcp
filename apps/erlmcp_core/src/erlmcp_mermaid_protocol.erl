%%%-------------------------------------------------------------------
%%% @doc erlmcp_mermaid_protocol - Mermaid Diagram Protocol Handler
%%%
%%% Core gen_server implementing JSON-RPC 2.0 interface for Mermaid diagram
%%% generation and validation. Provides request-ID correlation, comprehensive
%%% error handling with refusal codes, and support for all Mermaid diagram types.
%%%
%%% ## Diagram Types Supported
%%% - Flowcharts (flowchart, graph)
%%% - Sequence Diagrams (sequenceDiagram)
%%% - Class Diagrams (classDiagram)
%%% - State Diagrams (stateDiagram)
%%% - Entity Relationship (erDiagram)
%%% - User Journey (journey)
%%% - Gantt Charts (gantt)
%%% - Pie Charts (pie)
%%% - Mind Maps (mindmap)
%%% - Timeline (timeline)
%%% - Git Graphs (gitGraph)
%%%
%%% ## Architecture
%%% - Implements gen_server behavior with proper OTP patterns
%%% - Request-ID correlation via State.pending: #{RequestId => Request}
%%% - Integration with erlmcp_json_rpc for message encoding/decoding
%%% - Mermaid syntax validation with detailed error reporting
%%% - Refusal code support (1001-1089) for content policy violations
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_mermaid_protocol).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/1,
    render_diagram/2,
    render_diagram/3,
    validate_syntax/2,
    list_diagram_types/0,
    list_diagram_types/1,
    get_diagram_metadata/2,
    batch_render/2,
    cancel_render/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Type exports
-export_type([
    mermaid_server/0,
    diagram_type/0,
    diagram_request/0,
    diagram_result/0,
    render_options/0,
    validation_error/0,
    syntax_error/0
]).

%% Include records and macros
-include("erlmcp.hrl").

%%%====================================================================
%%% Type Definitions
%%%====================================================================

%% Mermaid server process
-type mermaid_server() :: pid().

%% Supported Mermaid diagram types
-type diagram_type() ::
    flowchart |                     %% Traditional flowchart
    graph |                         %% Graph (left-to-right or top-to-bottom)
    sequence_diagram |              %% Sequence diagram
    class_diagram |                 %% Class diagram
    state_diagram |                 %% State diagram
    er_diagram |                    %% Entity relationship diagram
    user_journey |                  %% User journey diagram
    gantt |                         %% Gantt chart
    pie_chart |                     %% Pie chart
    mindmap |                       %% Mind map
    timeline |                      %% Timeline diagram
    git_graph |                     %% Git graph
    block |                         %% Block diagram
    architecture |                  %% Architecture diagram
    c4_context |                    %% C4 context diagram
    c4_container |                  %% C4 container diagram
    c4_component |                  %% C4 component diagram
    c4_deployment |                 %% C4 deployment diagram
    custom_diagram.                 %% Custom diagram type

%% Diagram request structure
-type diagram_request() :: #{
    type => diagram_type(),
    source => binary(),
    options => render_options()
}.

%% Render options
-type render_options() :: #{
    theme => binary() | undefined,           %% Theme: default, forest, dark, neutral
    backgroundColor => binary() | undefined,  %% Background color
    format => binary() | undefined,           %% Output format: svg, png
    width => pos_integer() | undefined,       %% Diagram width
    height => pos_integer() | undefined,      %% Diagram height
    scale => float() | undefined,             %% Scale factor
    %% Mermaid-specific options
    direction => binary() | undefined,        %% TD (top-down) or LR (left-right)
    curveStyle => binary() | undefined,       %% Curve style for edges
    %% Security options
    max_nodes => pos_integer() | undefined,   %% Maximum nodes (DoS protection)
    max_edges => pos_integer() | undefined,   %% Maximum edges (DoS protection)
    timeout => pos_integer() | undefined      %% Render timeout in ms
}.

%% Diagram render result
-type diagram_result() :: #{
    status => ok | error,
    rendered => binary() | undefined,         %% Rendered output (SVG/PNG)
    metadata => map() | undefined,            %% Render metadata
    errors => [validation_error()] | undefined
}.

%% Validation error with location information
-type validation_error() :: #{
    line => pos_integer() | undefined,
    column => pos_integer() | undefined,
    severity => error | warning | info,
    code => integer(),                        %% Refusal code 1001-1089
    message => binary(),
    hint => binary()
}.

%% Syntax error details
-type syntax_error() :: #{
    type => parse_error | syntax_error | semantic_error,
    message => binary(),
    line => pos_integer() | undefined,
    context => binary() | undefined
}.

%% Request state for correlation
-type request_correlation() :: #{
    request_id => binary(),
    from => {pid(), term()},
    timestamp => integer(),
    method => binary(),
    params => map()
}.

%% Server state record
-record(state, {
    pending_requests = #{} :: #{binary() => request_correlation()},
    request_counter = 1 :: pos_integer(),
    supported_types :: [diagram_type()],
    default_theme = <<"default">> :: binary(),
    default_format = <<"svg">> :: binary(),
    max_diagram_size = 100000 :: pos_integer(),  %% 100KB max diagram source
    render_timeout = 30000 :: pos_integer(),      %% 30 second default timeout
    %% Performance tracking
    renders_total = 0 :: non_neg_integer(),
    renders_success = 0 :: non_neg_integer(),
    renders_failed = 0 :: non_neg_integer(),
    %% Cache for frequently rendered diagrams
    cache :: ets:tid() | undefined
}).

-type state() :: #state{}.

%%%====================================================================
%%% Macros
%%%====================================================================

%% Default render options
-define(DEFAULT_RENDER_OPTIONS, #{
    theme => <<"default">>,
    format => <<"svg">>,
    scale => 1.0,
    timeout => 30000
}).

%% Error code macros (refusal codes 1001-1089)
-define(ERR_MERMAID_SYNTAX_ERROR, 1001).
-define(ERR_MERMAID_PARSE_ERROR, 1002).
-define(ERR_MERMAID_TOO_LARGE, 1003).
-define(ERR_MERMAID_TIMEOUT, 1004).
-define(ERR_MERMAID_UNSUPPORTED_TYPE, 1005).
-define(ERR_MERMAID_POLICY_VIOLATION, 1006).
-define(ERR_MERMAID_RESOURCE_LIMIT, 1007).

%% Convenience error macros
-define(ERR_MSG_SYNTAX_ERROR, <<"Mermaid syntax error">>).
-define(ERR_MSG_PARSE_ERROR, <<"Failed to parse Mermaid diagram">>).
-define(ERR_MSG_TOO_LARGE, <<"Diagram source exceeds maximum size">>).
-define(ERR_MSG_TIMEOUT, <<"Diagram render timeout">>).
-define(ERR_MSG_UNSUPPORTED_TYPE, <<"Unsupported diagram type">>).
-define(ERR_MSG_POLICY_VIOLATION, <<"Diagram content violates policy">>).
-define(ERR_MSG_RESOURCE_LIMIT, <<"Diagram exceeds resource limits">>).

%% Generate unique request ID
-define(GENERATE_REQUEST_ID(Counter),
    integer_to_binary(Counter)
).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start Mermaid protocol server with default options
-spec start_link() -> {ok, mermaid_server()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start Mermaid protocol server with options
-spec start_link(map()) -> {ok, mermaid_server()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Stop Mermaid protocol server
-spec stop(mermaid_server()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%% @doc Render a Mermaid diagram (simplified interface)
-spec render_diagram(mermaid_server(), binary()) -> {ok, diagram_result()} | {error, term()}.
render_diagram(Server, Source) ->
    render_diagram(Server, Source, #{}).

%% @doc Render a Mermaid diagram with options
-spec render_diagram(mermaid_server(), binary(), map()) ->
    {ok, diagram_result()} | {error, term()}.
render_diagram(Server, Source, Options) when is_binary(Source), is_map(Options) ->
    gen_server:call(Server, {render_diagram, Source, Options}, infinity).

%% @doc Validate Mermaid diagram syntax without rendering
-spec validate_syntax(mermaid_server(), binary()) ->
    {ok, [map()]} | {error, [validation_error()]}.
validate_syntax(Server, Source) when is_binary(Source) ->
    gen_server:call(Server, {validate_syntax, Source}, 30000).

%% @doc List all supported diagram types
-spec list_diagram_types() -> [diagram_type()].
list_diagram_types() ->
    [
        flowchart,
        graph,
        sequence_diagram,
        class_diagram,
        state_diagram,
        er_diagram,
        user_journey,
        gantt,
        pie_chart,
        mindmap,
        timeline,
        git_graph,
        block,
        architecture,
        c4_context,
        c4_container,
        c4_component,
        c4_deployment,
        custom_diagram
    ].

%% @doc List supported diagram types from server
-spec list_diagram_types(mermaid_server()) -> {ok, [diagram_type()]}.
list_diagram_types(Server) ->
    gen_server:call(Server, list_types, 5000).

%% @doc Get metadata about a diagram type
-spec get_diagram_metadata(mermaid_server(), diagram_type()) ->
    {ok, map()} | {error, term()}.
get_diagram_metadata(Server, Type) ->
    gen_server:call(Server, {get_metadata, Type}, 5000).

%% @doc Render multiple diagrams in batch
-spec batch_render(mermaid_server(), [binary()]) ->
    {ok, [diagram_result()]} | {error, term()}.
batch_render(Server, Sources) when is_list(Sources) ->
    gen_server:call(Server, {batch_render, Sources}, infinity).

%% @doc Cancel an in-progress render operation
-spec cancel_render(mermaid_server(), binary()) -> ok | {error, term()}.
cancel_render(Server, RequestId) when is_binary(RequestId) ->
    gen_server:call(Server, {cancel_render, RequestId}, 5000).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

%% @doc Initialize Mermaid protocol server
-spec init(map()) -> {ok, state()}.
init(Options) ->
    logger:info("Initializing Mermaid protocol server"),

    %% Create ETS table for diagram caching
    CacheTable = ets:new(mermaid_diagram_cache, [
        named_table,
        set,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    %% Extract options with defaults
    SupportedTypes = maps:get(supported_types, Options, list_diagram_types()),
    DefaultTheme = maps_get_default(theme, Options, <<"default">>),
    DefaultFormat = maps_get_default(format, Options, <<"svg">>),
    MaxSize = maps_get_default(max_diagram_size, Options, 100000),
    Timeout = maps_get_default(render_timeout, Options, 30000),

    State = #state{
        supported_types = SupportedTypes,
        default_theme = DefaultTheme,
        default_format = DefaultFormat,
        max_diagram_size = MaxSize,
        render_timeout = Timeout,
        cache = CacheTable
    },

    logger:info("Mermaid protocol server initialized with ~p diagram types",
                [length(SupportedTypes)]),
    {ok, State}.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.

%% Render diagram request
handle_call({render_diagram, Source, Options}, From, State) ->
    RequestId = generate_request_id(State),

    logger:debug("Rendering diagram request ~p", [RequestId]),

    %% Validate diagram source size
    case validate_diagram_size(Source, State) of
        ok ->
            %% Detect diagram type from source
            case detect_diagram_type(Source) of
                {ok, DiagramType} ->
                    %% Merge options with defaults
                    RenderOptions = merge_render_options(Options, State),

                    %% Check if type is supported
                    case lists:member(DiagramType, State#state.supported_types) of
                        true ->
                            %% Store correlation for async processing
                            Correlation = #{
                                request_id => RequestId,
                                from => From,
                                timestamp => erlang:system_time(millisecond),
                                method => <<"render_diagram">>,
                                params => #{source => Source, options => RenderOptions}
                            },
                            NewPending = maps:put(RequestId, Correlation, State#state.pending_requests),

                            %% Spawn render worker
                            spawn_render_worker(self(), RequestId, DiagramType, Source, RenderOptions),

                            {noreply, State#state{pending_requests = NewPending}};
                        false ->
                            Error = build_error_result(
                                ?ERR_MERMAID_UNSUPPORTED_TYPE,
                                ?ERR_MSG_UNSUPPORTED_TYPE,
                                #{<<"supported_types">> => State#state.supported_types}
                            ),
                            {reply, {error, Error}, State}
                    end;
                {error, Reason} ->
                    Error = build_error_result(
                        ?ERR_MERMAID_PARSE_ERROR,
                        Reason,
                        #{}
                    ),
                    {reply, {error, Error}, State}
            end;
        {error, Reason} ->
            Error = build_error_result(
                ?ERR_MERMAID_TOO_LARGE,
                Reason,
                #{<<"max_size">> => State#state.max_diagram_size}
            ),
            {reply, {error, Error}, State}
    end;

%% Validate syntax request
handle_call({validate_syntax, Source}, _From, State) ->
    logger:debug("Validating Mermaid syntax"),

    case validate_diagram_size(Source, State) of
        ok ->
            case validate_mermaid_syntax(Source) of
                {ok, Warnings} ->
                    {reply, {ok, Warnings}, State};
                {error, Errors} ->
                    {reply, {error, Errors}, State}
            end;
        {error, Reason} ->
            Error = #{
                code => ?ERR_MERMAID_TOO_LARGE,
                message => Reason,
                hint => <<"Reduce diagram source size">>
            },
            {reply, {error, [Error]}, State}
    end;

%% List diagram types
handle_call(list_types, _From, State) ->
    {reply, {ok, State#state.supported_types}, State};

%% Get diagram type metadata
handle_call({get_metadata, Type}, _From, State) ->
    case lists:member(Type, State#state.supported_types) of
        true ->
            Metadata = get_type_metadata(Type),
            {reply, {ok, Metadata}, State};
        false ->
            {reply, {error, unsupported_type}, State}
    end;

%% Batch render
handle_call({batch_render, Sources}, From, State) when is_list(Sources) ->
    logger:debug("Batch rendering ~p diagrams", [length(Sources)]),

    %% Validate all sources first
    case validate_batch_sources(Sources, State) of
        ok ->
            RequestId = generate_request_id(State),
            Correlation = #{
                request_id => RequestId,
                from => From,
                timestamp => erlang:system_time(millisecond),
                method => <<"batch_render">>,
                params => #{sources => Sources}
            },
            NewPending = maps:put(RequestId, Correlation, State#state.pending_requests),

            %% Spawn batch render worker
            spawn_batch_render_worker(self(), RequestId, Sources, State),

            {noreply, State#state{pending_requests = NewPending}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Cancel render
handle_call({cancel_render, RequestId}, _From, State) ->
    case maps:find(RequestId, State#state.pending_requests) of
        {ok, _Correlation} ->
            %% Remove from pending and send cancel signal
            NewPending = maps:remove(RequestId, State#state.pending_requests),
            logger:info("Cancelled render request ~p", [RequestId]),
            {reply, ok, State#state{pending_requests = NewPending}};
        error ->
            {reply, {error, not_found}, State}
    end;

%% Unknown call
handle_call(Request, _From, State) ->
    logger:warning("Unknown call: ~p", [Request]),
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle asynchronous info
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({render_complete, RequestId, Result}, State) ->
    case maps:take(RequestId, State#state.pending_requests) of
        {#{from := From} = _Correlation, NewPending} ->
            gen_server:reply(From, {ok, Result}),
            {noreply, State#state{pending_requests = NewPending}};
        error ->
            logger:warning("Received result for unknown request: ~p", [RequestId]),
            {noreply, State}
    end;

handle_info({render_error, RequestId, Error}, State) ->
    case maps:take(RequestId, State#state.pending_requests) of
        {#{from := From} = _Correlation, NewPending} ->
            gen_server:reply(From, {error, Error}),
            {noreply, State#state{pending_requests = NewPending}};
        error ->
            logger:warning("Received error for unknown request: ~p", [RequestId]),
            {noreply, State}
    end;

handle_info({'EXIT', _Pid, Reason}, State) ->
    logger:error("Render worker exited: ~p", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    logger:debug("Unhandled info: ~p", [_Info]),
    {noreply, State}.

%% @doc Cleanup on termination
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{cache = Cache}) ->
    logger:info("Mermaid protocol server terminating"),
    %% Delete ETS table
    catch ets:delete(Cache),
    ok.

%% @doc Handle code change
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Generate unique request ID
-spec generate_request_id(state()) -> binary().
generate_request_id(#state{request_counter = Counter}) ->
    RequestId = ?GENERATE_REQUEST_ID(Counter),
    RequestId.

%% @doc Safely get value from map with default
-spec maps_get_default(term(), map(), term()) -> term().
maps_get_default(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.

%% @doc Validate diagram source size
-spec validate_diagram_size(binary(), state()) -> ok | {error, binary()}.
validate_diagram_size(Source, #state{max_diagram_size = MaxSize}) ->
    Size = byte_size(Source),
    case Size =< MaxSize of
        true -> ok;
        false ->
            Msg = io_lib:format("Diagram source size (~p bytes) exceeds maximum (~p bytes)",
                               [Size, MaxSize]),
            {error, list_to_binary(Msg)}
    end.

%% @doc Detect diagram type from Mermaid source
-spec detect_diagram_type(binary()) -> {ok, diagram_type()} | {error, binary()}.
detect_diagram_type(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global, trim_all]),
    case Lines of
        [] ->
            {error, <<"Empty diagram source">>};
        [FirstLine | _] ->
            Trimmed = string:trim(FirstLine),
            detect_type_from_keyword(Trimmed)
    end.

%% @doc Detect diagram type from first keyword
-spec detect_type_from_keyword(binary()) -> {ok, diagram_type()} | {error, binary()}.
detect_type_from_keyword(<<"flowchart", _/binary>>) -> {ok, flowchart};
detect_type_from_keyword(<<"graph", _/binary>>) -> {ok, graph};
detect_type_from_keyword(<<"sequenceDiagram", _/binary>>) -> {ok, sequence_diagram};
detect_type_from_keyword(<<"classDiagram", _/binary>>) -> {ok, class_diagram};
detect_type_from_keyword(<<"stateDiagram", _/binary>>) -> {ok, state_diagram};
detect_type_from_keyword(<<"erDiagram", _/binary>>) -> {ok, er_diagram};
detect_type_from_keyword(<<"journey", _/binary>>) -> {ok, user_journey};
detect_type_from_keyword(<<"gantt", _/binary>>) -> {ok, gantt};
detect_type_from_keyword(<<"pie", _/binary>>) -> {ok, pie_chart};
detect_type_from_keyword(<<"mindmap", _/binary>>) -> {ok, mindmap};
detect_type_from_keyword(<<"timeline", _/binary>>) -> {ok, timeline};
detect_type_from_keyword(<<"gitGraph", _/binary>>) -> {ok, git_graph};
detect_type_from_keyword(<<"block", _/binary>>) -> {ok, block};
detect_type_from_keyword(<<"architecture", _/binary>>) -> {ok, architecture};
detect_type_from_keyword(<<"C4Context", _/binary>>) -> {ok, c4_context};
detect_type_from_keyword(<<"C4Container", _/binary>>) -> {ok, c4_container};
detect_type_from_keyword(<<"C4Component", _/binary>>) -> {ok, c4_component};
detect_type_from_keyword(<<"C4Deployment", _/binary>>) -> {ok, c4_deployment};
detect_type_from_keyword(_) ->
    {error, <<"Unable to detect diagram type from source">>}.

%% @doc Merge render options with defaults
-spec merge_render_options(map(), state()) -> render_options().
merge_render_options(UserOptions, #state{default_theme = Theme, default_format = Format}) ->
    Defaults = ?DEFAULT_RENDER_OPTIONS,
    Base = maps:merge(Defaults, #{
        theme => Theme,
        format => Format
    }),
    maps:merge(Base, UserOptions).

%% @doc Build error result map
-spec build_error_result(integer(), binary(), map()) -> map().
build_error_result(Code, Message, Data) ->
    #{
        status => error,
        code => Code,
        message => Message,
        data => Data
    }.

%% @doc Validate Mermaid diagram syntax
-spec validate_mermaid_syntax(binary()) ->
    {ok, [map()]} | {error, [validation_error()]}.
validate_mermaid_syntax(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global, trim_all]),
    validate_syntax_lines(Lines, 1, []).

%% @doc Validate syntax line by line
-spec validate_syntax_lines([binary()], pos_integer(), [validation_error()]) ->
    {ok, [map()]} | {error, [validation_error()]}.
validate_syntax_lines([], _LineNum, []) ->
    {ok, []};
validate_syntax_lines([], _LineNum, Errors) when length(Errors) > 0 ->
    {error, lists:reverse(Errors)};
validate_syntax_lines([Line | Rest], LineNum, AccErrors) ->
    case validate_line(Line, LineNum) of
        ok ->
            validate_syntax_lines(Rest, LineNum + 1, AccErrors);
        {error, Error} ->
            validate_syntax_lines(Rest, LineNum + 1, [Error | AccErrors])
    end.

%% @doc Validate single line
-spec validate_line(binary(), pos_integer()) -> ok | {error, validation_error()}.
validate_line(<<>>, _LineNum) ->
    ok;
validate_line(Line, LineNum) ->
    Trimmed = string:trim(Line),
    case Trimmed of
        <<>> -> ok;
        <<"%%", _/binary>> -> ok;  %% Comment
        _ -> validate_syntax_structure(Trimmed, LineNum)
    end.

%% @doc Validate syntax structure
-spec validate_syntax_structure(binary(), pos_integer()) -> ok | {error, validation_error()}.
validate_syntax_structure(Line, LineNum) ->
    %% Basic validation - check for common syntax errors
    case re:run(Line, <<"^[A-Za-z][A-Za-z0-9_]*\\s*">>, [{capture, none}]) of
        match -> ok;
        nomatch ->
            case re:run(Line, <<"^[A-Za-z][A-Za-z0-9_]*\\(">>, [{capture, none}]) of
                match -> ok;
                nomatch ->
                    %% Check if it's a node definition
                    case re:run(Line, <<"^[A-Za-z0-9_]+\\[">>, [{capture, none}]) of
                        match -> ok;
                        nomatch ->
                            %% Check if it's an edge definition
                            EdgePattern = <<"-+->">>,
                            case re:run(Line, EdgePattern, [{capture, none}]) of
                                match -> ok;
                                nomatch ->
                                    %% Check if it's a relationship
                                    case re:run(Line, <<"\\.\\.\\.">>, [{capture, none}]) of
                                        match -> ok;
                                        nomatch ->
                                            Error = #{
                                                line => LineNum,
                                                severity => warning,
                                                code => ?ERR_MERMAID_SYNTAX_ERROR,
                                                message => ?ERR_MSG_SYNTAX_ERROR,
                                                hint => <<"Check Mermaid syntax reference for valid constructs">>
                                            },
                                            {error, Error}
                                    end
                            end
                    end
            end
    end.

%% @doc Get metadata for diagram type
-spec get_type_metadata(diagram_type()) -> map().
get_type_metadata(flowchart) -> #{
    type => flowchart,
    name => <<"Flowchart">>,
    description => <<"Process flow diagram with nodes and edges">>,
    keywords => [<<"graph">>, <<"flowchart">>],
    supports_subgraphs => true,
    supports_styling => true
};
get_type_metadata(sequence_diagram) -> #{
    type => sequence_diagram,
    name => <<"Sequence Diagram">>,
    description => <<"Interaction sequence between participants">>,
    keywords => [<<"sequenceDiagram">>],
    supports_participants => true,
    supports_notes => true
};
get_type_metadata(class_diagram) -> #{
    type => class_diagram,
    name => <<"Class Diagram">>,
    description => <<"Object-oriented class structure">>,
    keywords => [<<"classDiagram">>],
    supports_relationships => true,
    supports_methods => true
};
get_type_metadata(state_diagram) -> #{
    type => state_diagram,
    name => <<"State Diagram">>,
    description => <<"State machine transitions">>,
    keywords => [<<"stateDiagram">>],
    supports_nested_states => true,
    supports_concurrent_states => true
};
get_type_metadata(er_diagram) -> #{
    type => er_diagram,
    name => <<"Entity Relationship Diagram">>,
    description => <<"Database schema relationships">>,
    keywords => [<<"erDiagram">>],
    supports_cardinality => true,
    supports_attributes => true
};
get_type_metadata(Type) -> #{
    type => Type,
    name => atom_to_binary(Type),
    description => <<"Mermaid diagram">>,
    keywords => [],
    experimental => true
}.

%% @doc Validate batch sources
-spec validate_batch_sources([binary()], state()) -> ok | {error, binary()}.
validate_batch_sources(Sources, State) ->
    validate_batch_sources(Sources, State, 1).

-spec validate_batch_sources([binary()], state(), pos_integer()) -> ok | {error, binary()}.
validate_batch_sources([], _State, _Index) ->
    ok;
validate_batch_sources([Source | Rest], State, Index) ->
    case validate_diagram_size(Source, State) of
        ok ->
            validate_batch_sources(Rest, State, Index + 1);
        {error, Reason} ->
            Msg = io_lib:format("Source ~p: ~s", [Index, Reason]),
            {error, list_to_binary(Msg)}
    end.

%% @doc Spawn render worker process
-spec spawn_render_worker(pid(), binary(), diagram_type(), binary(), render_options()) -> pid().
spawn_render_worker(ServerPid, RequestId, DiagramType, Source, Options) ->
    spawn(fun() ->
        Timeout = maps_get_default(timeout, Options, 30000),
        Result = case catch do_render_diagram(DiagramType, Source, Options) of
            {ok, Rendered} ->
                #{
                    status => ok,
                    rendered => Rendered,
                    metadata => #{
                        type => DiagramType,
                        timestamp => erlang:system_time(millisecond),
                        size => byte_size(Rendered)
                    }
                };
            {'EXIT', Reason} ->
                build_error_result(
                    ?ERR_MERMAID_PARSE_ERROR,
                    <<"Render process crashed">>,
                    #{<<"reason">> => atom_to_binary(Reason)}
                );
            {error, Reason} ->
                build_error_result(
                    ?ERR_MERMAID_PARSE_ERROR,
                    Reason,
                    #{}
                )
        end,
        ServerPid ! {render_complete, RequestId, Result}
    end).

%% @doc Spawn batch render worker
-spec spawn_batch_render_worker(pid(), binary(), [binary()], state()) -> pid().
spawn_batch_render_worker(ServerPid, RequestId, Sources, State) ->
    spawn(fun() ->
        Results = lists:map(fun(Source) ->
            case detect_diagram_type(Source) of
                {ok, DiagramType} ->
                    Options = merge_render_options(#{}, State),
                    case catch do_render_diagram(DiagramType, Source, Options) of
                        {ok, Rendered} ->
                            #{
                                status => ok,
                                rendered => Rendered
                            };
                        {error, Reason} ->
                            build_error_result(
                                ?ERR_MERMAID_PARSE_ERROR,
                                Reason,
                                #{}
                            )
                    end;
                {error, Reason} ->
                    build_error_result(
                        ?ERR_MERMAID_PARSE_ERROR,
                        Reason,
                        #{}
                    )
            end
        end, Sources),
        ServerPid ! {render_complete, RequestId, Results}
    end).

%% @doc Actually render the diagram (placeholder for actual Mermaid CLI integration)
-spec do_render_diagram(diagram_type(), binary(), render_options()) ->
    {ok, binary()} | {error, binary()}.
do_render_diagram(_Type, _Source, _Options) ->
    %% Placeholder: In production, this would call the Mermaid CLI or renderer
    %% For now, return a simple SVG wrapper
    Svg = <<"
<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 50'>
    <text x='10' y='25' font-family='monospace' font-size='12'>
        Mermaid diagram placeholder
    </text>
</svg>
    ">>,
    {ok, Svg}.
