-module(erlmcp_mermaid_parser).
-behaviour(gen_server).

%% API
-export([start_link/0,
         parse/2,
         validate/2,
         detect_type/1,
         extract_metadata/1,
         check_syntax/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%% State record
-record(state, {
    mermaid_cli :: binary() | undefined,
    version :: binary()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the parser server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Parse diagram and return AST or error
-spec parse(binary(), binary()) ->
    {ok, map()} | {error, #{reason := binary(), line => integer(), column => integer()}}.
parse(DiagramCode, DiagramType) ->
    gen_server:call(?SERVER, {parse, DiagramCode, DiagramType}).

%% @doc Validate diagram syntax only (no AST)
-spec validate(binary(), binary()) ->
    {ok, valid} | {error, #{reason := binary(), location => #{line := integer(), column := integer()}}}.
validate(DiagramCode, DiagramType) ->
    gen_server:call(?SERVER, {validate, DiagramCode, DiagramType}).

%% @doc Detect diagram type from syntax
-spec detect_type(binary()) -> {ok, binary()} | {error, unknown_type}.
detect_type(DiagramCode) ->
    gen_server:call(?SERVER, {detect_type, DiagramCode}).

%% @doc Extract diagram metadata
-spec extract_metadata(binary()) -> {ok, map()}.
extract_metadata(DiagramCode) ->
    gen_server:call(?SERVER, {extract_metadata, DiagramCode}).

%% @doc Check syntax with Mermaid CLI
-spec check_syntax(binary()) -> {ok, valid} | {error, map()}.
check_syntax(DiagramCode) ->
    gen_server:call(?SERVER, {check_syntax, DiagramCode}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    % Check if Mermaid CLI is available
    MermaidPath = case os:find_executable("npx") of
        false -> undefined;
        NpxPath -> list_to_binary(NpxPath)
    end,

    Version = detect_mermaid_version(),

    {ok, #state{
        mermaid_cli = MermaidPath,
        version = Version
    }}.

%% @private
handle_call({parse, DiagramCode, DiagramType}, _From, State) ->
    case validate(DiagramCode, DiagramType) of
        {ok, valid} ->
            AST = extract_ast(DiagramCode, DiagramType),
            {reply, {ok, AST}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({validate, DiagramCode, DiagramType}, _From, State) ->
    case State#state.mermaid_cli of
        undefined ->
            % Fallback to basic validation
            Result = validate_basic(DiagramCode, DiagramType),
            {reply, Result, State};
        _ ->
            % Use Mermaid CLI for validation
            Result = validate_with_cli(DiagramCode, DiagramType, State),
            {reply, Result, State}
    end;

handle_call({detect_type, DiagramCode}, _From, State) ->
    Result = do_detect_type(DiagramCode),
    {reply, Result, State};

handle_call({extract_metadata, DiagramCode}, _From, State) ->
    Meta = do_extract_metadata(DiagramCode),
    {reply, {ok, Meta}, State};

handle_call({check_syntax, DiagramCode}, _From, State) ->
    case State#state.mermaid_cli of
        undefined ->
            {reply, {error, #{reason => <<"mermaid_cli_not_available">>}}, State};
        _ ->
            Result = do_check_syntax(DiagramCode),
            {reply, Result, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
detect_mermaid_version() ->
    case os:cmd("npx mermaid --version 2>&1") of
        [] ->
            <<"not_installed">>;
        Output ->
            case re:run(Output, "v(\\d+\\.\\d+\\.\\d+)", [{capture, all_but_first, list}]) of
                {match, [Version]} -> list_to_binary(Version);
                nomatch -> <<"unknown">>
            end
    end.

%% @private
validate_with_cli(DiagramCode, DiagramType, State) ->
    % Write diagram to temp file
    TempFile = create_temp_file(DiagramCode),

    % Run Mermaid CLI to validate
    Cmd = io_lib:format("npx mermaid ~s -c ~s 2>&1", [DiagramType, TempFile]),
    Output = os:cmd(Cmd),

    % Clean up temp file
    file:delete(TempFile),

    case Output of
        [] ->
            {ok, valid};
        ErrorOutput ->
            parse_error_output(ErrorOutput)
    end.

%% @private
validate_basic(DiagramCode, DiagramType) ->
    % Basic syntax validation without CLI
    case check_diagram_structure(DiagramCode, DiagramType) of
        {ok, valid} ->
            check_common_errors(DiagramCode);
        {error, _} = Error ->
            Error
    end.

%% @private
check_diagram_structure(Code, Type) ->
    TypeRegex = case Type of
        <<"flowchart">> -> "^graph (TD|LR|TB|RL)|^flowchart (TD|LR|TB|RL)";
        <<"sequenceDiagram">> -> "^sequenceDiagram";
        <<"gantt">> -> "^gantt";
        <<"classDiagram">> -> "^classDiagram";
        <<"stateDiagram-v2">> -> "^stateDiagram-v2";
        <<"stateDiagram">> -> "^stateDiagram";
        <<"erDiagram">> -> "^erDiagram";
        <<"pie">> -> "^pie";
        <<"mindmap">> -> "^mindmap";
        <<"gitgraph">> -> "^gitgraph";
        <<"journey">> -> "^journey";
        _ -> "graph|sequenceDiagram|gantt|classDiagram|stateDiagram|erDiagram|pie|mindmap|gitgraph|journey"
    end,

    case re:run(Code, TypeRegex, [{capture, none}, caseless]) of
        match -> {ok, valid};
        nomatch -> {error, #{reason => <<"invalid_diagram_type_or_structure">>}}
    end.

%% @private
check_common_errors(Code) ->
    % Check for common syntax errors
    Checks = [
        fun check_unmatched_brackets/1,
        fun check_invalid_edges/1,
        fun check_empty_statements/1
    ],

    lists:foldl(fun(Fun, Acc) ->
        case Acc of
            {ok, valid} -> Fun(Code);
            Error -> Error
        end
    end, {ok, valid}, Checks).

%% @private
check_unmatched_brackets(Code) ->
    OpenBrackets = length(re:split(Code, "\\[", [{return, list}])) - 1,
    CloseBrackets = length(re:split(Code, "\\]", [{return, list}])) - 1,

    case OpenBrackets =:= CloseBrackets of
        true -> {ok, valid};
        false ->
            {error, #{
                reason => <<"unmatched_brackets">>,
                hint => iolist_to_binary(io_lib:format("Open: ~p, Close: ~p", [OpenBrackets, CloseBrackets]))
            }}
    end.

%% @private
check_invalid_edges(Code) ->
    % Check for invalid edge patterns
    case re:run(Code, "\\s-\\s[^->|]", [global, {capture, first}]) of
        nomatch -> {ok, valid};
        {match, _} ->
            {error, #{
                reason => <<"invalid_edge_syntax">>,
                hint => <<"Edge must use -->, -->, or similar syntax">>
            }}
    end.

%% @private
check_empty_statements(Code) ->
    Lines = binary:split(Code, <<"\n">>, [global]),
    HasContent = lists:any(fun(Line) ->
        Trimmed = binary:trim(Line),
        byte_size(Trimmed) > 0 andalso binary:at(Trimmed, 0) =/= $%
    end, Lines),

    case HasContent of
        true -> {ok, valid};
        false -> {error, #{reason => <<"empty_diagram">>}}
    end.

%% @private
parse_error_output(ErrorOutput) ->
    % Try to extract line and column from Mermaid CLI error
    case re:run(ErrorOutput, "line (\\d+), column (\\d+)", [{capture, all, list}]) of
        {match, [_, Line, Col]} ->
            {error, #{
                reason => <<"syntax_error">>,
                line => list_to_integer(Line),
                column => list_to_integer(Col),
                message => list_to_binary(ErrorOutput)
            }};
        nomatch ->
            % Try other error patterns
            case re:run(ErrorOutput, "Parse error at line (\\d+)", [{capture, all, list}]) of
                {match, [_, Line]} ->
                    {error, #{
                        reason => <<"parse_error">>,
                        line => list_to_integer(Line),
                        message => list_to_binary(ErrorOutput)
                    }};
                nomatch ->
                    {error, #{
                        reason => <<"syntax_error">>,
                        message => list_to_binary(ErrorOutput)
                    }}
            end
    end.

%% @private
do_detect_type(DiagramCode) ->
    % Extract first non-comment line
    Lines = binary:split(DiagramCode, <<"\n">>, [global]),
    FirstLine = find_first_content_line(Lines),

    case FirstLine of
        undefined ->
            {error, unknown_type};
        Line ->
            case re:run(Line, "^(graph|flowchart|sequenceDiagram|gantt|classDiagram|stateDiagram|erDiagram|pie|mindmap|gitgraph|journey)", [{capture, all_but_first, list}, caseless]) of
                {match, [Type]} ->
                    CanonicalType = canonicalize_type(list_to_binary(Type)),
                    {ok, CanonicalType};
                nomatch ->
                    {error, unknown_type}
            end
    end.

%% @private
find_first_content_line(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case Acc of
            undefined ->
                Trimmed = binary:trim(Line),
                case byte_size(Trimmed) > 0 andalso binary:at(Trimmed, 0) =/= $% of
                    true -> Trimmed;
                    false -> undefined
                end;
            _ ->
                Acc
        end
    end, undefined, Lines).

%% @private
canonicalize_type(<<"graph">>) -> <<"flowchart">>;
canonicalize_type(<<"flowchart">>) -> <<"flowchart">>;
canonicalize_type(<<"sequence">>) -> <<"sequenceDiagram">>;
canonicalize_type(<<"sequenceDiagram">>) -> <<"sequenceDiagram">>;
canonicalize_type(<<"state">>) -> <<"stateDiagram-v2">>;
canonicalize_type(<<"stateDiagram">>) -> <<"stateDiagram-v2">>;
canonicalize_type(<<"stateDiagram-v2">>) -> <<"stateDiagram-v2">>;
canonicalize_type(<<"er">>) -> <<"erDiagram">>;
canonicalize_type(<<"erDiagram">>) -> <<"erDiagram">>;
canonicalize_type(<<"class">>) -> <<"classDiagram">>;
canonicalize_type(<<"classDiagram">>) -> <<"classDiagram">>;
canonicalize_type(Type) -> Type.

%% @private
do_extract_metadata(DiagramCode) ->
    #{
        line_count => count_lines(DiagramCode),
        char_count => byte_size(DiagramCode),
        node_count => count_nodes(DiagramCode),
        edge_count => count_edges(DiagramCode),
        complexity => calculate_complexity(DiagramCode)
    }.

%% @private
count_lines(Code) ->
    length(binary:split(Code, <<"\n">>, [global])).

%% @private
count_nodes(Code) ->
    % Count various node patterns
    Patterns = [
        "\\[[^\\]]+\\]",  % Square brackets
        "\\([^\\)]+\\)",  % Parentheses
        "\\{[^\\}]+\\}",  % Curly braces
        "\\[\\[[^\\]]+\\]\\]", % Double square brackets
        "\\(\\([^\\)]+\\)\\)"  % Double parentheses
    ],
    lists:foldl(fun(Pattern, Acc) ->
        case re:run(Code, Pattern, [global]) of
            {match, Matches} -> Acc + length(Matches);
            nomatch -> Acc
        end
    end, 0, Patterns).

%% @private
count_edges(Code) ->
    Patterns = [
        "-->", "->", "---->",
        "~>", "-->>",
        "\\|\\|", "\\|\\|\\|", "\\|\\|\\|\\|",
        ">", "\\.", "\\.-", "\\.-."
    ],
    lists:foldl(fun(Pattern, Acc) ->
        % Need to escape special regex chars
        EscapedPattern = re:replace(Pattern, "[()|\\[\\]{}^$*+?\\\\]", "\\\\\\&", [global, {return, list}]),
        case re:run(Code, EscapedPattern, [global]) of
            {match, Matches} -> Acc + length(Matches);
            nomatch -> Acc
        end
    end, 0, Patterns).

%% @private
calculate_complexity(Code) ->
    Meta = do_extract_metadata(Code),
    maps:get(node_count, Meta, 0) * 2 + maps:get(edge_count, Meta, 0).

%% @private
extract_ast(Code, Type) ->
    #{
        type => Type,
        nodes => extract_nodes(Code),
        edges => extract_edges(Code),
        subgraphs => extract_subgraphs(Code),
        metadata => do_extract_metadata(Code)
    }.

%% @private
extract_nodes(Code) ->
    % Extract node definitions
    Patterns = [
        "([a-zA-Z0-9_-]+)\\[([^\\]]+)\\]",  % A[Label]
        "([a-zA-Z0-9_-]+)\\(([^\\)]+)\\)",  % A(Label)
        "([a-zA-Z0-9_-]+)\\{([^\\}]+)\\}",  % A{Label}
        "\\[\\[([^\\]]+)\\]\\]",  % [[Label]]
        "\\(\\(([^\\)]+)\\)\\)"  % ((Label))
    ],
    lists:foldl(fun(Pattern, Acc) ->
        case re:run(Code, Pattern, [global, {capture, all_but_first, list}]) of
            {match, Matches} ->
                Nodes = lists:map(fun
                    ([Id, Label]) -> #{id => list_to_binary(Id), label => list_to_binary(Label)};
                    ([Label]) -> #{id => list_to_binary(Label), label => list_to_binary(Label)}
                end, Matches),
                maps:merge(Acc, maps:from_list([{list_to_binary(Id), N} || N = #{id := Id} <- Nodes]));
            nomatch ->
                Acc
        end
    end, #{}, Patterns).

%% @private
extract_edges(Code) ->
    % Extract edge definitions
    Patterns = [
        "([a-zA-Z0-9_-]+)-->([a-zA-Z0-9_-]+)",
        "([a-zA-Z0-9_-]+)-->([a-zA-Z0-9_-]+)",
        "([a-zA-Z0-9_-]+)~> ([a-zA-Z0-9_-]+)"
    ],
    lists:foldl(fun(Pattern, Acc) ->
        case re:run(Code, Pattern, [global, {capture, all_but_first, list}]) of
            {match, Matches} ->
                Edges = lists:map(fun([From, To]) ->
                    #{
                        from => list_to_binary(From),
                        to => list_to_binary(To),
                        type => edge
                    }
                end, Matches),
                Edges ++ Acc;
            nomatch ->
                Acc
        end
    end, [], Patterns).

%% @private
extract_subgraphs(Code) ->
    % Extract subgraph definitions
    case re:run(Code, "subgraph ([^\\{]+)\\{([^\\}]+)\\}", [global, {capture, all_but_first, list}, caseless]) of
        {match, Matches} ->
            lists:map(fun([Title, Content]) ->
                #{
                    title => list_to_binary(Title),
                    content => list_to_binary(Content)
                }
            end, Matches);
        nomatch ->
            []
    end.

%% @private
create_temp_file(Content) ->
    TempDir = case application:get_env(erlmcp_core, mermaid_temp_dir) of
        {ok, Dir} -> Dir;
        undefined -> "/tmp"
    end,
    FileName = "mermaid-" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".mmd",
    FilePath = filename:join(TempDir, FileName),
    ok = file:write_file(FilePath, Content),
    FilePath.

%% @private
do_check_syntax(DiagramCode) ->
    TempFile = create_temp_file(DiagramCode),
    Cmd = io_lib:format("npx mermaid mmd -c ~s 2>&1", [TempFile]),
    Output = os:cmd(Cmd),
    file:delete(TempFile),

    case Output of
        [] -> {ok, valid};
        _ -> parse_error_output(Output)
    end.
