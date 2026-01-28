-module(erlmcp_codegen).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    generate/3,
    generate/4,
    extract_definitions/1,
    render_template/3,
    supported_languages/0,
    validate_language/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Supported languages
-define(SUPPORTED_LANGUAGES, [typescript, python, go]).

%% Template directory
-define(TEMPLATE_DIR, "priv/codegen/templates").

%% State record
-record(state, {
    templates = #{} :: #{atom() => binary()},
    cache = #{} :: #{term() => term()}
}).

-type state() :: #state{}.
-type language() :: typescript | python | go.
-type server_definitions() :: #{
    tools := [map()],
    resources := [map()],
    prompts := [map()],
    capabilities := map()
}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Generate client SDK for a server
-spec generate(language(), server_definitions(), file:filename()) ->
    ok | {error, term()}.
generate(Language, Definitions, OutputPath) ->
    generate(Language, Definitions, OutputPath, #{}).

%% @doc Generate client SDK with options
-spec generate(language(), server_definitions(), file:filename(), map()) ->
    ok | {error, term()}.
generate(Language, Definitions, OutputPath, Options) ->
    gen_server:call(?MODULE, {generate, Language, Definitions, OutputPath, Options}, 30000).

%% @doc Extract tool/resource/prompt definitions from a server
-spec extract_definitions(pid()) -> {ok, server_definitions()} | {error, term()}.
extract_definitions(ServerPid) ->
    try
        %% Get tools
        {ok, ToolsResult} = erlmcp_client:call_method(ServerPid, ?MCP_METHOD_TOOLS_LIST, #{}, 5000),
        Tools = maps:get(<<"tools">>, ToolsResult, []),

        %% Get resources
        {ok, ResourcesResult} = erlmcp_client:call_method(ServerPid, ?MCP_METHOD_RESOURCES_LIST, #{}, 5000),
        Resources = maps:get(<<"resources">>, ResourcesResult, []),

        %% Get prompts
        {ok, PromptsResult} = erlmcp_client:call_method(ServerPid, ?MCP_METHOD_PROMPTS_LIST, #{}, 5000),
        Prompts = maps:get(<<"prompts">>, PromptsResult, []),

        %% Get capabilities (from initialize result)
        Capabilities = #{},

        {ok, #{
            tools => Tools,
            resources => Resources,
            prompts => Prompts,
            capabilities => Capabilities
        }}
    catch
        _:Error ->
            {error, {extraction_failed, Error}}
    end.

%% @doc Render a template with data
-spec render_template(language(), server_definitions(), map()) ->
    {ok, binary()} | {error, term()}.
render_template(Language, Definitions, Options) ->
    gen_server:call(?MODULE, {render_template, Language, Definitions, Options}, 30000).

%% @doc Get list of supported languages
-spec supported_languages() -> [language()].
supported_languages() ->
    ?SUPPORTED_LANGUAGES.

%% @doc Validate if language is supported
-spec validate_language(atom()) -> boolean().
validate_language(Language) ->
    lists:member(Language, ?SUPPORTED_LANGUAGES).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Load all templates at startup
    Templates = load_all_templates(),
    {ok, #state{templates = Templates}}.

handle_call({generate, Language, Definitions, OutputPath, Options}, _From, State) ->
    Result = do_generate(Language, Definitions, OutputPath, Options, State),
    {reply, Result, State};

handle_call({render_template, Language, Definitions, Options}, _From, State) ->
    Result = do_render_template(Language, Definitions, Options, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
-spec do_generate(language(), server_definitions(), file:filename(), map(), state()) ->
    ok | {error, term()}.
do_generate(Language, Definitions, OutputPath, Options, State) ->
    case validate_language(Language) of
        false ->
            {error, {unsupported_language, Language}};
        true ->
            case do_render_template(Language, Definitions, Options, State) of
                {ok, Content} ->
                    write_output(Language, Content, OutputPath, Options);
                Error ->
                    Error
            end
    end.

%% @private
-spec do_render_template(language(), server_definitions(), map(), state()) ->
    {ok, binary()} | {error, term()}.
do_render_template(Language, Definitions, Options, State) ->
    case maps:get(Language, State#state.templates, undefined) of
        undefined ->
            {error, {template_not_found, Language}};
        Template ->
            TemplateData = prepare_template_data(Language, Definitions, Options),
            try
                Rendered = bbmustache:render(Template, TemplateData, [
                    {key_type, atom},
                    {escape_fun, fun(X) -> X end}
                ]),
                {ok, Rendered}
            catch
                Error:Reason:Stacktrace ->
                    logger:error("Template rendering failed: ~p:~p~n~p", [Error, Reason, Stacktrace]),
                    {error, {render_failed, Reason}}
            end
    end.

%% @private
-spec prepare_template_data(language(), server_definitions(), map()) -> map().
prepare_template_data(Language, Definitions, Options) ->
    #{
        language => atom_to_binary(Language, utf8),
        package_name => maps:get(package_name, Options, <<"mcp_client">>),
        version => maps:get(version, Options, <<"1.0.0">>),
        tools => prepare_tools(maps:get(tools, Definitions, []), Language),
        resources => prepare_resources(maps:get(resources, Definitions, []), Language),
        prompts => prepare_prompts(maps:get(prompts, Definitions, []), Language),
        has_tools => length(maps:get(tools, Definitions, [])) > 0,
        has_resources => length(maps:get(resources, Definitions, [])) > 0,
        has_prompts => length(maps:get(prompts, Definitions, [])) > 0,
        capabilities => prepare_capabilities(maps:get(capabilities, Definitions, #{}), Language),
        timestamp => iso8601_timestamp(),
        generator_version => <<"1.0.0">>
    }.

%% @private
-spec prepare_tools([map()], language()) -> [map()].
prepare_tools(Tools, Language) ->
    [prepare_tool(Tool, Language) || Tool <- Tools].

%% @private
-spec prepare_tool(map(), language()) -> map().
prepare_tool(Tool, Language) ->
    Name = maps:get(<<"name">>, Tool, <<"unknown">>),
    Description = maps:get(<<"description">>, Tool, <<"">>),
    InputSchema = maps:get(<<"inputSchema">>, Tool, #{}),

    #{
        name => Name,
        method_name => format_method_name(Name, Language),
        description => Description,
        input_schema => InputSchema,
        parameters => extract_parameters(InputSchema, Language),
        has_parameters => map_size(maps:get(<<"properties">>, InputSchema, #{})) > 0
    }.

%% @private
-spec prepare_resources([map()], language()) -> [map()].
prepare_resources(Resources, Language) ->
    [prepare_resource(Resource, Language) || Resource <- Resources].

%% @private
-spec prepare_resource(map(), language()) -> map().
prepare_resource(Resource, Language) ->
    Uri = maps:get(<<"uri">>, Resource, <<"unknown">>),
    Name = maps:get(<<"name">>, Resource, Uri),

    #{
        uri => Uri,
        name => Name,
        method_name => format_method_name(Name, Language),
        description => maps:get(<<"description">>, Resource, <<"">>),
        mime_type => maps:get(<<"mimeType">>, Resource, <<"text/plain">>)
    }.

%% @private
-spec prepare_prompts([map()], language()) -> [map()].
prepare_prompts(Prompts, Language) ->
    [prepare_prompt(Prompt, Language) || Prompt <- Prompts].

%% @private
-spec prepare_prompt(map(), language()) -> map().
prepare_prompt(Prompt, Language) ->
    Name = maps:get(<<"name">>, Prompt, <<"unknown">>),
    Arguments = maps:get(<<"arguments">>, Prompt, []),

    #{
        name => Name,
        method_name => format_method_name(Name, Language),
        description => maps:get(<<"description">>, Prompt, <<"">>),
        arguments => [prepare_argument(Arg, Language) || Arg <- Arguments],
        has_arguments => length(Arguments) > 0
    }.

%% @private
-spec prepare_argument(map(), language()) -> map().
prepare_argument(Arg, Language) ->
    Name = maps:get(<<"name">>, Arg, <<"unknown">>),
    #{
        name => Name,
        param_name => format_param_name(Name, Language),
        description => maps:get(<<"description">>, Arg, <<"">>),
        required => maps:get(<<"required">>, Arg, false)
    }.

%% @private
-spec prepare_capabilities(map(), language()) -> map().
prepare_capabilities(Caps, _Language) ->
    #{
        experimental => maps:get(<<"experimental">>, Caps, #{}),
        roots => maps:get(<<"roots">>, Caps, #{})
    }.

%% @private
-spec extract_parameters(map(), language()) -> [map()].
extract_parameters(Schema, Language) ->
    Properties = maps:get(<<"properties">>, Schema, #{}),
    Required = maps:get(<<"required">>, Schema, []),

    maps:fold(fun(Name, PropSchema, Acc) ->
        IsRequired = lists:member(Name, Required),
        Param = #{
            name => Name,
            param_name => format_param_name(Name, Language),
            type => infer_type(PropSchema, Language),
            description => maps:get(<<"description">>, PropSchema, <<"">>),
            required => IsRequired
        },
        [Param | Acc]
    end, [], Properties).

%% @private
-spec infer_type(map(), language()) -> binary().
infer_type(Schema, typescript) ->
    case maps:get(<<"type">>, Schema, <<"any">>) of
        <<"string">> -> <<"string">>;
        <<"number">> -> <<"number">>;
        <<"integer">> -> <<"number">>;
        <<"boolean">> -> <<"boolean">>;
        <<"array">> -> <<"any[]">>;
        <<"object">> -> <<"Record<string, any>">>;
        _ -> <<"any">>
    end;
infer_type(Schema, python) ->
    case maps:get(<<"type">>, Schema, <<"Any">>) of
        <<"string">> -> <<"str">>;
        <<"number">> -> <<"float">>;
        <<"integer">> -> <<"int">>;
        <<"boolean">> -> <<"bool">>;
        <<"array">> -> <<"List[Any]">>;
        <<"object">> -> <<"Dict[str, Any]">>;
        _ -> <<"Any">>
    end;
infer_type(Schema, go) ->
    case maps:get(<<"type">>, Schema, <<"interface{}">>) of
        <<"string">> -> <<"string">>;
        <<"number">> -> <<"float64">>;
        <<"integer">> -> <<"int64">>;
        <<"boolean">> -> <<"bool">>;
        <<"array">> -> <<"[]interface{}">>;
        <<"object">> -> <<"map[string]interface{}">>;
        _ -> <<"interface{}">>
    end.

%% @private
-spec format_method_name(binary(), language()) -> binary().
format_method_name(Name, typescript) ->
    to_camel_case(Name);
format_method_name(Name, python) ->
    to_snake_case(Name);
format_method_name(Name, go) ->
    to_pascal_case(Name).

%% @private
-spec format_param_name(binary(), language()) -> binary().
format_param_name(Name, typescript) ->
    to_camel_case(Name);
format_param_name(Name, python) ->
    to_snake_case(Name);
format_param_name(Name, go) ->
    to_camel_case(Name).

%% @private
-spec to_camel_case(binary()) -> binary().
to_camel_case(Bin) ->
    Words = binary:split(Bin, [<<"-">>, <<"_">>, <<" ">>], [global]),
    [First | Rest] = Words,
    iolist_to_binary([string:lowercase(First) | [string:titlecase(W) || W <- Rest]]).

%% @private
-spec to_snake_case(binary()) -> binary().
to_snake_case(Bin) ->
    Words = binary:split(Bin, [<<"-">>, <<" ">>], [global]),
    iolist_to_binary(lists:join(<<"_">>, [string:lowercase(W) || W <- Words])).

%% @private
-spec to_pascal_case(binary()) -> binary().
to_pascal_case(Bin) ->
    Words = binary:split(Bin, [<<"-">>, <<"_">>, <<" ">>], [global]),
    iolist_to_binary([string:titlecase(W) || W <- Words]).

%% @private
-spec write_output(language(), binary(), file:filename(), map()) -> ok | {error, term()}.
write_output(Language, Content, OutputPath, Options) ->
    Filename = get_output_filename(Language, OutputPath, Options),
    case filelib:ensure_dir(Filename) of
        ok ->
            case file:write_file(Filename, Content) of
                ok ->
                    logger:info("Generated ~p SDK: ~s", [Language, Filename]),
                    ok;
                {error, Reason} ->
                    {error, {write_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {mkdir_failed, Reason}}
    end.

%% @private
-spec get_output_filename(language(), file:filename(), map()) -> file:filename().
get_output_filename(typescript, OutputPath, Options) ->
    Basename = maps:get(filename, Options, "mcp_client.ts"),
    filename:join(OutputPath, Basename);
get_output_filename(python, OutputPath, Options) ->
    Basename = maps:get(filename, Options, "mcp_client.py"),
    filename:join(OutputPath, Basename);
get_output_filename(go, OutputPath, Options) ->
    Basename = maps:get(filename, Options, "mcp_client.go"),
    filename:join(OutputPath, Basename).

%% @private
-spec load_all_templates() -> #{language() => binary()}.
load_all_templates() ->
    lists:foldl(fun(Lang, Acc) ->
        case load_template(Lang) of
            {ok, Template} ->
                maps:put(Lang, Template, Acc);
            {error, Reason} ->
                logger:warning("Failed to load ~p template: ~p", [Lang, Reason]),
                Acc
        end
    end, #{}, ?SUPPORTED_LANGUAGES).

%% @private
-spec load_template(language()) -> {ok, binary()} | {error, term()}.
load_template(Language) ->
    Filename = atom_to_list(Language) ++ "_client.mustache",

    %% Try multiple paths to find the template
    Paths = [
        %% 1. Installed application priv dir
        case code:priv_dir(erlmcp_core) of
            {error, _} -> undefined;
            Dir -> filename:join([Dir, "codegen", "templates", Filename])
        end,
        %% 2. Development relative path (from repo root)
        filename:join(["priv", "codegen", "templates", Filename]),
        %% 3. Absolute path in current working directory
        filename:join(["/Users/sac/erlmcp/priv", "codegen", "templates", Filename])
    ],

    try_load_from_paths(lists:filter(fun(P) -> P =/= undefined end, Paths)).

%% @private
try_load_from_paths([]) ->
    {error, template_not_found};
try_load_from_paths([Path | Rest]) ->
    case file:read_file(Path) of
        {ok, Content} ->
            logger:debug("Loaded template from: ~s", [Path]),
            {ok, Content};
        {error, _} ->
            try_load_from_paths(Rest)
    end.

%% @private
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                    [Y, M, D, H, Min, S])).
