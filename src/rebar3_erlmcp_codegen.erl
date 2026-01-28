-module(rebar3_erlmcp_codegen).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, codegen).
-define(NAMESPACE, erlmcp).
-define(DEPS, [app_discovery]).

%%====================================================================
%% API
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 erlmcp codegen --language typescript --server my_server --output ./client/"},
        {opts, [
            {language, $l, "language", {atom, typescript}, "Target language (typescript, python, go)"},
            {server, $s, "server", {atom, undefined}, "Server module name"},
            {output, $o, "output", string, "Output directory"},
            {package, $p, "package", {string, "mcp_client"}, "Package/module name"},
            {version, $v, "version", {string, "1.0.0"}, "Generated SDK version"}
        ]},
        {short_desc, "Generate MCP client SDKs from server definitions"},
        {desc, "Generate type-safe MCP client SDKs in TypeScript, Python, or Go from an Erlang MCP server definition."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),

    Language = proplists:get_value(language, Args, typescript),
    ServerModule = proplists:get_value(server, Args),
    Output = proplists:get_value(output, Args),
    Package = proplists:get_value(package, Args, "mcp_client"),
    Version = proplists:get_value(version, Args, "1.0.0"),

    case validate_args(Language, ServerModule, Output) of
        ok ->
            case generate_sdk(Language, ServerModule, Output, Package, Version, State) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    {error, format_error(Reason)}
            end;
        {error, Reason} ->
            {error, format_error(Reason)}
    end.

-spec format_error(any()) -> iolist().
format_error({missing_arg, Arg}) ->
    io_lib:format("Missing required argument: --~p", [Arg]);
format_error({unsupported_language, Lang}) ->
    io_lib:format("Unsupported language: ~p. Supported: typescript, python, go", [Lang]);
format_error({server_not_found, Module}) ->
    io_lib:format("Server module not found: ~p", [Module]);
format_error({generation_failed, Reason}) ->
    io_lib:format("SDK generation failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec validate_args(atom(), atom() | undefined, string() | undefined) ->
    ok | {error, term()}.
validate_args(_Language, undefined, _Output) ->
    {error, {missing_arg, server}};
validate_args(_Language, _Server, undefined) ->
    {error, {missing_arg, output}};
validate_args(Language, _Server, _Output) ->
    case erlmcp_codegen:validate_language(Language) of
        true -> ok;
        false -> {error, {unsupported_language, Language}}
    end.

-spec generate_sdk(atom(), atom(), string(), string(), string(), rebar_state:t()) ->
    ok | {error, term()}.
generate_sdk(Language, ServerModule, Output, Package, Version, State) ->
    rebar_api:info("Generating ~p SDK from ~p...", [Language, ServerModule]),

    %% Start the server to extract definitions
    case start_server_for_extraction(ServerModule, State) of
        {ok, ServerPid} ->
            try
                case erlmcp_codegen:extract_definitions(ServerPid) of
                    {ok, Definitions} ->
                        Options = #{
                            package_name => list_to_binary(Package),
                            version => list_to_binary(Version)
                        },
                        case erlmcp_codegen:generate(Language, Definitions, Output, Options) of
                            ok ->
                                rebar_api:info("Successfully generated ~p SDK in ~s", [Language, Output]),
                                ok;
                            {error, Reason} ->
                                rebar_api:error("Generation failed: ~p", [Reason]),
                                {error, {generation_failed, Reason}}
                        end;
                    {error, Reason} ->
                        rebar_api:error("Failed to extract definitions: ~p", [Reason]),
                        {error, {extraction_failed, Reason}}
                end
            after
                stop_server(ServerPid)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec start_server_for_extraction(atom(), rebar_state:t()) ->
    {ok, pid()} | {error, term()}.
start_server_for_extraction(ServerModule, State) ->
    %% Ensure application is loaded
    AppDir = rebar_state:dir(State),
    code:add_pathz(filename:join([AppDir, "ebin"])),

    case code:ensure_loaded(ServerModule) of
        {module, _} ->
            %% Start the server
            case apply(ServerModule, start_link, []) of
                {ok, Pid} ->
                    rebar_api:debug("Started server ~p with pid ~p", [ServerModule, Pid]),
                    {ok, Pid};
                {error, Reason} ->
                    {error, {server_start_failed, Reason}}
            end;
        {error, _} ->
            {error, {server_not_found, ServerModule}}
    end.

-spec stop_server(pid()) -> ok.
stop_server(Pid) ->
    case is_process_alive(Pid) of
        true ->
            erlmcp_server:stop(Pid);
        false ->
            ok
    end.
