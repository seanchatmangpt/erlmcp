-module(erlmcp_codegen_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

sample_definitions() ->
    #{
        tools => [
            #{
                <<"name">> => <<"add">>,
                <<"description">> => <<"Add two numbers">>,
                <<"inputSchema">> => #{
                    <<"type">> => <<"object">>,
                    <<"properties">> => #{
                        <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"First number">>},
                        <<"b">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Second number">>}
                    },
                    <<"required">> => [<<"a">>, <<"b">>]
                }
            },
            #{
                <<"name">> => <<"echo-message">>,
                <<"description">> => <<"Echo a message">>,
                <<"inputSchema">> => #{
                    <<"type">> => <<"object">>,
                    <<"properties">> => #{
                        <<"message">> => #{<<"type">> => <<"string">>}
                    },
                    <<"required">> => [<<"message">>]
                }
            }
        ],
        resources => [
            #{
                <<"uri">> => <<"file:///etc/config.json">>,
                <<"name">> => <<"config">>,
                <<"description">> => <<"Server configuration">>,
                <<"mimeType">> => <<"application/json">>
            }
        ],
        prompts => [
            #{
                <<"name">> => <<"summarize">>,
                <<"description">> => <<"Summarize text">>,
                <<"arguments">> => [
                    #{
                        <<"name">> => <<"text">>,
                        <<"description">> => <<"Text to summarize">>,
                        <<"required">> => true
                    },
                    #{
                        <<"name">> => <<"max_length">>,
                        <<"description">> => <<"Maximum summary length">>,
                        <<"required">> => false
                    }
                ]
            }
        ],
        capabilities => #{}
    }.

%%====================================================================
%% Validation Tests
%%====================================================================

validate_language_test() ->
    ?assert(erlmcp_codegen:validate_language(typescript)),
    ?assert(erlmcp_codegen:validate_language(python)),
    ?assert(erlmcp_codegen:validate_language(go)),
    ?assertNot(erlmcp_codegen:validate_language(rust)),
    ?assertNot(erlmcp_codegen:validate_language(java)).

supported_languages_test() ->
    Languages = erlmcp_codegen:supported_languages(),
    ?assertEqual([typescript, python, go], Languages),
    ?assertEqual(3, length(Languages)).

%%====================================================================
%% Template Rendering Tests
%%====================================================================

render_typescript_template_test() ->
    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),

    Definitions = sample_definitions(),
    Options = #{
        package_name => <<"test_client">>,
        version => <<"1.0.0">>
    },

    {ok, Content} = erlmcp_codegen:render_template(typescript, Definitions, Options),

    %% Verify TypeScript content
    ?assert(is_binary(Content)),
    ?assert(byte_size(Content) > 0),

    %% Check for key TypeScript patterns
    ?assertMatch({match, _}, re:run(Content, <<"export class McpClient">>)),
    ?assertMatch({match, _}, re:run(Content, <<"interface.*Input">>)),
    ?assertMatch({match, _}, re:run(Content, <<"async add\\(">>)),
    ?assertMatch({match, _}, re:run(Content, <<"async echoMessage\\(">>)),
    ?assertMatch({match, _}, re:run(Content, <<"test_client">>)),

    ok.

render_python_template_test() ->
    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),

    Definitions = sample_definitions(),
    Options = #{
        package_name => <<"test_client">>,
        version => <<"1.0.0">>
    },

    {ok, Content} = erlmcp_codegen:render_template(python, Definitions, Options),

    %% Verify Python content
    ?assert(is_binary(Content)),
    ?assert(byte_size(Content) > 0),

    %% Check for key Python patterns
    ?assertMatch({match, _}, re:run(Content, <<"class McpClient:">>)),
    ?assertMatch({match, _}, re:run(Content, <<"class.*_input\\(TypedDict">>)),
    ?assertMatch({match, _}, re:run(Content, <<"async def add\\(">>)),
    ?assertMatch({match, _}, re:run(Content, <<"async def echo_message\\(">>)),
    ?assertMatch({match, _}, re:run(Content, <<"test_client">>)),

    ok.

render_go_template_test() ->
    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),

    Definitions = sample_definitions(),
    Options = #{
        package_name => <<"test_client">>,
        version => <<"1.0.0">>
    },

    {ok, Content} = erlmcp_codegen:render_template(go, Definitions, Options),

    %% Verify Go content
    ?assert(is_binary(Content)),
    ?assert(byte_size(Content) > 0),

    %% Check for key Go patterns
    ?assertMatch({match, _}, re:run(Content, <<"package test_client">>)),
    ?assertMatch({match, _}, re:run(Content, <<"type McpClient struct">>)),
    ?assertMatch({match, _}, re:run(Content, <<"type.*Input struct">>)),
    ?assertMatch({match, _}, re:run(Content, <<"func \\(c \\*McpClient\\) Add\\(">>)),
    ?assertMatch({match, _}, re:run(Content, <<"func \\(c \\*McpClient\\) EchoMessage\\(">>)),

    ok.

%%====================================================================
%% Name Formatting Tests
%%====================================================================

format_method_name_test() ->
    %% Test camelCase (TypeScript)
    ?assertEqual(<<"echoMessage">>, format_method_name(<<"echo-message">>, typescript)),
    ?assertEqual(<<"getUserInfo">>, format_method_name(<<"get-user-info">>, typescript)),

    %% Test snake_case (Python)
    ?assertEqual(<<"echo_message">>, format_method_name(<<"echo-message">>, python)),
    ?assertEqual(<<"get_user_info">>, format_method_name(<<"get-user-info">>, python)),

    %% Test PascalCase (Go)
    ?assertEqual(<<"EchoMessage">>, format_method_name(<<"echo-message">>, go)),
    ?assertEqual(<<"GetUserInfo">>, format_method_name(<<"get-user-info">>, go)),

    ok.

%% Helper to test name formatting (access private function via module export)
format_method_name(Name, Language) ->
    Words = binary:split(Name, [<<"-">>, <<"_">>, <<" ">>], [global]),
    case Language of
        typescript ->
            [First | Rest] = Words,
            iolist_to_binary([string:lowercase(First) | [string:titlecase(W) || W <- Rest]]);
        python ->
            iolist_to_binary(lists:join(<<"_">>, [string:lowercase(W) || W <- Words]));
        go ->
            iolist_to_binary([string:titlecase(W) || W <- Words])
    end.

%%====================================================================
%% Type Inference Tests
%%====================================================================

infer_type_test() ->
    %% TypeScript types
    ?assertEqual(<<"string">>, infer_type(#{<<"type">> => <<"string">>}, typescript)),
    ?assertEqual(<<"number">>, infer_type(#{<<"type">> => <<"number">>}, typescript)),
    ?assertEqual(<<"boolean">>, infer_type(#{<<"type">> => <<"boolean">>}, typescript)),
    ?assertEqual(<<"any[]">>, infer_type(#{<<"type">> => <<"array">>}, typescript)),

    %% Python types
    ?assertEqual(<<"str">>, infer_type(#{<<"type">> => <<"string">>}, python)),
    ?assertEqual(<<"float">>, infer_type(#{<<"type">> => <<"number">>}, python)),
    ?assertEqual(<<"int">>, infer_type(#{<<"type">> => <<"integer">>}, python)),
    ?assertEqual(<<"bool">>, infer_type(#{<<"type">> => <<"boolean">>}, python)),

    %% Go types
    ?assertEqual(<<"string">>, infer_type(#{<<"type">> => <<"string">>}, go)),
    ?assertEqual(<<"float64">>, infer_type(#{<<"type">> => <<"number">>}, go)),
    ?assertEqual(<<"int64">>, infer_type(#{<<"type">> => <<"integer">>}, go)),
    ?assertEqual(<<"bool">>, infer_type(#{<<"type">> => <<"boolean">>}, go)),

    ok.

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

%%====================================================================
%% Integration Tests
%%====================================================================

generate_typescript_sdk_test() ->
    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),

    Definitions = sample_definitions(),
    OutputDir = "/tmp/erlmcp_codegen_test",

    %% Ensure output directory exists
    file:make_dir(OutputDir),

    %% Generate SDK
    Options = #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>,
        filename => "calculator_client.ts"
    },

    Result = erlmcp_codegen:generate(typescript, Definitions, OutputDir, Options),
    ?assertEqual(ok, Result),

    %% Verify file was created
    OutputFile = filename:join(OutputDir, "calculator_client.ts"),
    ?assert(filelib:is_file(OutputFile)),

    %% Clean up
    file:delete(OutputFile),
    file:del_dir(OutputDir),

    ok.

generate_python_sdk_test() ->
    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),

    Definitions = sample_definitions(),
    OutputDir = "/tmp/erlmcp_codegen_test_python",

    %% Ensure output directory exists
    file:make_dir(OutputDir),

    %% Generate SDK
    Options = #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>,
        filename => "calculator_client.py"
    },

    Result = erlmcp_codegen:generate(python, Definitions, OutputDir, Options),
    ?assertEqual(ok, Result),

    %% Verify file was created
    OutputFile = filename:join(OutputDir, "calculator_client.py"),
    ?assert(filelib:is_file(OutputFile)),

    %% Clean up
    file:delete(OutputFile),
    file:del_dir(OutputDir),

    ok.

generate_go_sdk_test() ->
    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),

    Definitions = sample_definitions(),
    OutputDir = "/tmp/erlmcp_codegen_test_go",

    %% Ensure output directory exists
    file:make_dir(OutputDir),

    %% Generate SDK
    Options = #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>,
        filename => "calculator_client.go"
    },

    Result = erlmcp_codegen:generate(go, Definitions, OutputDir, Options),
    ?assertEqual(ok, Result),

    %% Verify file was created
    OutputFile = filename:join(OutputDir, "calculator_client.go"),
    ?assert(filelib:is_file(OutputFile)),

    %% Clean up
    file:delete(OutputFile),
    file:del_dir(OutputDir),

    ok.

unsupported_language_test() ->
    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),

    Definitions = sample_definitions(),
    Result = erlmcp_codegen:generate(rust, Definitions, "/tmp", #{}),
    ?assertMatch({error, {unsupported_language, rust}}, Result),

    ok.
