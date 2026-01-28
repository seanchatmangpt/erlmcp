#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

main(_) ->
    %% Start required applications
    application:ensure_all_started(jsx),
    application:ensure_all_started(bbmustache),

    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),
    io:format("✓ Started erlmcp_codegen~n"),

    %% Test 1: Validate languages
    io:format("~n=== Test 1: Validate Languages ===~n"),
    true = erlmcp_codegen:validate_language(typescript),
    true = erlmcp_codegen:validate_language(python),
    true = erlmcp_codegen:validate_language(go),
    false = erlmcp_codegen:validate_language(rust),
    io:format("✓ Language validation works~n"),

    %% Test 2: Render TypeScript template
    io:format("~n=== Test 2: Render TypeScript Template ===~n"),
    Definitions = #{
        tools => [#{
            <<"name">> => <<"add">>,
            <<"description">> => <<"Add two numbers">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"First">>},
                    <<"b">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Second">>}
                },
                <<"required">> => [<<"a">>, <<"b">>]
            }
        }],
        resources => [#{
            <<"uri">> => <<"file:///config.json">>,
            <<"name">> => <<"config">>,
            <<"description">> => <<"Configuration file">>
        }],
        prompts => [#{
            <<"name">> => <<"summarize">>,
            <<"description">> => <<"Summarize text">>,
            <<"arguments">> => [#{
                <<"name">> => <<"text">>,
                <<"required">> => true
            }]
        }],
        capabilities => #{}
    },

    {ok, TsContent} = erlmcp_codegen:render_template(typescript, Definitions, #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>
    }),
    io:format("✓ Generated TypeScript: ~p bytes~n", [byte_size(TsContent)]),

    %% Verify TypeScript content
    true = binary:match(TsContent, <<"export class McpClient">>) =/= nomatch,
    true = binary:match(TsContent, <<"async add(">>) =/= nomatch,
    io:format("✓ TypeScript content validated~n"),

    %% Test 3: Render Python template
    io:format("~n=== Test 3: Render Python Template ===~n"),
    {ok, PyContent} = erlmcp_codegen:render_template(python, Definitions, #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>
    }),
    io:format("✓ Generated Python: ~p bytes~n", [byte_size(PyContent)]),

    %% Verify Python content
    true = binary:match(PyContent, <<"class McpClient:">>) =/= nomatch,
    true = binary:match(PyContent, <<"async def add(">>) =/= nomatch,
    io:format("✓ Python content validated~n"),

    %% Test 4: Render Go template
    io:format("~n=== Test 4: Render Go Template ===~n"),
    {ok, GoContent} = erlmcp_codegen:render_template(go, Definitions, #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>
    }),
    io:format("✓ Generated Go: ~p bytes~n", [byte_size(GoContent)]),

    %% Verify Go content
    true = binary:match(GoContent, <<"package calculator_client">>) =/= nomatch,
    true = binary:match(GoContent, <<"func (c *McpClient) Add(">>) =/= nomatch,
    io:format("✓ Go content validated~n"),

    %% Test 5: Generate to file
    io:format("~n=== Test 5: Generate TypeScript SDK to File ===~n"),
    OutputDir = "/tmp/erlmcp_codegen_test",
    file:make_dir(OutputDir),

    ok = erlmcp_codegen:generate(typescript, Definitions, OutputDir, #{
        package_name => <<"test_client">>,
        version => <<"1.0.0">>,
        filename => "test_client.ts"
    }),

    OutputFile = filename:join(OutputDir, "test_client.ts"),
    true = filelib:is_file(OutputFile),
    io:format("✓ Generated file: ~s~n", [OutputFile]),

    %% Cleanup
    file:delete(OutputFile),
    file:del_dir(OutputDir),

    io:format("~n=== ALL TESTS PASSED ===~n"),
    halt(0).
