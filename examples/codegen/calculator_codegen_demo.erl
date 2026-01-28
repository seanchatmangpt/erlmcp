%%%====================================================================
%%% Calculator Server Client SDK Code Generation Demo
%%%====================================================================
%%% Demonstrates generating TypeScript, Python, and Go client SDKs
%%% from the calculator server definition.
%%%====================================================================

-module(calculator_codegen_demo).
-export([generate_all_sdks/0, generate_sdk/1]).

%% @doc Generate all client SDKs for calculator server
generate_all_sdks() ->
    %% Start codegen server
    {ok, _Pid} = erlmcp_codegen:start_link(),

    %% Define calculator server capabilities
    Definitions = #{
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
                <<"name">> => <<"subtract">>,
                <<"description">> => <<"Subtract two numbers">>,
                <<"inputSchema">> => #{
                    <<"type">> => <<"object">>,
                    <<"properties">> => #{
                        <<"a">> => #{<<"type">> => <<"number">>},
                        <<"b">> => #{<<"type">> => <<"number">>}
                    },
                    <<"required">> => [<<"a">>, <<"b">>]
                }
            },
            #{
                <<"name">> => <<"multiply">>,
                <<"description">> => <<"Multiply two numbers">>,
                <<"inputSchema">> => #{
                    <<"type">> => <<"object">>,
                    <<"properties">> => #{
                        <<"a">> => #{<<"type">> => <<"number">>},
                        <<"b">> => #{<<"type">> => <<"number">>}
                    },
                    <<"required">> => [<<"a">>, <<"b">>]
                }
            },
            #{
                <<"name">> => <<"divide">>,
                <<"description">> => <<"Divide two numbers">>,
                <<"inputSchema">> => #{
                    <<"type">> => <<"object">>,
                    <<"properties">> => #{
                        <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Dividend">>},
                        <<"b">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Divisor">>}
                    },
                    <<"required">> => [<<"a">>, <<"b">>]
                }
            }
        ],
        resources => [
            #{
                <<"uri">> => <<"calculator://history">>,
                <<"name">> => <<"history">>,
                <<"description">> => <<"Calculation history">>,
                <<"mimeType">> => <<"application/json">>
            }
        ],
        prompts => [],
        capabilities => #{}
    },

    %% Generate SDKs for all languages
    OutputDir = "./generated_sdks",
    file:make_dir(OutputDir),

    io:format("=== Generating Calculator Client SDKs ===~n~n"),

    %% TypeScript
    io:format("Generating TypeScript SDK...~n"),
    ok = erlmcp_codegen:generate(typescript, Definitions, OutputDir, #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>,
        filename => "calculator_client.ts"
    }),
    io:format("✓ TypeScript SDK: ~s/calculator_client.ts~n~n", [OutputDir]),

    %% Python
    io:format("Generating Python SDK...~n"),
    ok = erlmcp_codegen:generate(python, Definitions, OutputDir, #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>,
        filename => "calculator_client.py"
    }),
    io:format("✓ Python SDK: ~s/calculator_client.py~n~n", [OutputDir]),

    %% Go
    io:format("Generating Go SDK...~n"),
    ok = erlmcp_codegen:generate(go, Definitions, OutputDir, #{
        package_name => <<"calculator_client">>,
        version => <<"1.0.0">>,
        filename => "calculator_client.go"
    }),
    io:format("✓ Go SDK: ~s/calculator_client.go~n~n", [OutputDir]),

    io:format("=== Generation Complete! ===~n"),
    io:format("Generated SDKs are in: ~s/~n", [OutputDir]),

    {ok, OutputDir}.

%% @doc Generate SDK for a specific language
generate_sdk(Language) when Language =:= typescript; Language =:= python; Language =:= go ->
    {ok, _OutputDir} = generate_all_sdks(),
    ok.
