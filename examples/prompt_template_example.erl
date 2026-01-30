-module(prompt_template_example).
-export([run/0]).

%% Example usage of erlmcp_prompt_template module
%% Demonstrates the prompt string templating engine for MCP Prompts API

run() ->
    io:format("~n=== Prompt Template Examples ===~n~n"),

    example_1_simple_variable(),
    example_2_multiple_variables(),
    example_3_essay_prompt(),
    example_4_code_review_prompt(),
    example_5_with_sections(),
    example_6_validation(),
    example_7_integration_with_server(),

    io:format("~n=== Examples Complete ===~n~n").

%% Example 1: Simple variable interpolation
example_1_simple_variable() ->
    io:format("Example 1: Simple Variable Interpolation~n"),
    io:format("-------------------------------------------~n"),

    Template = <<"Hello {{name}}!">>,
    Variables = #{<<"name">> => <<"World">>},

    Result = erlmcp_prompt_template:render(Template, Variables),

    io:format("Template:  ~s~n", [Template]),
    io:format("Variables: ~p~n", [Variables]),
    io:format("Result:    ~s~n~n", [Result]).

%% Example 2: Multiple variables
example_2_multiple_variables() ->
    io:format("Example 2: Multiple Variables~n"),
    io:format("-------------------------------~n"),

    Template = <<"{{greeting}} {{name}}, welcome to {{place}}!">>,
    Variables = #{
        <<"greeting">> => <<"Hello">>,
        <<"name">> => <<"Alice">>,
        <<"place">> => <<"Wonderland">>
    },

    Result = erlmcp_prompt_template:render(Template, Variables),

    io:format("Template:  ~s~n", [Template]),
    io:format("Variables: ~p~n", [Variables]),
    io:format("Result:    ~s~n~n", [Result]).

%% Example 3: Essay writing prompt (from protocol docs)
example_3_essay_prompt() ->
    io:format("Example 3: Essay Writing Prompt~n"),
    io:format("--------------------------------~n"),

    Template = <<"Write a {{style}} essay about {{topic}}">>,
    Variables = #{
        <<"topic">> => <<"climate change">>,
        <<"style">> => <<"formal">>
    },

    Result = erlmcp_prompt_template:render(Template, Variables),

    io:format("Template:  ~s~n", [Template]),
    io:format("Variables: ~p~n", [Variables]),
    io:format("Result:    ~s~n~n", [Result]).

%% Example 4: Code review prompt with nested sections
example_4_code_review_prompt() ->
    io:format("Example 4: Code Review Prompt with Sections~n"),
    io:format("--------------------------------------------~n"),

    Template = <<"You are reviewing {{language}} code in {{style}} style.\n"
                 "Please review the following code and provide feedback:\n\n"
                 "{{#rules}}Rule: {{rule}}\n{{/rules}}">>,

    Variables = #{
        <<"language">> => <<"Erlang">>,
        <<"style">> => <<"OTP">>,
        <<"rules">> => [
            #{<<"rule">> => <<"Check for proper gen_server callbacks">>},
            #{<<"rule">> => <<"Ensure supervision tree is correct">>},
            #{<<"rule">> => <<"Verify error handling">>}
        ]
    },

    Result = erlmcp_prompt_template:render(Template, Variables),

    io:format("Template:  ~s~n", [Template]),
    io:format("Variables: ~p~n", [Variables]),
    io:format("Result:    ~s~n~n", [Result]).

%% Example 5: Working with sections and conditionals
example_5_with_sections() ->
    io:format("Example 5: Sections and Conditionals~n"),
    io:format("--------------------------------------~n"),

    % Example with boolean section
    Template1 = <<"{{#show}}This content is visible{{/show}}">>,
    Variables1 = #{<<"show">> => true},
    Result1 = erlmcp_prompt_template:render(Template1, Variables1),

    io:format("Boolean Section (true):~n"),
    io:format("  Template:  ~s~n", [Template1]),
    io:format("  Result:    ~s~n~n", [Result1]),

    % Example with inverted section
    Template2 = <<"{{^items}}No items available{{/items}}">>,
    Variables2 = #{<<"items">> => []},
    Result2 = erlmcp_prompt_template:render(Template2, Variables2),

    io:format("Inverted Section (empty list):~n"),
    io:format("  Template:  ~s~n", [Template2]),
    io:format("  Result:    ~s~n~n", [Result2]).

%% Example 6: Template validation
example_6_validation() ->
    io:format("Example 6: Template Validation~n"),
    io:format("-------------------------------~n"),

    % Valid template
    ValidTemplate = <<"Hello {{name}}, you have {{count}} messages">>,
    ValidResult = erlmcp_prompt_template:validate(ValidTemplate),
    io:format("Valid Template:   ~s~n", [ValidTemplate]),
    io:format("Validation:       ~p~n~n", [ValidResult]),

    % Invalid template (unclosed tag)
    InvalidTemplate = <<"Hello {{name, you have {{count}} messages">>,
    InvalidResult = erlmcp_prompt_template:validate(InvalidTemplate),
    io:format("Invalid Template: ~s~n", [InvalidTemplate]),
    io:format("Validation:       ~p~n~n", [InvalidResult]),

    % Template syntax detection
    HasSyntax = erlmcp_prompt_template:has_template_syntax(<<"Hello {{name}}">>),
    NoSyntax = erlmcp_prompt_template:has_template_syntax(<<"Hello World">>),
    io:format("Has syntax check: {{...}} -> ~p, plain -> ~p~n~n", [HasSyntax, NoSyntax]).

%% Example 7: Integration with MCP server
example_7_integration_with_server() ->
    io:format("Example 7: Integration with MCP Server~n"),
    io:format("---------------------------------------~n"),

    io:format("Before (manual string construction):~n"),
    io:format("  Handler = fun(Args) ->~n"),
    io:format("    Topic = maps:get(<<\"topic\">>, Args, <<\"general\">>),~n"),
    io:format("    Style = maps:get(<<\"style\">>, Args, <<\"formal\">>),~n"),
    io:format("    <<\"Write a \", Style/binary, \" essay about \", Topic/binary>>~n"),
    io:format("  end~n~n"),

    io:format("After (with templating):~n"),
    io:format("  Handler = fun(Args) ->~n"),
    io:format("    Template = <<\"Write a {{style}} essay about {{topic}}\">>,~n"),
    io:format("    erlmcp_prompt_template:render(Template, Args)~n"),
    io:format("  end~n~n"),

    io:format("Benefits:~n"),
    io:format("  - Cleaner, more readable code~n"),
    io:format("  - Separates content from logic~n"),
    io:format("  - Supports complex templates with sections~n"),
    io:format("  - Automatic template syntax validation~n"),
    io:format("  - Compatible with Mustache standard~n~n").
