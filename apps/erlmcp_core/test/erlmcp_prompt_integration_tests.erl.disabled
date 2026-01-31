%%%====================================================================
%%% @doc Test Suite for erlmcp_prompt_template Integration
%%% Chicago School TDD - Test API boundaries, no state inspection
%%% @end
%%%====================================================================
-module(erlmcp_prompt_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_complete_template_workflow()),
        ?_test(test_template_with_validation_pipeline()),
        ?_test(test_batch_template_processing())
    ].

test_complete_template_workflow() ->
    %% Create a complex template
    Template = <<"Hello {{name}}, you have {{count}} new messages.{{#unread}} ({{unread}} unread){{/unread}}">>,
    Variables = #{
        <<"name">> => <<"Alice">>,
        <<"count">> => 5,
        <<"unread">> => 2
    },

    %% Validate template
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)),

    %% Check template syntax detection
    ?assertEqual(true, erlmcp_prompt_template:has_template_syntax(Template)),

    %% Render safely
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello Alice, you have 5 new messages. (2 unread)">>, Result).

test_template_with_validation_pipeline() ->
    %% Valid template
    ValidTemplate = <<"{{greeting}}, {{name}}!">>,
    ?assertEqual(ok, erlmcp_prompt_template:validate(ValidTemplate)),

    %% Invalid template (unclosed section)
    InvalidTemplate = <<"{{#section}}content">>,
    ?assertMatch({error, {invalid_template_syntax, _}}, erlmcp_prompt_template:validate(InvalidTemplate)),

    %% Too large template
    LargeTemplate = binary:copy(<<"a">>, 10241),
    ?assertMatch({error, {template_too_large, _, _}}, erlmcp_prompt_template:validate(LargeTemplate)).

test_batch_template_processing() ->
    %% Process multiple templates
    Templates = [
        {<<"Template {{num}}">>, #{<<"num">> => <<"1">>}, <<"Template 1">>},
        {<<"Template {{num}}">>, #{<<"num">> => <<"2">>}, <<"Template 2">>},
        {<<"Template {{num}}">>, #{<<"num">> => <<"3">>}, <<"Template 3">>}
    ],

    %% Validate all templates
    lists:foreach(fun({Template, _, _}) ->
        ?assertEqual(ok, erlmcp_prompt_template:validate(Template))
    end, Templates),

    %% Render all templates
    lists:foreach(fun({Template, Variables, Expected}) ->
        ?assertMatch({ok, Expected}, erlmcp_prompt_template:render_safe(Template, Variables))
    end, Templates).

%%====================================================================
%% End-to-End Tests
%%====================================================================

end_to_end_test_() ->
    [
        ?_test(test_complex_message_template_workflow()),
        ?_test(test_batch_processing_with_compilation()),
        ?_test(test_template_reuse_workflow())
    ].

test_complex_message_template_workflow() ->
    %% Complex email-like template
    Template = <<
        "From: {{from_name}} <{{from_email}}>\n"
        "To: {{to_name}} <{{to_email}}>\n"
        "Subject: {{subject}}\n"
        "\n"
        "Dear {{to_name}},\n"
        "\n"
        "{{#greeting}}{{greeting}}\n\n{{/greeting}}"
        "{{message}}\n"
        "\n"
        "Best regards,\n"
        "{{from_name}}\n"
        "\n"
        "{{#attachments}}Attachments:\n{{#attachments}}- {{name}} ({{size}} bytes)\n{{/attachments}}{{/attachments}}"
    >>,

    Variables = #{
        <<"from_name">> => <<"Bob Smith">>,
        <<"from_email">> => <<"bob@example.com">>,
        <<"to_name">> => <<"Alice Johnson">>,
        <<"to_email">> => <<"alice@example.com">>,
        <<"subject">> => <<"Project Update">>,
        <<"greeting">> => <<"Hope this email finds you well.">>,
        <<"message">> => <<"The project is progressing nicely. We have completed the first phase.">>,
        <<"attachments">> => [
            #{<<"name">> => <<"report.pdf">>, <<"size">> => 1024},
            #{<<"name">> => <<"data.xlsx">>, <<"size">> => 2048}
        ]
    },

    %% Full workflow
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)),
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),

    %% Verify key parts of output
    ?assert(<<>> =/= Result),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

test_batch_processing_with_compilation() ->
    %% Compile once, render multiple times
    Template = <<"Hello {{name}}, your ID is {{id}}.">>,
    {ok, Compiled} = erlmcp_prompt_template:compile(Template),

    %% Render with different variables
    VariableSets = [
        #{<<"name">> => <<"Alice">>, <<"id">> => 1},
        #{<<"name">> => <<"Bob">>, <<"id">> => 2},
        #{<<"name">> => <<"Charlie">>, <<"id">> => 3}
    ],

    ExpectedResults = [
        <<"Hello Alice, your ID is 1.">>,
        <<"Hello Bob, your ID is 2.">>,
        <<"Hello Charlie, your ID is 3.">>
    ],

    Results = [begin
        {ok, Result} = erlmcp_prompt_template:render(Compiled, Vars),
        Result
    end || Vars <- VariableSets],

    ?assertEqual(ExpectedResults, Results).

test_template_reuse_workflow() ->
    %% Test template reuse across different contexts
    Template = <<"{{title}}: {{value}}">>,

    Contexts = [
        #{<<"title">> => <<"Name">>, <<"value">> => <<"Alice">>},
        #{<<"title">> => <<"Age">>, <<"value">> => 30},
        #{<<"title">> => <<"City">>, <<"value">> => <<"NYC">>}
    ],

    Expected = [
        <<"Name: Alice">>,
        <<"Age: 30">>,
        <<"City: NYC">>
    ],

    %% Validate once
    ?assertEqual(ok, erlmcp_prompt_template:validate(Template)),

    %% Render for each context
    Results = [begin
        {ok, Result} = erlmcp_prompt_template:render_safe(Template, Ctx),
        Result
    end || Ctx <- Contexts],

    ?assertEqual(Expected, Results).

%%====================================================================
%% Real-World Scenario Tests
%%====================================================================

real_world_scenario_test_() ->
    [
        ?_test(test_api_response_template()),
        ?_test(test_log_message_template()),
        ?_test(test_user_notification_template())
    ].

test_api_response_template() ->
    %% API response template with conditional sections
    Template = <<
        "{{#success}}Success: {{message}}{{/success}}"
        "{{^success}}Error: {{error_code}} - {{error_message}}{{/success}}"
    >>,

    %% Success case
    SuccessVars = #{
        <<"success">> => true,
        <<"message">> => <<"Resource created">>
    },
    {ok, SuccessResult} = erlmcp_prompt_template:render_safe(Template, SuccessVars),
    ?assertEqual(<<"Success: Resource created">>, SuccessResult),

    %% Error case
    ErrorVars = #{
        <<"success">> => false,
        <<"error_code">> => 404,
        <<"error_message">> => <<"Not found">>
    },
    {ok, ErrorResult} = erlmcp_prompt_template:render_safe(Template, ErrorVars),
    ?assertEqual(<<"Error: 404 - Not found">>, ErrorResult).

test_log_message_template() ->
    %% Log message with severity levels
    Template = <<
        "[{{timestamp}}] [{{level}}] [{{module}}] {{#context}}Context: {{context}} - {{/context}}{{message}}"
    >>,

    Variables = #{
        <<"timestamp">> => <<"2024-01-30 12:00:00">>,
        <<"level">> => <<"ERROR">>,
        <<"module">> => <<"erlmcp_server">>,
        <<"context">> => <<"request_id=123">>,
        <<"message">> => <<"Connection failed">>
    },

    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assert(<<>> =/= Result),
    ?assert(is_binary(Result)).

test_user_notification_template() ->
    %% User notification with optional items
    Template = <<
        "Hi {{username}},\n"
        "\n"
        "{{#updates}}You have {{count}} new updates:\n"
        "{{#updates}}- {{title}}\n{{/updates}}"
        "{{/updates}}"
        "{{^updates}}No new updates.\n{{/updates}}"
        "\n"
        "Thanks!"
    >>,

    %% With updates
    WithUpdates = #{
        <<"username">> => <<"Alice">>,
        <<"count">> => 2,
        <<"updates">> => [
            #{<<"title">> => <<"New feature available">>},
            #{<<"title">> => <<"Security update">>}
        ]
    },
    {ok, WithUpdatesResult} = erlmcp_prompt_template:render_safe(Template, WithUpdates),
    ?assert(<<>> =/= WithUpdatesResult),

    %% Without updates
    NoUpdates = #{
        <<"username">> => <<"Bob">>,
        <<"updates">> => []
    },
    {ok, NoUpdatesResult} = erlmcp_prompt_template:render_safe(Template, NoUpdates),
    ?assert(<<>> =/= NoUpdatesResult).
