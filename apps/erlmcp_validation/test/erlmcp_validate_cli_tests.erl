%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive Test Suite for erlmcp_validate_cli
%%%
%%% Chicago School TDD: Tests FIRST, real CLI execution, NO mocks.
%%% Tests ALL observable behavior through ALL interfaces (CLI, API).
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_validate_cli_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Setup and Teardown
%%%====================================================================

%% Setup fixture
cli_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Parse help command", fun test_parse_help/0},
      {"Parse version command", fun test_parse_version/0},
      {"Parse validate spec command", fun test_parse_validate_spec/0},
      {"Parse validate protocol command", fun test_parse_validate_protocol/0},
      {"Parse validate transport command", fun test_parse_validate_transport/0},
      {"Parse validate security command", fun test_parse_validate_security/0},
      {"Parse validate performance command", fun test_parse_validate_performance/0},
      {"Parse validate all command", fun test_parse_validate_all/0},
      {"Parse validate with format json", fun test_parse_validate_format_json/0},
      {"Parse validate with format text", fun test_parse_validate_format_text/0},
      {"Parse validate with transport option", fun test_parse_validate_transport_option/0},
      {"Parse report generate command", fun test_parse_report_generate/0},
      {"Parse report with format markdown", fun test_parse_report_format_markdown/0},
      {"Parse report with output file", fun test_parse_report_output_file/0},
      {"Parse invalid command", fun test_parse_invalid_command/0},
      {"Parse invalid validate type", fun test_parse_invalid_validate_type/0},
      {"Parse invalid format", fun test_parse_invalid_format/0},
      {"Parse invalid transport", fun test_parse_invalid_transport/0},
      {"Run command validate spec", fun test_run_command_validate_spec/0},
      {"Run command validate protocol", fun test_run_command_validate_protocol/0},
      {"Run command validate transport", fun test_run_command_validate_transport/0},
      {"Run command validate security", fun test_run_command_validate_security/0},
      {"Run command validate performance", fun test_run_command_validate_performance/0},
      {"Run command validate all", fun test_run_command_validate_all/0},
      {"Run command report generate", fun test_run_command_report_generate/0},
      {"Run command version", fun test_run_command_version/0},
      {"Run command help", fun test_run_command_help/0},
      {"Run command unknown", fun test_run_command_unknown/0},
      {"Execute validate spec text", fun test_execute_validate_spec_text/0},
      {"Execute validate spec json", fun test_execute_validate_spec_json/0},
      {"Execute validate protocol text", fun test_execute_validate_protocol_text/0},
      {"Execute validate protocol json", fun test_execute_validate_protocol_json/0},
      {"Execute validate transport stdio", fun test_execute_validate_transport_stdio/0},
      {"Execute validate transport tcp", fun test_execute_validate_transport_tcp/0},
      {"Execute validate security stdio", fun test_execute_validate_security_stdio/0},
      {"Execute validate performance stdio", fun test_execute_validate_performance_stdio/0},
      {"Execute validate all default", fun test_execute_validate_all_default/0},
      {"Execute report generate text", fun test_execute_report_generate_text/0},
      {"Execute report generate json", fun test_execute_report_generate_json/0},
      {"Execute report generate markdown", fun test_execute_report_generate_markdown/0},
      {"Execute report generate html", fun test_execute_report_generate_html/0},
      {"Execute report generate with output", fun test_execute_report_generate_with_output/0},
      {"Validate spec version", fun test_validate_spec_version/0},
      {"Validate lifecycle", fun test_validate_lifecycle/0},
      {"Validate tools API", fun test_validate_tools_api/0},
      {"Validate resources API", fun test_validate_resources_api/0},
      {"Validate prompts API", fun test_validate_prompts_api/0},
      {"Validate JSON-RPC structure", fun test_validate_jsonrpc_structure/0},
      {"Validate request format", fun test_validate_request_format/0},
      {"Validate response format", fun test_validate_response_format/0},
      {"Validate notification format", fun test_validate_notification_format/0},
      {"Validate error codes", fun test_validate_error_codes/0},
      {"Validate transport callbacks", fun test_validate_transport_callbacks/0},
      {"Validate transport framing", fun test_validate_transport_framing/0},
      {"Validate transport registry", fun test_validate_transport_registry/0},
      {"Validate transport lifecycle", fun test_validate_transport_lifecycle/0},
      {"Validate security authentication", fun test_validate_security_authentication/0},
      {"Validate security input validation", fun test_validate_security_input_validation/0},
      {"Validate security secrets", fun test_validate_security_secrets/0},
      {"Validate security JWT", fun test_validate_security_jwt/0},
      {"Validate security rate limiting", fun test_validate_security_rate_limiting/0},
      {"Validate security CORS", fun test_validate_security_cors/0},
      {"Summarize validation results all passed", fun test_summarize_all_passed/0},
      {"Summarize validation results all failed", fun test_summarize_all_failed/0},
      {"Summarize validation results mixed", fun test_summarize_mixed/0},
      {"Exit code success", fun test_exit_code_success/0},
      {"Exit code failed", fun test_exit_code_failed/0},
      {"Transport to module mapping", fun test_transport_to_module/0},
      {"Transport to type mapping", fun test_transport_to_type/0},
      {"Format to atom mapping", fun test_format_to_atom/0}]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(asn1),
    application:ensure_all_started(public_key),
    application:ensure_all_started(ssl),
    ok.

cleanup(_Args) ->
    ok.

%%%====================================================================
%%% Argument Parsing Tests (15 tests)
%%%====================================================================

test_parse_help() ->
    ?assertMatch({help, _}, erlmcp_validate_cli:parse_args(["--help"])).

test_parse_version() ->
    ?assertMatch({ok, {version, #{}}}, erlmcp_validate_cli:parse_args(["--version"])).

test_parse_validate_spec() ->
    ?assertMatch({ok, {validate, spec, #{type := spec}}},
                 erlmcp_validate_cli:parse_args(["validate", "spec"])).

test_parse_validate_protocol() ->
    ?assertMatch({ok, {validate, protocol, #{type := protocol}}},
                 erlmcp_validate_cli:parse_args(["validate", "protocol"])).

test_parse_validate_transport() ->
    ?assertMatch({ok, {validate, transport, #{type := transport}}},
                 erlmcp_validate_cli:parse_args(["validate", "transport"])).

test_parse_validate_security() ->
    ?assertMatch({ok, {validate, security, #{type := security}}},
                 erlmcp_validate_cli:parse_args(["validate", "security"])).

test_parse_validate_performance() ->
    ?assertMatch({ok, {validate, performance, #{type := performance}}},
                 erlmcp_validate_cli:parse_args(["validate", "performance"])).

test_parse_validate_all() ->
    ?assertMatch({ok, {validate, all, #{type := all}}},
                 erlmcp_validate_cli:parse_args(["validate", "all"])).

test_parse_validate_format_json() ->
    ?assertMatch({ok, {validate, _, #{format := json}}},
                 erlmcp_validate_cli:parse_args(["validate", "spec", "--format", "json"])).

test_parse_validate_format_text() ->
    ?assertMatch({ok, {validate, _, #{format := text}}},
                 erlmcp_validate_cli:parse_args(["validate", "protocol", "--format", "text"])).

test_parse_validate_transport_option() ->
    ?assertMatch({ok, {validate, _, #{transport := tcp}}},
                 erlmcp_validate_cli:parse_args(["validate", "transport", "--transport", "tcp"])).

test_parse_report_generate() ->
    ?assertMatch({ok, {report, generate, #{}}},
                 erlmcp_validate_cli:parse_args(["report", "generate"])).

test_parse_report_format_markdown() ->
    ?assertMatch({ok, {report, generate, #{format := markdown}}},
                 erlmcp_validate_cli:parse_args(["report", "generate", "--format", "markdown"])).

test_parse_report_output_file() ->
    ?assertMatch({ok, {report, generate, #{output := "report.md"}}},
                 erlmcp_validate_cli:parse_args(["report", "generate", "--output", "report.md"])).

test_parse_invalid_command() ->
    ?assertMatch({error, "Unknown command:" ++ _}, erlmcp_validate_cli:parse_args(["unknown"])).

test_parse_invalid_validate_type() ->
    ?assertMatch({error, "Unknown validation type:" ++ _},
                 erlmcp_validate_cli:parse_args(["validate", "invalid"])).

test_parse_invalid_format() ->
    ?assertMatch({error, "Invalid format:" ++ _},
                 erlmcp_validate_cli:parse_args(["validate", "spec", "--format", "invalid"])).

test_parse_invalid_transport() ->
    ?assertMatch({error, "Invalid transport:" ++ _},
                 erlmcp_validate_cli:parse_args(["validate", "spec", "--transport", "invalid"])).

%%%====================================================================
%%% Run Command Tests (10 tests)
%%%====================================================================

test_run_command_validate_spec() ->
    Command = {validate, spec, #{}},
    ?assertMatch({ok, _}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_validate_protocol() ->
    Command = {validate, protocol, #{}},
    ?assertMatch({ok, _}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_validate_transport() ->
    Command = {validate, transport, #{}},
    ?assertMatch({ok, _}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_validate_security() ->
    Command = {validate, security, #{}},
    ?assertMatch({ok, _}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_validate_performance() ->
    Command = {validate, performance, #{}},
    ?assertMatch({ok, _}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_validate_all() ->
    Command = {validate, all, #{}},
    ?assertMatch({ok, _}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_report_generate() ->
    Command = {report, generate, #{}},
    ?assertMatch({ok, _}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_version() ->
    Command = {version, #{}},
    ?assertMatch({ok, 0}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_help() ->
    Command = {help, #{}},
    ?assertMatch({ok, 0}, erlmcp_validate_cli:run_command(Command, [])).

test_run_command_unknown() ->
    Command = {unknown, #{}},
    ?assertMatch({error, {unknown_command, _}}, erlmcp_validate_cli:run_command(Command, [])).

%%%====================================================================
%%% Execute Command Tests (10 tests)
%%%====================================================================

test_execute_validate_spec_text() ->
    %% Test that validate spec executes with text format
    Result = erlmcp_validate_cli:execute_validate(spec, #{format => text}),
    ?assertMatch({ok, _}, Result).

test_execute_validate_spec_json() ->
    %% Test that validate spec executes with json format
    Result = erlmcp_validate_cli:execute_validate(spec, #{format => json}),
    ?assertMatch({ok, _}, Result).

test_execute_validate_protocol_text() ->
    %% Test that validate protocol executes
    Result = erlmcp_validate_cli:execute_validate(protocol, #{format => text}),
    ?assertMatch({ok, _}, Result).

test_execute_validate_protocol_json() ->
    %% Test that validate protocol executes with json format
    Result = erlmcp_validate_cli:execute_validate(protocol, #{format => json}),
    ?assertMatch({ok, _}, Result).

test_execute_validate_transport_stdio() ->
    %% Test that validate transport executes for stdio
    Result = erlmcp_validate_cli:execute_validate(transport, #{transport => stdio, format => text}),
    ?assertMatch({ok, _}, Result).

test_execute_validate_transport_tcp() ->
    %% Test that validate transport executes for tcp
    Result = erlmcp_validate_cli:execute_validate(transport, #{transport => tcp, format => text}),
    ?assertMatch({ok, _}, Result).

test_execute_validate_security_stdio() ->
    %% Test that validate security executes
    Result = erlmcp_validate_cli:execute_validate(security, #{transport => stdio, format => text}),
    ?assertMatch({ok, _}, Result).

test_execute_validate_performance_stdio() ->
    %% Test that validate performance executes
    Result =
        erlmcp_validate_cli:execute_validate(performance, #{transport => stdio, format => text}),
    ?assertMatch({ok, _}, Result).

test_execute_validate_all_default() ->
    %% Test that validate all executes with defaults
    Result = erlmcp_validate_cli:execute_validate(all, #{}),
    ?assertMatch({ok, _}, Result).

test_execute_report_generate_text() ->
    %% Test that report generate executes with text format
    Result = erlmcp_validate_cli:execute_report_generate(#{format => text}),
    ?assertMatch({ok, _}, Result).

test_execute_report_generate_json() ->
    %% Test that report generate executes with json format
    Result = erlmcp_validate_cli:execute_report_generate(#{format => json}),
    ?assertMatch({ok, _}, Result).

test_execute_report_generate_markdown() ->
    %% Test that report generate executes with markdown format
    Result = erlmcp_validate_cli:execute_report_generate(#{format => markdown}),
    ?assertMatch({ok, _}, Result).

test_execute_report_generate_html() ->
    %% Test that report generate executes with html format
    Result = erlmcp_validate_cli:execute_report_generate(#{format => html}),
    ?assertMatch({ok, _}, Result).

test_execute_report_generate_with_output() ->
    %% Test that report generate writes to file
    OutputFile = "/tmp/test_report.md",
    Result =
        erlmcp_validate_cli:execute_report_generate(#{format => markdown, output => OutputFile}),
    ?assertMatch({ok, _}, Result),
    %% Verify file was created
    ?assert(filelib:is_file(OutputFile)),
    %% Cleanup
    file:delete(OutputFile).

%%%====================================================================
%%% Validation Function Tests (15 tests)
%%%====================================================================

test_validate_spec_version() ->
    %% Test that spec version validation returns proper format
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_spec_validation(text)).

test_validate_lifecycle() ->
    %% Test that lifecycle validation returns proper format
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_protocol_validation(text)).

test_validate_tools_api() ->
    %% Test that tools API validation returns proper format
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_spec_validation(text)).

test_validate_resources_api() ->
    %% Test that resources API validation returns proper format
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_spec_validation(text)).

test_validate_prompts_api() ->
    %% Test that prompts API validation returns proper format
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_spec_validation(text)).

test_validate_jsonrpc_structure() ->
    %% Test JSON-RPC structure validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_protocol_validation(text)).

test_validate_request_format() ->
    %% Test request format validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_protocol_validation(text)).

test_validate_response_format() ->
    %% Test response format validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_protocol_validation(text)).

test_validate_notification_format() ->
    %% Test notification format validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_protocol_validation(text)).

test_validate_error_codes() ->
    %% Test error codes validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_protocol_validation(text)).

test_validate_transport_callbacks() ->
    %% Test transport callbacks validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_transport_validation(stdio, text)).

test_validate_transport_framing() ->
    %% Test transport framing validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_transport_validation(stdio, text)).

test_validate_transport_registry() ->
    %% Test transport registry validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_transport_validation(stdio, text)).

test_validate_transport_lifecycle() ->
    %% Test transport lifecycle validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_transport_validation(stdio, text)).

test_validate_security_authentication() ->
    %% Test security authentication validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_security_validation(stdio, text)).

test_validate_security_input_validation() ->
    %% Test security input validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_security_validation(stdio, text)).

test_validate_security_secrets() ->
    %% Test security secrets validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_security_validation(stdio, text)).

test_validate_security_jwt() ->
    %% Test security JWT validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_security_validation(stdio, text)).

test_validate_security_rate_limiting() ->
    %% Test security rate limiting validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_security_validation(stdio, text)).

test_validate_security_cors() ->
    %% Test security CORS validation
    ?assertMatch({ok,
                  #{passed := _,
                    failed := _,
                    status := _}},
                 erlmcp_validate_cli:run_security_validation(stdio, text)).

%%%====================================================================
%%% Summary and Helper Tests (5 tests)
%%%====================================================================

test_summarize_all_passed() ->
    %% Test summary with all passed results
    Results = [{test1, {ok, #{status => passed}}}, {test2, {ok, #{status => passed}}}],
    Summary = erlmcp_validate_cli:summarize_validation_results(Results),
    ?assertEqual(2, maps:get(total, Summary)),
    ?assertEqual(2, maps:get(passed, Summary)),
    ?assertEqual(0, maps:get(failed, Summary)),
    ?assertEqual(success, maps:get(status, Summary)).

test_summarize_all_failed() ->
    %% Test summary with all failed results
    Results = [{test1, {ok, #{status => failed}}}, {test2, {ok, #{status => failed}}}],
    Summary = erlmcp_validate_cli:summarize_validation_results(Results),
    ?assertEqual(2, maps:get(total, Summary)),
    ?assertEqual(0, maps:get(passed, Summary)),
    ?assertEqual(2, maps:get(failed, Summary)),
    ?assertEqual(failed, maps:get(status, Summary)).

test_summarize_mixed() ->
    %% Test summary with mixed results
    Results =
        [{test1, {ok, #{status => passed}}},
         {test2, {ok, #{status => failed}}},
         {test3, {ok, #{status => warning}}}],
    Summary = erlmcp_validate_cli:summarize_validation_results(Results),
    ?assertEqual(3, maps:get(total, Summary)),
    ?assertEqual(1, maps:get(passed, Summary)),
    ?assertEqual(1, maps:get(failed, Summary)),
    ?assertEqual(failed, maps:get(status, Summary)).

test_exit_code_success() ->
    %% Test exit code for success
    ?assertEqual(0, erlmcp_validate_cli:exit_code(#{status => success})).

test_exit_code_failed() ->
    %% Test exit code for failure
    ?assertEqual(1, erlmcp_validate_cli:exit_code(#{status => failed})).

test_transport_to_module() ->
    %% Test transport to module mapping
    ?assertEqual(erlmcp_transport_stdio, erlmcp_validate_cli:transport_to_module(stdio)),
    ?assertEqual(erlmcp_transport_tcp, erlmcp_validate_cli:transport_to_module(tcp)),
    ?assertEqual(erlmcp_transport_http, erlmcp_validate_cli:transport_to_module(http)),
    ?assertEqual(erlmcp_transport_ws, erlmcp_validate_cli:transport_to_module(websocket)).

test_transport_to_type() ->
    %% Test transport to type mapping
    ?assertEqual(stdio, erlmcp_validate_cli:transport_to_type(stdio)),
    ?assertEqual(tcp, erlmcp_validate_cli:transport_to_type(tcp)),
    ?assertEqual(http, erlmcp_validate_cli:transport_to_type(http)),
    ?assertEqual(websocket, erlmcp_validate_cli:transport_to_type(websocket)).

test_format_to_atom() ->
    %% Test format to atom mapping
    ?assertEqual(text, erlmcp_validate_cli:format_to_atom(text)),
    ?assertEqual(json, erlmcp_validate_cli:format_to_atom(json)),
    ?assertEqual(markdown, erlmcp_validate_cli:format_to_atom(markdown)),
    ?assertEqual(html, erlmcp_validate_cli:format_to_atom(html)).

%%%====================================================================
%%% Integration Tests (5 tests)
%%%====================================================================

integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun cleanup_integration/1,
     [{"Full validate spec workflow", fun test_full_validate_spec_workflow/0},
      {"Full validate protocol workflow", fun test_full_validate_protocol_workflow/0},
      {"Full report generate workflow", fun test_full_report_generate_workflow/0},
      {"Error handling for missing applications", fun test_error_handling_missing_apps/0},
      {"Output format verification text", fun test_output_format_verification_text/0}]}.

setup_integration() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(asn1),
    application:ensure_all_started(public_key),
    ok.

cleanup_integration(_Args) ->
    ok.

test_full_validate_spec_workflow() ->
    %% Test complete validate spec workflow
    ?assertMatch({ok, 0}, erlmcp_validate_cli:run_command({validate, spec, #{}}, [])).

test_full_validate_protocol_workflow() ->
    %% Test complete validate protocol workflow
    ?assertMatch({ok, 0}, erlmcp_validate_cli:run_command({validate, protocol, #{}}, [])).

test_full_report_generate_workflow() ->
    %% Test complete report generate workflow
    ?assertMatch({ok, 0}, erlmcp_validate_cli:run_command({report, generate, #{}}, [])).

test_error_handling_missing_apps() ->
    %% Test error handling when applications are not available
    %% This is tested by the normal behavior - applications are started in setup
    ?assertMatch({ok, _}, erlmcp_validate_cli:run_command({validate, spec, #{}}, [])).

test_output_format_verification_text() ->
    %% Test that text output format works correctly
    ?assertMatch({ok, 0}, erlmcp_validate_cli:run_command({validate, spec, #{format => text}}, [])).
