%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Examples and Walkthrough Workflows
%%%
%%% Provides interactive examples demonstrating TCPS workflows.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_examples).

-export([run/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

run(["security-patch" | Args]) ->
    security_patch_workflow(parse_args(Args));

run(["new-feature" | Args]) ->
    new_feature_workflow(parse_args(Args));

run(["andon-resolve" | Args]) ->
    andon_resolve_workflow(parse_args(Args));

run(["full-cycle" | Args]) ->
    full_cycle_workflow(parse_args(Args));

run([]) ->
    list_examples();

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown example: ~s", [Unknown]),
    io:format("~nAvailable examples:~n"),
    io:format("  security-patch   Security patch workflow~n"),
    io:format("  new-feature      Feature development workflow~n"),
    io:format("  andon-resolve    Andon resolution workflow~n"),
    io:format("  full-cycle       Complete development cycle~n~n"),
    halt(1).

%%%=============================================================================
%%% Example Workflows
%%%=============================================================================

security_patch_workflow(Args) ->
    Interactive = maps:get(interactive, Args, false),
    DryRun = maps:get(dry_run, Args, false),

    print_header("Security Patch Workflow Example"),

    io:format("This example demonstrates the complete workflow for applying a security patch.~n~n"),

    Steps = [
        {"1. Create work order for security patch",
         "tcps work-order create --bucket security --priority 10 --title \"Fix CVE-2024-1234\""},

        {"2. Implement the fix",
         "# Developer implements the fix in code"},

        {"3. Run compilation",
         "# Compilation generates receipt"},

        {"4. Run tests",
         "# Testing generates receipt"},

        {"5. Run SHACL validation",
         "# Validation generates receipt"},

        {"6. Verify quality gates",
         "tcps quality gates sku_12345"},

        {"7. Complete work order",
         "tcps work-order complete wo_67890"}
    ],

    execute_workflow(Steps, Interactive, DryRun),
    halt(0).

new_feature_workflow(Args) ->
    Interactive = maps:get(interactive, Args, false),
    DryRun = maps:get(dry_run, Args, false),

    print_header("New Feature Development Workflow Example"),

    io:format("This example demonstrates the complete workflow for developing a new feature.~n~n"),

    Steps = [
        {"1. Create work order for new feature",
         "tcps work-order create --bucket reliability --priority 50 --title \"Add user authentication\""},

        {"2. Check current WIP status",
         "tcps kanban status"},

        {"3. Implement the feature",
         "# Developer implements the feature"},

        {"4. Run compilation and capture receipt",
         "# Compilation generates receipt for sku_12345"},

        {"5. Run tests and capture receipt",
         "# Testing generates receipt"},

        {"6. Run SHACL validation",
         "# Validation checks ontology compliance"},

        {"7. Verify complete receipt chain",
         "tcps receipt chain sku_12345"},

        {"8. Check quality gates",
         "tcps quality gates sku_12345"},

        {"9. Complete work order",
         "tcps work-order complete wo_67890"},

        {"10. View Kaizen metrics",
         "tcps kaizen report --weekly"}
    ],

    execute_workflow(Steps, Interactive, DryRun),
    halt(0).

andon_resolve_workflow(Args) ->
    Interactive = maps:get(interactive, Args, false),
    DryRun = maps:get(dry_run, Args, false),

    print_header("Andon Resolution Workflow Example"),

    io:format("This example demonstrates how to resolve an Andon stop-the-line event.~n~n"),

    Steps = [
        {"1. Test failure triggers Andon",
         "tcps andon trigger --type test_failure --sku sku_12345 --message \"Race condition detected\""},

        {"2. Check SKU status (should be blocked)",
         "tcps andon status sku_12345"},

        {"3. Start 5 Whys root cause analysis",
         "tcps root-cause start ANDON-123"},

        {"4. Answer Why #1",
         "tcps root-cause add-why analysis_456 1 \"Test failed due to race condition\""},

        {"5. Answer Why #2",
         "tcps root-cause add-why analysis_456 2 \"Shared state without synchronization\""},

        {"6. Answer Why #3",
         "tcps root-cause add-why analysis_456 3 \"No mutex protecting critical section\""},

        {"7. Answer Why #4",
         "tcps root-cause add-why analysis_456 4 \"Design didn't account for concurrency\""},

        {"8. Answer Why #5",
         "tcps root-cause add-why analysis_456 5 \"Missing concurrency requirements in spec\""},

        {"9. Finalize root cause analysis",
         "tcps root-cause finalize analysis_456 "
         "--root-cause \"Missing concurrency design\" "
         "--prevention \"Add concurrency tests and mutex guards\""},

        {"10. Implement the fix",
         "# Add mutex and concurrency tests"},

        {"11. Resolve the Andon event",
         "tcps andon resolve ANDON-123 "
         "--root-cause \"Missing concurrency design\" "
         "--fix \"Added mutex for shared state\" "
         "--prevention \"Added concurrency test suite\""},

        {"12. Verify SKU is unblocked",
         "tcps andon status sku_12345"}
    ],

    execute_workflow(Steps, Interactive, DryRun),
    halt(0).

full_cycle_workflow(Args) ->
    Interactive = maps:get(interactive, Args, false),
    DryRun = maps:get(dry_run, Args, false),

    print_header("Full Development Cycle Example"),

    io:format("This example shows a complete development cycle with all TCPS components.~n~n"),

    Steps = [
        {"1. Check system health",
         "tcps tpm health"},

        {"2. Create work order",
         "tcps work-order create --bucket reliability --priority 50 --title \"Add feature X\""},

        {"3. Check Kanban WIP limits",
         "tcps kanban status"},

        {"4. Development phase",
         "# Developer implements feature"},

        {"5. Compilation generates receipt",
         "# Compilation creates receipt for compilation stage"},

        {"6. Testing generates receipt",
         "# Tests run and create receipt"},

        {"7. Validation generates receipt",
         "# SHACL validation creates receipt"},

        {"8. Verify receipt chain",
         "tcps receipt chain sku_12345 --format graph"},

        {"9. Check quality gates",
         "tcps quality gates sku_12345 --verbose"},

        {"10. Complete work order",
         "tcps work-order complete wo_67890"},

        {"11. View quality metrics",
         "tcps quality metrics --period weekly --compare"},

        {"12. Review Kaizen improvements",
         "tcps kaizen proposals --top 5"},

        {"13. Check for waste points",
         "tcps kaizen waste"},

        {"14. Run daily maintenance",
         "tcps tpm maintenance --daily"}
    ],

    execute_workflow(Steps, Interactive, DryRun),
    halt(0).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--interactive" | Rest], Acc) ->
    parse_args(Rest, Acc#{interactive => true});
parse_args(["--dry-run" | Rest], Acc) ->
    parse_args(Rest, Acc#{dry_run => true});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

list_examples() ->
    print_header("TCPS Example Workflows"),

    io:format("Available examples:~n~n"),

    io:format("  security-patch~n"),
    io:format("    Complete workflow for applying a security patch.~n"),
    io:format("    Usage: tcps example security-patch [--interactive] [--dry-run]~n~n"),

    io:format("  new-feature~n"),
    io:format("    Complete workflow for developing a new feature.~n"),
    io:format("    Usage: tcps example new-feature [--interactive] [--dry-run]~n~n"),

    io:format("  andon-resolve~n"),
    io:format("    Complete workflow for resolving an Andon stop-the-line event.~n"),
    io:format("    Usage: tcps example andon-resolve [--interactive] [--dry-run]~n~n"),

    io:format("  full-cycle~n"),
    io:format("    Complete development cycle with all TCPS components.~n"),
    io:format("    Usage: tcps example full-cycle [--interactive] [--dry-run]~n~n"),

    io:format("Options:~n"),
    io:format("  --interactive    Prompt before each step~n"),
    io:format("  --dry-run        Show commands without executing~n~n"),

    halt(0).

print_header(Title) ->
    io:format("~n"),
    io:format("~s~n", [Title]),
    io:format("~s~n~n", [lists:duplicate(length(Title), $=)]).

execute_workflow(Steps, Interactive, DryRun) ->
    lists:foreach(fun({Description, Command}) ->
        io:format("~s~n", [Description]),
        io:format("  $ ~s~n", [Command]),

        case {Interactive, DryRun} of
            {true, false} ->
                io:format("~nPress Enter to execute (or Ctrl+C to quit)..."),
                io:get_line(""),
                case string:prefix(Command, "#") of
                    nomatch ->
                        io:format("Executing...~n"),
                        % In production, would actually execute the command
                        io:format("(Command execution not implemented in examples)~n");
                    _ ->
                        io:format("(Manual step - skipped)~n")
                end;
            {_, true} ->
                io:format("(Dry run - not executing)~n");
            _ ->
                ok
        end,

        io:format("~n")
    end, Steps).
