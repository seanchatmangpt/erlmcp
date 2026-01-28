%%%-------------------------------------------------------------------
%% @doc
%% Marketplace CLI Command - erlmcp marketplace list [team|enterprise|gov]
%%
%% Generates and displays marketplace listings from plan specifications.
%%
%% Usage:
%%   erlmcp marketplace list team
%%   erlmcp marketplace list enterprise
%%   erlmcp marketplace list gov
%%
%% Output: Rendered markdown listing to stdout, ready for marketplace submission.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_marketplace).

-export([
    list/1,
    generate_and_write/2
]).

-type plan_name() :: team | enterprise | gov.
-type cli_args() :: [string()].

%% ===================================================================
%% Public API
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Main CLI entry point for 'erlmcp marketplace list [plan]'.
%%
%% Usage in escript:
%%   {ok, _} = erlmcp_cli_marketplace:list(["team"])
%%   {ok, _} = erlmcp_cli_marketplace:list(["enterprise"])
%%
%% @end
%%--------------------------------------------------------------------
-spec list(Args :: cli_args()) ->
    {ok, Output :: binary()} | {error, Reason :: atom()}.

list([]) ->
    print_usage(),
    {error, no_plan_specified};
list([PlanStr | _Args]) ->
    case string_to_plan_name(PlanStr) of
        {ok, PlanName} ->
            case erlmcp_plan_loader:load_plan(PlanName) of
                {ok, PlanSpec} ->
                    case generate_listing(PlanName, PlanSpec) of
                        {ok, Listing} ->
                            io:format("~s~n", [Listing]),
                            {ok, Listing};
                        {error, Reason} ->
                            print_error(
                                "Failed to generate listing: ~w~n",
                                [Reason]
                            ),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    print_error(
                        "Failed to load plan '~s': ~w~n",
                        [PlanStr, Reason]
                    ),
                    {error, Reason}
            end;
        error ->
            print_error(
                "Unknown plan: '~s' (valid plans: team, enterprise, gov)~n",
                [PlanStr]
            ),
            {error, invalid_plan}
    end.

%%--------------------------------------------------------------------
%% @doc Generate listing and write to file.
%%
%% Writes to dist/marketplace/PLAN-plan.md
%% Useful for batch generation (see Makefile target: generate-marketplace)
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_and_write(PlanName :: plan_name(), OutputDir :: string()) ->
    {ok, FilePath :: string()} | {error, Reason :: atom()}.

generate_and_write(PlanName, OutputDir) ->
    case erlmcp_plan_loader:load_plan(PlanName) of
        {ok, PlanSpec} ->
            case generate_listing(PlanName, PlanSpec) of
                {ok, Listing} ->
                    FileName = atom_to_list(PlanName) ++ "-plan.md",
                    FilePath = filename:join(OutputDir, FileName),
                    case file:write_file(FilePath, Listing) of
                        ok ->
                            io:format("Generated: ~s~n", [FilePath]),
                            {ok, FilePath};
                        {error, Reason} ->
                            print_error("Failed to write ~s: ~w~n", [FilePath, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec generate_listing(PlanName :: plan_name(), PlanSpec :: erlmcp_marketplace_copy:plan_spec()) ->
    {ok, binary()} | {error, atom()}.

generate_listing(team, PlanSpec) ->
    erlmcp_marketplace_copy:generate_team_listing(PlanSpec);
generate_listing(enterprise, PlanSpec) ->
    erlmcp_marketplace_copy:generate_enterprise_listing(PlanSpec);
generate_listing(gov, PlanSpec) ->
    erlmcp_marketplace_copy:generate_gov_listing(PlanSpec).

-spec string_to_plan_name(PlanStr :: string()) ->
    {ok, plan_name()} | error.

string_to_plan_name("team") ->
    {ok, team};
string_to_plan_name("enterprise") ->
    {ok, enterprise};
string_to_plan_name("gov") ->
    {ok, gov};
string_to_plan_name(_) ->
    error.

-spec print_usage() ->
    ok.

print_usage() ->
    io:format(
        "~nUsage: erlmcp marketplace list [PLAN]~n~n"
        "Available plans:~n"
        "  team       - Team tier listing (up to 100 RPS)~n"
        "  enterprise - Enterprise tier listing (up to 10K RPS)~n"
        "  gov        - Government tier listing (up to 50K RPS)~n~n"
        "Examples:~n"
        "  erlmcp marketplace list team~n"
        "  erlmcp marketplace list enterprise~n"
        "  erlmcp marketplace list gov~n~n",
        []
    ).

-spec print_error(Format :: string(), Args :: list()) ->
    ok.

print_error(Format, Args) ->
    io:format(user_error, "Error: " ++ Format, Args).
