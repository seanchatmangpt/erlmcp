-module(gen_marketplace).
-export([main/0]).

main() ->
    %% Load team plan
    {ok, TeamPlan} = erlmcp_plan_loader:load_plan(team),
    {ok, TeamMD} = erlmcp_marketplace_copy:generate_team_listing(TeamPlan),
    file:write_file("dist/marketplace/team-plan.md", TeamMD),
    io:format("✓ Generated: dist/marketplace/team-plan.md (~p bytes)~n", [byte_size(TeamMD)]),
    
    %% Load enterprise plan
    {ok, EntPlan} = erlmcp_plan_loader:load_plan(enterprise),
    {ok, EntMD} = erlmcp_marketplace_copy:generate_enterprise_listing(EntPlan),
    file:write_file("dist/marketplace/enterprise-plan.md", EntMD),
    io:format("✓ Generated: dist/marketplace/enterprise-plan.md (~p bytes)~n", [byte_size(EntMD)]),
    
    %% Load gov plan
    {ok, GovPlan} = erlmcp_plan_loader:load_plan(gov),
    {ok, GovMD} = erlmcp_marketplace_copy:generate_gov_listing(GovPlan),
    file:write_file("dist/marketplace/gov-plan.md", GovMD),
    io:format("✓ Generated: dist/marketplace/gov-plan.md (~p bytes)~n", [byte_size(GovMD)]),
    
    %% Verify determinism
    {ok, TeamMD2} = erlmcp_marketplace_copy:generate_team_listing(TeamPlan),
    case TeamMD =:= TeamMD2 of
        true -> io:format("✓ Team listing generation is deterministic~n");
        false -> io:format("✗ Determinism violation detected~n")
    end,
    
    halt(0).
