#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    io:format("Testing marketplace generation...~n", []),
    
    %% Compile and load modules
    c:l(erlmcp_plan_loader),
    c:l(erlmcp_marketplace_copy),
    
    %% Load team plan
    case erlmcp_plan_loader:load_plan(team) of
        {ok, PlanSpec} ->
            io:format("✓ Team plan loaded~n", []),
            case erlmcp_marketplace_copy:generate_team_listing(PlanSpec) of
                {ok, Markdown} ->
                    io:format("✓ Generated team listing: ~p bytes~n", [byte_size(Markdown)]),
                    %% Validate
                    case erlmcp_marketplace_copy:validate_listing_markdown(Markdown) of
                        {ok, validated} ->
                            io:format("✓ Team markdown valid~n", []);
                        {error, Reason} ->
                            io:format("✗ Validation failed: ~w~n", [Reason])
                    end;
                {error, Reason} ->
                    io:format("✗ Generation failed: ~w~n", [Reason])
            end;
        {error, Reason} ->
            io:format("✗ Plan load failed: ~w~n", [Reason])
    end,
    
    %% Load enterprise plan
    case erlmcp_plan_loader:load_plan(enterprise) of
        {ok, PlanSpec2} ->
            io:format("✓ Enterprise plan loaded~n", []),
            case erlmcp_marketplace_copy:generate_enterprise_listing(PlanSpec2) of
                {ok, Markdown2} ->
                    io:format("✓ Generated enterprise listing: ~p bytes~n", [byte_size(Markdown2)]);
                {error, Reason2} ->
                    io:format("✗ Enterprise generation failed: ~w~n", [Reason2])
            end;
        {error, Reason2} ->
            io:format("✗ Enterprise plan load failed: ~w~n", [Reason2])
    end,
    
    %% Load gov plan
    case erlmcp_plan_loader:load_plan(gov) of
        {ok, PlanSpec3} ->
            io:format("✓ Government plan loaded~n", []),
            case erlmcp_marketplace_copy:generate_gov_listing(PlanSpec3) of
                {ok, Markdown3} ->
                    io:format("✓ Generated government listing: ~p bytes~n", [byte_size(Markdown3)]);
                {error, Reason3} ->
                    io:format("✗ Government generation failed: ~w~n", [Reason3])
            end;
        {error, Reason3} ->
            io:format("✗ Government plan load failed: ~w~n", [Reason3])
    end,
    
    io:format("~nAll tests completed!~n", []),
    halt(0).
