%%% @doc Tests for C4 Architecture Model
%%%
%%% Validates the C4 model structure, relationships, and export formats.
%%% Follows Chicago School TDD - no mocks, real data structures only.
%%%
%%% @end
-module(pqc_c4_model_test).

-include_lib("eunit/include/eunit.hrl").
-include("pqchain.hrl").

%%% ============================================================================
%%% Test Setup
%%% ============================================================================

c4_model_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Context level has all required elements", fun test_context/0},
         {"Containers are properly defined", fun test_containers/0},
         {"Components are properly nested", fun test_components/0},
         {"Relationships are valid", fun test_relationships/0},
         {"Model validation passes", fun test_validation/0},
         {"PlantUML context export is valid", fun test_plantuml_context/0},
         {"PlantUML container export is valid", fun test_plantuml_container/0},
         {"PlantUML component export is valid", fun test_plantuml_component/0},
         {"Structurizr DSL export is valid", fun test_structurizr_dsl/0},
         {"Mermaid context export is valid", fun test_mermaid_context/0},
         {"Mermaid container export is valid", fun test_mermaid_container/0},
         {"All IDs are unique", fun test_unique_ids/0},
         {"One kernel two faces architecture", fun test_one_kernel_two_faces/0},
         {"All core containers present", fun test_core_containers/0}
     ]
    }.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%% ============================================================================
%%% Context Level Tests
%%% ============================================================================

test_context() ->
    Context = pqc_c4_model:context(),

    %% Verify context structure
    ?assertEqual(<<"pqchain">>, element(2, Context)),
    ?assertEqual(<<"SwarmFlow PQChain">>, element(3, Context)),
    ?assertNotEqual(<<>>, element(4, Context)),
    ?assertEqual(false, element(5, Context)),

    %% Verify containers are present
    Containers = element(6, Context),
    ?assert(is_list(Containers)),
    ?assert(length(Containers) >= 9),

    ok.

%%% ============================================================================
%%% Container Level Tests
%%% ============================================================================

test_containers() ->
    Containers = pqc_c4_model:containers(),

    %% Should have 9 containers
    ?assertEqual(9, length(Containers)),

    %% Verify each container has required fields
    lists:foreach(
        fun(Container) ->
            ?assertMatch({c4_container, _, _, _, _, _}, Container),
            ?assertNotEqual(<<>>, element(2, Container)),  % id
            ?assertNotEqual(<<>>, element(3, Container)),  % name
            ?assertNotEqual(<<>>, element(4, Container)),  % technology
            ?assertNotEqual(<<>>, element(5, Container)),  % description
            ?assert(is_list(element(6, Container)))        % components
        end,
        Containers
    ),

    ok.

test_core_containers() ->
    Containers = pqc_c4_model:containers(),
    ContainerIds = [element(2, C) || C <- Containers],

    %% Verify core containers exist
    RequiredContainers = [
        <<"case_kernel">>,
        <<"a2a_face">>,
        <<"mcp_face">>,
        <<"consensus_engine">>,
        <<"peer_network">>,
        <<"contract_runtime">>,
        <<"mempool_service">>,
        <<"chain_state_db">>,
        <<"crypto_services">>
    ],

    lists:foreach(
        fun(Required) ->
            ?assert(lists:member(Required, ContainerIds),
                   io_lib:format("Missing required container: ~s", [Required]))
        end,
        RequiredContainers
    ),

    ok.

%%% ============================================================================
%%% Component Level Tests
%%% ============================================================================

test_components() ->
    %% Test components for each container
    Containers = pqc_c4_model:containers(),

    lists:foreach(
        fun(Container) ->
            ContainerId = element(2, Container),
            Components = pqc_c4_model:components(ContainerId),

            %% Should return list of components
            ?assert(is_list(Components)),

            %% Each container should have components
            ?assert(length(Components) > 0,
                   io_lib:format("Container ~s has no components", [ContainerId])),

            %% Verify component structure
            lists:foreach(
                fun(Component) ->
                    ?assertMatch({c4_component, _, _, _, _, _}, Component),
                    ?assertNotEqual(<<>>, element(2, Component)),  % id
                    ?assertNotEqual(<<>>, element(3, Component)),  % name
                    ?assertNotEqual(<<>>, element(4, Component)),  % technology
                    ?assertNotEqual(<<>>, element(5, Component)),  % description
                    ?assert(is_list(element(6, Component)))        % responsibilities
                end,
                Components
            )
        end,
        Containers
    ),

    %% Test invalid container
    ?assertEqual({error, not_found}, pqc_c4_model:components(<<"invalid">>)),

    ok.

%%% ============================================================================
%%% Relationship Tests
%%% ============================================================================

test_relationships() ->
    Relationships = pqc_c4_model:relationships(),

    %% Should have many relationships
    ?assert(length(Relationships) >= 20),

    %% Verify relationship structure
    lists:foreach(
        fun(Rel) ->
            ?assertMatch({c4_relationship, _, _, _, _, _}, Rel),
            ?assertNotEqual(<<>>, element(2, Rel)),  % source
            ?assertNotEqual(<<>>, element(3, Rel)),  % target
            ?assertNotEqual(<<>>, element(4, Rel)),  % description
            ?assert(is_boolean(element(6, Rel)))     % bidirectional
        end,
        Relationships
    ),

    %% Verify key relationships exist
    Sources = [element(2, R) || R <- Relationships],
    Targets = [element(3, R) || R <- Relationships],

    %% A2A face should have relationships
    ?assert(lists:member(<<"a2a_face">>, Sources) orelse
            lists:member(<<"a2a_face">>, Targets)),

    %% MCP face should have relationships
    ?assert(lists:member(<<"mcp_face">>, Sources) orelse
            lists:member(<<"mcp_face">>, Targets)),

    %% Consensus engine should have relationships
    ?assert(lists:member(<<"consensus_engine">>, Sources) orelse
            lists:member(<<"consensus_engine">>, Targets)),

    ok.

%%% ============================================================================
%%% Validation Tests
%%% ============================================================================

test_validation() ->
    %% Model should validate successfully
    ?assertEqual(ok, pqc_c4_model:validate_model()),

    ok.

test_unique_ids() ->
    AllIds = pqc_c4_model:get_all_ids(),

    %% No duplicates
    UniqueIds = lists:usort(AllIds),
    ?assertEqual(length(UniqueIds), length(AllIds),
                "Duplicate IDs found in model"),

    %% Should have many IDs
    ?assert(length(AllIds) >= 30),

    ok.

%%% ============================================================================
%%% Architecture Pattern Tests
%%% ============================================================================

test_one_kernel_two_faces() ->
    Containers = pqc_c4_model:containers(),
    ContainerIds = [element(2, C) || C <- Containers],

    %% Verify "one kernel, two faces" pattern
    ?assert(lists:member(<<"case_kernel">>, ContainerIds),
           "Case Kernel (one kernel) missing"),
    ?assert(lists:member(<<"a2a_face">>, ContainerIds),
           "A2A Face (first face) missing"),
    ?assert(lists:member(<<"mcp_face">>, ContainerIds),
           "MCP Face (second face) missing"),

    %% Verify relationships between kernel and faces
    Relationships = pqc_c4_model:relationships(),

    %% Find relationships connecting faces to kernel
    A2AToKernel = lists:any(
        fun(R) ->
            (element(2, R) =:= <<"a2a_face">> andalso
             element(3, R) =:= <<"case_kernel">>) orelse
            (element(3, R) =:= <<"a2a_face">> andalso
             element(2, R) =:= <<"case_kernel">>)
        end,
        Relationships
    ),
    ?assert(A2AToKernel, "No relationship between A2A Face and Case Kernel"),

    MCPToKernel = lists:any(
        fun(R) ->
            (element(2, R) =:= <<"mcp_face">> andalso
             element(3, R) =:= <<"case_kernel">>) orelse
            (element(3, R) =:= <<"mcp_face">> andalso
             element(2, R) =:= <<"case_kernel">>)
        end,
        Relationships
    ),
    ?assert(MCPToKernel, "No relationship between MCP Face and Case Kernel"),

    ok.

%%% ============================================================================
%%% Export Format Tests
%%% ============================================================================

test_plantuml_context() ->
    PlantUML = iolist_to_binary(pqc_c4_model:to_plantuml(context)),

    %% Should contain PlantUML header
    ?assertMatch({match, _}, re:run(PlantUML, "@startuml")),
    ?assertMatch({match, _}, re:run(PlantUML, "@enduml")),

    %% Should include C4-PlantUML library
    ?assertMatch({match, _}, re:run(PlantUML, "C4_Context.puml")),

    %% Should contain system
    ?assertMatch({match, _}, re:run(PlantUML, "System\\(pqchain")),

    %% Should contain people
    ?assertMatch({match, _}, re:run(PlantUML, "Person\\(")),

    %% Should contain relationships
    ?assertMatch({match, _}, re:run(PlantUML, "Rel\\(")),

    ok.

test_plantuml_container() ->
    PlantUML = iolist_to_binary(pqc_c4_model:to_plantuml(container)),

    %% Should contain PlantUML header
    ?assertMatch({match, _}, re:run(PlantUML, "@startuml")),
    ?assertMatch({match, _}, re:run(PlantUML, "@enduml")),

    %% Should include C4-PlantUML library
    ?assertMatch({match, _}, re:run(PlantUML, "C4_Container.puml")),

    %% Should contain containers
    ?assertMatch({match, _}, re:run(PlantUML, "Container\\(")),

    %% Should have system boundary
    ?assertMatch({match, _}, re:run(PlantUML, "System_Boundary")),

    ok.

test_plantuml_component() ->
    PlantUML = iolist_to_binary(pqc_c4_model:to_plantuml(component)),

    %% Should contain PlantUML header
    ?assertMatch({match, _}, re:run(PlantUML, "@startuml")),
    ?assertMatch({match, _}, re:run(PlantUML, "@enduml")),

    %% Should include C4-PlantUML library
    ?assertMatch({match, _}, re:run(PlantUML, "C4_Component.puml")),

    %% Should contain components
    ?assertMatch({match, _}, re:run(PlantUML, "Component\\(")),

    %% Should have container boundary
    ?assertMatch({match, _}, re:run(PlantUML, "Container_Boundary")),

    ok.

test_structurizr_dsl() ->
    DSL = iolist_to_binary(pqc_c4_model:to_structurizr_dsl()),

    %% Should contain workspace definition
    ?assertMatch({match, _}, re:run(DSL, "workspace")),

    %% Should contain model
    ?assertMatch({match, _}, re:run(DSL, "model \\{")),

    %% Should contain people
    ?assertMatch({match, _}, re:run(DSL, "person")),

    %% Should contain software system
    ?assertMatch({match, _}, re:run(DSL, "softwareSystem")),

    %% Should contain containers
    ?assertMatch({match, _}, re:run(DSL, "container")),

    %% Should contain relationships
    ?assertMatch({match, _}, re:run(DSL, "->")),

    %% Should contain views
    ?assertMatch({match, _}, re:run(DSL, "views \\{")),
    ?assertMatch({match, _}, re:run(DSL, "systemContext")),

    ok.

test_mermaid_context() ->
    Mermaid = iolist_to_binary(pqc_c4_model:to_mermaid(context)),

    %% Should be a graph
    ?assertMatch({match, _}, re:run(Mermaid, "graph TB")),

    %% Should have class definitions
    ?assertMatch({match, _}, re:run(Mermaid, "classDef")),

    %% Should have nodes
    ?assertMatch({match, _}, re:run(Mermaid, "\\[")),

    %% Should have relationships
    ?assertMatch({match, _}, re:run(Mermaid, "-->")),

    ok.

test_mermaid_container() ->
    Mermaid = iolist_to_binary(pqc_c4_model:to_mermaid(container)),

    %% Should be a graph
    ?assertMatch({match, _}, re:run(Mermaid, "graph TB")),

    %% Should have containers
    ?assertMatch({match, _}, re:run(Mermaid, "case_kernel")),
    ?assertMatch({match, _}, re:run(Mermaid, "a2a_face")),
    ?assertMatch({match, _}, re:run(Mermaid, "mcp_face")),

    ok.

%%% ============================================================================
%%% Element Lookup Tests
%%% ============================================================================

test_find_element() ->
    %% Find person
    ?assertMatch({ok, _}, pqc_c4_model:find_element(person, <<"human_user">>)),

    %% Find system
    ?assertMatch({ok, _}, pqc_c4_model:find_element(system, <<"pqchain">>)),

    %% Find container
    ?assertMatch({ok, _}, pqc_c4_model:find_element(container, <<"case_kernel">>)),

    %% Find component
    ?assertMatch({ok, _}, pqc_c4_model:find_element(component, <<"root_supervisor">>)),

    %% Not found
    ?assertEqual({error, not_found}, pqc_c4_model:find_element(person, <<"invalid">>)),

    ok.
