%%%-------------------------------------------------------------------
%%% @doc Common Test Suite for RDF Utils
%%%
%%% Tests RDF triple generation, SPARQL execution, and format conversion.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rdf_utils_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    % RDF Generation Tests
    test_format_triple/1,
    test_format_triples/1,
    test_uri_generation/1,
    test_literal_escaping/1,
    test_typed_literal/1,

    % SPARQL Execution Tests
    test_execute_sparql/1,
    test_execute_sparql_file/1,

    % Conversion Tests
    test_turtle_to_ntriples/1,
    test_turtle_to_jsonld/1,

    % Validation Tests
    test_shacl_validation_valid/1,
    test_shacl_validation_invalid/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        {group, rdf_generation},
        {group, sparql_execution},
        {group, conversion},
        {group, validation}
    ].

groups() ->
    [
        {rdf_generation, [parallel], [
            test_format_triple,
            test_format_triples,
            test_uri_generation,
            test_literal_escaping,
            test_typed_literal
        ]},
        {sparql_execution, [sequence], [
            test_execute_sparql,
            test_execute_sparql_file
        ]},
        {conversion, [sequence], [
            test_turtle_to_ntriples,
            test_turtle_to_jsonld
        ]},
        {validation, [sequence], [
            test_shacl_validation_valid,
            test_shacl_validation_invalid
        ]}
    ].

init_per_suite(Config) ->
    % Set up test environment
    filelib:ensure_dir("test_rdf/dummy"),
    Config.

end_per_suite(_Config) ->
    % Clean up
    os:cmd("rm -rf test_rdf"),
    ok.

%%%===================================================================
%%% Test Cases - RDF Generation
%%%===================================================================

test_format_triple(_Config) ->
    Triple = rdf_utils:format_triple(
        <<"tcps:Receipt_001">>,
        <<"rdf:type">>,
        <<"tcps:Receipt">>
    ),

    Expected = <<"tcps:Receipt_001 rdf:type tcps:Receipt .\n">>,
    ?assertEqual(Expected, Triple).

test_format_triples(_Config) ->
    Triples = [
        {<<"tcps:Receipt_001">>, <<"rdf:type">>, <<"tcps:Receipt">>},
        {<<"tcps:Receipt_001">>, <<"tcps:stage">>, <<"tcps:COMPILE">>}
    ],

    Result = rdf_utils:format_triples(Triples),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

test_uri_generation(_Config) ->
    Uri = rdf_utils:uri(<<"http://example.org/resource">>),
    ?assertEqual(<<"<http://example.org/resource>">>, Uri).

test_literal_escaping(_Config) ->
    Literal = rdf_utils:literal(<<"He said \"Hello\"">>),
    ?assertEqual(<<"\"He said \\\"Hello\\\"\"">>, Literal).

test_typed_literal(_Config) ->
    TypedLit = rdf_utils:typed_literal(
        <<"2026-01-26T10:00:00Z">>,
        <<"xsd:dateTime">>
    ),

    Expected = <<"\"2026-01-26T10:00:00Z\"^^xsd:dateTime">>,
    ?assertEqual(Expected, TypedLit).

%%%===================================================================
%%% Test Cases - SPARQL Execution
%%%===================================================================

test_execute_sparql(_Config) ->
    % Create test ontology file
    TestFile = "test_rdf/test.ttl",
    Content = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
                "@prefix tcps: <http://example.org/tcps#> .\n\n"
                "tcps:Receipt_001 rdf:type tcps:Receipt ;\n"
                "  tcps:stage tcps:COMPILE .\n">>,
    ok = file:write_file(TestFile, Content),

    Query = <<"PREFIX tcps: <http://example.org/tcps#>\n"
              "SELECT ?receipt WHERE { ?receipt rdf:type tcps:Receipt }">>,

    {ok, Results} = rdf_utils:execute_sparql([TestFile], Query),
    ?assert(is_list(Results)).

test_execute_sparql_file(_Config) ->
    % Create test ontology file
    TestFile = "test_rdf/test2.ttl",
    Content = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
                "@prefix tcps: <http://example.org/tcps#> .\n\n"
                "tcps:WorkOrder_001 rdf:type tcps:WorkOrder .\n">>,
    ok = file:write_file(TestFile, Content),

    % Create query file
    QueryFile = "test_rdf/query.sparql",
    Query = <<"PREFIX tcps: <http://example.org/tcps#>\n"
              "SELECT ?wo WHERE { ?wo rdf:type tcps:WorkOrder }">>,
    ok = file:write_file(QueryFile, Query),

    {ok, Results} = rdf_utils:execute_sparql_file([TestFile], QueryFile),
    ?assert(is_list(Results)).

%%%===================================================================
%%% Test Cases - Conversion
%%%===================================================================

test_turtle_to_ntriples(_Config) ->
    Turtle = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
               "@prefix tcps: <http://example.org/tcps#> .\n\n"
               "tcps:Receipt_001 rdf:type tcps:Receipt .\n">>,

    {ok, NTriples} = rdf_utils:turtle_to_ntriples(Turtle),
    ?assert(is_binary(NTriples)).

test_turtle_to_jsonld(_Config) ->
    Turtle = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
               "@prefix tcps: <http://example.org/tcps#> .\n\n"
               "tcps:Receipt_001 rdf:type tcps:Receipt .\n">>,

    {ok, JsonLd} = rdf_utils:turtle_to_jsonld(Turtle),
    ?assert(is_binary(JsonLd)).

%%%===================================================================
%%% Test Cases - Validation
%%%===================================================================

test_shacl_validation_valid(_Config) ->
    % Create test data file
    DataFile = "test_rdf/valid_data.ttl",
    DataContent = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
                    "@prefix tcps: <http://example.org/tcps#> .\n\n"
                    "tcps:Receipt_001 rdf:type tcps:Receipt ;\n"
                    "  tcps:stage tcps:COMPILE ;\n"
                    "  tcps:timestamp \"2026-01-26T10:00:00Z\"^^xsd:dateTime .\n">>,
    ok = file:write_file(DataFile, DataContent),

    % Create shapes file
    ShapesFile = "test_rdf/shapes.ttl",
    ShapesContent = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
                      "@prefix sh: <http://www.w3.org/ns/shacl#> .\n"
                      "@prefix tcps: <http://example.org/tcps#> .\n\n"
                      "tcps:ReceiptShape a sh:NodeShape ;\n"
                      "  sh:targetClass tcps:Receipt ;\n"
                      "  sh:property [\n"
                      "    sh:path tcps:stage ;\n"
                      "    sh:minCount 1 ;\n"
                      "  ] .\n">>,
    ok = file:write_file(ShapesFile, ShapesContent),

    Result = rdf_utils:validate_with_shacl([DataFile], ShapesFile),
    ?assertMatch({ok, valid}, Result).

test_shacl_validation_invalid(_Config) ->
    % Create test data file (missing required property)
    DataFile = "test_rdf/invalid_data.ttl",
    DataContent = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
                    "@prefix tcps: <http://example.org/tcps#> .\n\n"
                    "tcps:Receipt_002 rdf:type tcps:Receipt .\n">>,
    ok = file:write_file(DataFile, DataContent),

    % Create shapes file
    ShapesFile = "test_rdf/shapes2.ttl",
    ShapesContent = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
                      "@prefix sh: <http://www.w3.org/ns/shacl#> .\n"
                      "@prefix tcps: <http://example.org/tcps#> .\n\n"
                      "tcps:ReceiptShape a sh:NodeShape ;\n"
                      "  sh:targetClass tcps:Receipt ;\n"
                      "  sh:property [\n"
                      "    sh:path tcps:stage ;\n"
                      "    sh:minCount 1 ;\n"
                      "  ] .\n">>,
    ok = file:write_file(ShapesFile, ShapesContent),

    Result = rdf_utils:validate_with_shacl([DataFile], ShapesFile),
    ?assertMatch({error, _}, Result).
