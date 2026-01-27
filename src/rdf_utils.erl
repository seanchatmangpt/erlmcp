%%%-------------------------------------------------------------------
%%% @doc RDF Utilities for Triple Generation and SPARQL
%%%
%%% Provides helper functions for RDF/Turtle generation and
%%% SPARQL query execution via Python rdflib.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rdf_utils).

-export([
    % RDF Generation
    format_triple/3,
    format_triples/1,
    uri/1,
    literal/1,
    typed_literal/2,

    % SPARQL Execution
    execute_sparql/2,
    execute_sparql_file/2,

    % Python Port Management
    start_rdf_port/0,
    stop_rdf_port/1,

    % Conversion
    turtle_to_ntriples/1,
    turtle_to_jsonld/1,

    % Validation
    validate_with_shacl/2
]).

-define(PYTHON_SCRIPT, "priv/python/rdf_query.py").
-define(SHACL_SCRIPT, "priv/python/shacl_validate.py").

%%%===================================================================
%%% API - RDF Generation
%%%===================================================================

-spec format_triple(Subject :: binary(), Predicate :: binary(), Object :: binary()) -> binary().
format_triple(Subject, Predicate, Object) ->
    iolist_to_binary([Subject, " ", Predicate, " ", Object, " .\n"]).

-spec format_triples([{binary(), binary(), binary()}]) -> binary().
format_triples(Triples) ->
    iolist_to_binary([format_triple(S, P, O) || {S, P, O} <- Triples]).

-spec uri(Value :: binary()) -> binary().
uri(Value) ->
    <<"<", Value/binary, ">">>.

-spec literal(Value :: binary()) -> binary().
literal(Value) ->
    Escaped = escape_string(Value),
    <<"\"", Escaped/binary, "\"">>.

-spec typed_literal(Value :: binary(), Type :: binary()) -> binary().
typed_literal(Value, Type) ->
    Lit = literal(Value),
    <<Lit/binary, "^^", Type/binary>>.

%%%===================================================================
%%% API - SPARQL Execution
%%%===================================================================

-spec execute_sparql(OntologyFiles :: [string()], Query :: binary()) -> {ok, [map()]} | {error, term()}.
execute_sparql(OntologyFiles, Query) ->
    Port = start_rdf_port(),
    try
        % Send command to Python port
        Command = #{
            action => query,
            files => [list_to_binary(F) || F <- OntologyFiles],
            query => Query
        },

        CommandJson = jsone:encode(Command),
        port_command(Port, [CommandJson, "\n"]),

        % Receive response
        receive
            {Port, {data, Response}} ->
                Result = jsone:decode(Response, [{object_format, map}]),
                case maps:get(<<"status">>, Result) of
                    <<"ok">> ->
                        {ok, maps:get(<<"results">>, Result, [])};
                    <<"error">> ->
                        {error, maps:get(<<"message">>, Result, <<"unknown error">>)}
                end
        after 10000 ->
            {error, timeout}
        end
    after
        stop_rdf_port(Port)
    end.

-spec execute_sparql_file(OntologyFiles :: [string()], QueryFile :: string()) -> {ok, [map()]} | {error, term()}.
execute_sparql_file(OntologyFiles, QueryFile) ->
    {ok, Query} = file:read_file(QueryFile),
    execute_sparql(OntologyFiles, Query).

%%%===================================================================
%%% API - Python Port Management
%%%===================================================================

-spec start_rdf_port() -> port().
start_rdf_port() ->
    open_port({spawn_executable, find_python()}, [
        {args, [?PYTHON_SCRIPT]},
        {line, 10000},
        binary,
        exit_status
    ]).

-spec stop_rdf_port(Port :: port()) -> ok.
stop_rdf_port(Port) ->
    catch port_close(Port),
    ok.

%%%===================================================================
%%% API - Conversion
%%%===================================================================

-spec turtle_to_ntriples(TurtleContent :: binary()) -> {ok, binary()} | {error, term()}.
turtle_to_ntriples(TurtleContent) ->
    Port = start_rdf_port(),
    try
        Command = #{
            action => convert,
            format => ntriples,
            content => TurtleContent
        },

        CommandJson = jsone:encode(Command),
        port_command(Port, [CommandJson, "\n"]),

        receive
            {Port, {data, Response}} ->
                Result = jsone:decode(Response, [{object_format, map}]),
                case maps:get(<<"status">>, Result) of
                    <<"ok">> ->
                        {ok, maps:get(<<"content">>, Result)};
                    <<"error">> ->
                        {error, maps:get(<<"message">>, Result)}
                end
        after 10000 ->
            {error, timeout}
        end
    after
        stop_rdf_port(Port)
    end.

-spec turtle_to_jsonld(TurtleContent :: binary()) -> {ok, binary()} | {error, term()}.
turtle_to_jsonld(TurtleContent) ->
    Port = start_rdf_port(),
    try
        Command = #{
            action => convert,
            format => jsonld,
            content => TurtleContent
        },

        CommandJson = jsone:encode(Command),
        port_command(Port, [CommandJson, "\n"]),

        receive
            {Port, {data, Response}} ->
                Result = jsone:decode(Response, [{object_format, map}]),
                case maps:get(<<"status">>, Result) of
                    <<"ok">> ->
                        {ok, maps:get(<<"content">>, Result)};
                    <<"error">> ->
                        {error, maps:get(<<"message">>, Result)}
                end
        after 10000 ->
            {error, timeout}
        end
    after
        stop_rdf_port(Port)
    end.

%%%===================================================================
%%% API - Validation
%%%===================================================================

-spec validate_with_shacl(DataFiles :: [string()], ShapesFile :: string()) -> {ok, valid} | {error, [map()]}.
validate_with_shacl(DataFiles, ShapesFile) ->
    Port = start_validation_port(),
    try
        Command = #{
            data_files => [list_to_binary(F) || F <- DataFiles],
            shapes_file => list_to_binary(ShapesFile)
        },

        CommandJson = jsone:encode(Command),
        port_command(Port, [CommandJson, "\n"]),

        receive
            {Port, {data, Response}} ->
                Result = jsone:decode(Response, [{object_format, map}]),
                case maps:get(<<"status">>, Result) of
                    <<"valid">> ->
                        {ok, valid};
                    <<"invalid">> ->
                        {error, maps:get(<<"violations">>, Result, [])};
                    <<"error">> ->
                        {error, maps:get(<<"message">>, Result)}
                end
        after 10000 ->
            {error, timeout}
        end
    after
        stop_rdf_port(Port)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_validation_port() ->
    open_port({spawn_executable, find_python()}, [
        {args, [?SHACL_SCRIPT]},
        {line, 10000},
        binary,
        exit_status
    ]).

find_python() ->
    case os:find_executable("python3") of
        false ->
            case os:find_executable("python") of
                false -> error(python_not_found);
                Path -> Path
            end;
        Path ->
            Path
    end.

escape_string(Value) ->
    binary:replace(Value, <<"\"">>, <<"\\\"">>, [global]).
