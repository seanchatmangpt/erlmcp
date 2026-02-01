%%%-------------------------------------------------------------------
%%% @doc
%%% AVRO Formatter Plugin - Converts JSON to Apache AVRO format
%%%
%%% Example plugin demonstrating the erlmcp plugin system.
%%% Converts compliance reports and validation results to AVRO binary format.
%%%
%%% == Usage ==
%%%
%%% From CLI:
%%% ```
%%% erlmcp_validate report --format avro
%%% '''
%%%
%%% Programmatically:
%%% ```
%%% erlmcp_plugin_manager:load_plugin(erlmcp_plugin_avro_formatter),
%%% {ok, Plugins} = erlmcp_plugin_registry:list_plugins_by_type(formatter),
%%% #{pid := Pid} = hd([P || P <- Plugins, maps:get(module, P) =:= erlmcp_plugin_avro_formatter]),
%%% erlmcp_plugin_worker:call_function(Pid, format, [Data]).
%%% '''
%%%
%%% == AVRO Schema ==
%%%
%%% Compliance Report Schema:
%%% ```
%%% {
%%%   "type": "record",
%%%   "name": "ComplianceReport",
%%%   "fields": [
%%%     {"name": "spec_version", "type": "string"},
%%%     {"name": "timestamp", "type": "string"},
%%%     {"name": "compliance_score", "type": "float"},
%%%     {"name": "test_results", "type": {"type": "array", "items": "TestResult"}}
%%%   ]
%%% }
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_avro_formatter).
-behaviour(erlmcp_plugin_formatter).

%% Plugin behavior callbacks
-export([
    init/1,
    format/2,
    supports_format/0,
    metadata/0,
    terminate/2
]).

-record(state, {
    schema :: map(),
    compression :: boolean()
}).

%%====================================================================
%% Plugin Behavior Callbacks
%%====================================================================

%% @doc Plugin metadata
metadata() ->
    #{
        name => <<"avro_formatter">>,
        version => <<"1.0.0">>,
        type => formatter,
        description => <<"Converts JSON to Apache AVRO binary format">>,
        author => <<"erlmcp team">>,
        dependencies => []
    }.

%% @doc Initialize plugin
init(Opts) ->
    Compression = maps:get(compression, Opts, false),
    Schema = get_avro_schema(),
    {ok, #state{schema = Schema, compression = Compression}}.

%% @doc Format data to AVRO
format(Data, State) when is_map(Data) ->
    try
        %% Convert JSON to AVRO format
        AvroData = json_to_avro(Data, State#state.schema),

        %% Encode as binary
        AvroEncoded = encode_avro(AvroData),

        %% Optionally compress
        FinalData = case State#state.compression of
            true -> zlib:compress(AvroEncoded);
            false -> AvroEncoded
        end,

        {ok, FinalData, State}
    catch
        _:Error:Stack ->
            {error, {avro_encoding_failed, Error, Stack}}
    end;
format(_Data, State) ->
    {error, invalid_data_format, State}.

%% @doc Supported format
supports_format() ->
    avro.

%% @doc Terminate plugin
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get AVRO schema for compliance reports
get_avro_schema() ->
    #{
        type => record,
        name => <<"ComplianceReport">>,
        fields => [
            #{name => <<"spec_version">>, type => string},
            #{name => <<"timestamp">>, type => string},
            #{name => <<"compliance_score">>, type => float},
            #{name => <<"status">>, type => string},
            #{name => <<"test_results">>, type => #{
                type => array,
                items => #{
                    type => record,
                    name => <<"TestResult">>,
                    fields => [
                        #{name => <<"name">>, type => string},
                        #{name => <<"status">>, type => string},
                        #{name => <<"evidence">>, type => string}
                    ]
                }
            }}
        ]
    }.

%% @private Convert JSON map to AVRO format
json_to_avro(JsonData, _Schema) ->
    %% This is a simplified conversion
    %% In production, use a proper AVRO library like 'erlavro'
    #{
        <<"spec_version">> => maps:get(spec_version, JsonData, <<"unknown">>),
        <<"timestamp">> => maps:get(timestamp, JsonData, iso8601_now()),
        <<"compliance_score">> => maps:get(compliance_score, JsonData, 0.0),
        <<"status">> => atom_to_binary(maps:get(status, JsonData, unknown)),
        <<"test_results">> => format_test_results(maps:get(test_results, JsonData, []))
    }.

%% @private Format test results for AVRO
format_test_results(TestResults) when is_list(TestResults) ->
    [format_test_result(TR) || TR <- TestResults];
format_test_results(_) ->
    [].

%% @private Format single test result
format_test_result(#{name := Name, status := Status} = TR) ->
    #{
        <<"name">> => ensure_binary(Name),
        <<"status">> => ensure_binary(Status),
        <<"evidence">> => ensure_binary(maps:get(evidence, TR, <<>>))
    };
format_test_result(_) ->
    #{
        <<"name">> => <<"unknown">>,
        <<"status">> => <<"unknown">>,
        <<"evidence">> => <<>>
    }.

%% @private Encode AVRO data to binary
%% NOTE: This is a simplified implementation for demonstration
%% In production, use a proper AVRO library like 'erlavro'
encode_avro(AvroData) when is_map(AvroData) ->
    %% Simplified binary encoding
    %% Format: [field_count][field1_len][field1_data][field2_len][field2_data]...

    Fields = [
        {<<"spec_version">>, maps:get(<<"spec_version">>, AvroData, <<>>)},
        {<<"timestamp">>, maps:get(<<"timestamp">>, AvroData, <<>>)},
        {<<"compliance_score">>, maps:get(<<"compliance_score">>, AvroData, 0.0)},
        {<<"status">>, maps:get(<<"status">>, AvroData, <<>>)},
        {<<"test_results">>, encode_test_results(maps:get(<<"test_results">>, AvroData, []))}
    ],

    EncodedFields = lists:map(fun({_Name, Value}) ->
        encode_field(Value)
    end, Fields),

    FieldCount = length(Fields),
    iolist_to_binary([<<FieldCount:32/integer>>, EncodedFields]).

%% @private Encode field value
encode_field(Value) when is_binary(Value) ->
    Len = byte_size(Value),
    <<Len:32/integer, Value/binary>>;
encode_field(Value) when is_float(Value) ->
    <<Value:64/float>>;
encode_field(Value) when is_integer(Value) ->
    <<Value:64/integer>>;
encode_field(Value) when is_list(Value) ->
    encode_field(iolist_to_binary(Value));
encode_field(Value) when is_atom(Value) ->
    encode_field(atom_to_binary(Value, utf8));
encode_field(_Value) ->
    encode_field(<<>>).

%% @private Encode test results array
encode_test_results(TestResults) when is_list(TestResults) ->
    Count = length(TestResults),
    Encoded = lists:map(fun(TR) ->
        Name = maps:get(<<"name">>, TR, <<>>),
        Status = maps:get(<<"status">>, TR, <<>>),
        Evidence = maps:get(<<"evidence">>, TR, <<>>),
        <<
            (byte_size(Name)):32/integer, Name/binary,
            (byte_size(Status)):32/integer, Status/binary,
            (byte_size(Evidence)):32/integer, Evidence/binary
        >>
    end, TestResults),
    <<Count:32/integer, (iolist_to_binary(Encoded))/binary>>;
encode_test_results(_) ->
    <<0:32/integer>>.

%% @private Ensure value is binary
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(_) -> <<>>.

%% @private Get current ISO8601 timestamp
iso8601_now() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                    [Year, Month, Day, Hour, Min, Sec])).
