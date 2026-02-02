-module(erlmcp_tool_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_tool Integration Workflows
%% Chicago School TDD - Real processes, API boundaries only
%%====================================================================

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [?_test(test_complete_tool_workflow()),
     ?_test(test_tool_validation_pipeline()),
     ?_test(test_multiple_tools())].

test_complete_tool_workflow() ->
    %% Create a tool
    Tool =
        #mcp_tool{name = <<"weather">>,
                  description = <<"Get weather information">>,
                  input_schema =
                      #{type => <<"object">>,
                        properties =>
                            #{city => #{type => <<"string">>},
                              units => #{type => <<"string">>, enum => [<<"C">>, <<"F">>]}},
                        required => [<<"city">>]}},

    %% Validate it
    ?assertEqual(ok, erlmcp_tool:validate_tool(Tool)),

    %% Encode it
    Encoded = erlmcp_tool:encode_tool(Tool),
    ?assert(maps:is_key(<<"name">>, Encoded)),
    ?assert(maps:is_key(<<"description">>, Encoded)),
    ?assert(maps:is_key(<<"inputSchema">>, Encoded)),

    %% Decode it back
    Decoded = erlmcp_tool:decode_tool(Encoded),
    ?assertEqual(Tool, Decoded).

test_tool_validation_pipeline() ->
    %% Valid tool
    ValidTool =
        #mcp_tool{name = <<"valid">>,
                  description = <<"Valid tool">>,
                  input_schema = #{}},
    ?assertEqual(ok, erlmcp_tool:validate_tool(ValidTool)),

    %% Invalid name
    InvalidName = #mcp_tool{name = <<>>, description = <<"Valid description">>},
    ?assertMatch({error, invalid_tool_name}, erlmcp_tool:validate_tool(InvalidName)),

    %% Invalid description (too long)
    InvalidDesc = #mcp_tool{name = <<"valid">>, description = binary:copy(<<"x">>, 10001)},
    ?assertMatch({error, {description_too_long, _}}, erlmcp_tool:validate_tool(InvalidDesc)).

test_multiple_tools() ->
    Tools =
        [#mcp_tool{name = <<"tool1">>, description = <<"First tool">>},
         #mcp_tool{name = <<"tool2">>, description = <<"Second tool">>},
         #mcp_tool{name = <<"tool3">>, description = <<"Third tool">>}],

    %% Validate all
    Results = [erlmcp_tool:validate_tool(T) || T <- Tools],
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),

    %% Encode all
    Encoded = [erlmcp_tool:encode_tool(T) || T <- Tools],
    ?assertEqual(3, length(Encoded)),

    %% Decode all
    Decoded = [erlmcp_tool:decode_tool(E) || E <- Encoded],
    ?assertEqual(Tools, Decoded).

%%====================================================================
%% End-to-End Tests
%%====================================================================

end_to_end_test_() ->
    [?_test(test_batch_tool_workflow()), ?_test(test_complex_tool_workflow())].

test_batch_tool_workflow() ->
    %% Simulate batch processing of tools
    ToolList =
        [#mcp_tool{name = <<"batch_tool_", (integer_to_binary(I))/binary>>,
                   description = <<"Batch tool ", (integer_to_binary(I))/binary>>,
                   input_schema = #{type => <<"string">>}}
         || I <- lists:seq(1, 50)],

    %% Validate all
    ?assertEqual(50, length(ToolList)),
    Validated = [erlmcp_tool:validate_tool(T) || T <- ToolList],
    ?assert(lists:all(fun(R) -> R =:= ok end, Validated)),

    %% Encode all
    EncodedList = [erlmcp_tool:encode_tool(T) || T <- ToolList],
    ?assertEqual(50, length(EncodedList)),

    %% Decode all
    DecodedList = [erlmcp_tool:decode_tool(E) || E <- EncodedList],
    ?assertEqual(ToolList, DecodedList).

test_complex_tool_workflow() ->
    %% Tool with complex nested schema
    ComplexSchema =
        #{type => <<"object">>,
          properties =>
              #{query =>
                    #{type => <<"object">>,
                      properties =>
                          #{field => #{type => <<"string">>},
                            operator =>
                                #{type => <<"string">>,
                                  enum => [<<"eq">>, <<"ne">>, <<"gt">>, <<"lt">>]},
                            value =>
                                #{oneOf =>
                                      [#{type => <<"string">>},
                                       #{type => <<"number">>},
                                       #{type => <<"boolean">>}]}},
                      required => [<<"field">>, <<"operator">>, <<"value">>]},
                options =>
                    #{type => <<"object">>,
                      properties =>
                          #{limit =>
                                #{type => <<"integer">>,
                                  minimum => 1,
                                  maximum => 1000},
                            offset => #{type => <<"integer">>, minimum => 0}}}},
          required => [<<"query">>]},

    Tool =
        #mcp_tool{name = <<"complex_tool">>,
                  description = <<"Tool with complex schema">>,
                  input_schema = ComplexSchema},

    %% Full workflow
    ?assertEqual(ok, erlmcp_tool:validate_tool(Tool)),
    Encoded = erlmcp_tool:encode_tool(Tool),
    Decoded = erlmcp_tool:decode_tool(Encoded),
    ?assertEqual(Tool, Decoded).
