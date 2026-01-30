-module(erlmcp_completion_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

completion_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun test_register_completion/0,
            fun test_unregister_completion/0,
            fun test_list_completions/0,
            fun test_get_completion/0,
            fun test_complete_success/0,
            fun test_complete_not_found/0,
            fun test_complete_handler_error/0,
            fun test_validate_completion_request/0,
            fun test_validate_completion_result/0,
            fun test_encode_completion/0,
            fun test_encode_completion_result/0,
            fun test_decode_completion_request/0,
            fun test_decode_invalid_request/0
        ]
    }.

setup() ->
    #{}.

cleanup(_) ->
    ok.

%%====================================================================
%% Registration Tests
%%====================================================================

test_register_completion() ->
    Handler = fun(_Req) -> #mcp_completion_result{completions = []} end,
    Registry = erlmcp_completion:register_completion(#{}, <<"test">>, Handler),
    ?assertEqual([<<"test">>], erlmcp_completion:list_completions(Registry)),
    ?assertMatch({ok, _}, erlmcp_completion:get_completion(Registry, <<"test">>)).

test_unregister_completion() ->
    Handler = fun(_Req) -> #mcp_completion_result{completions = []} end,
    Registry = erlmcp_completion:register_completion(#{}, <<"test">>, Handler),
    Registry2 = erlmcp_completion:unregister_completion(Registry, <<"test">>),
    ?assertEqual([], erlmcp_completion:list_completions(Registry2)),
    ?assertEqual({error, not_found}, erlmcp_completion:get_completion(Registry2, <<"test">>)).

test_list_completions() ->
    Handler1 = fun(_Req) -> #mcp_completion_result{completions = []} end,
    Handler2 = fun(_Req) -> #mcp_completion_result{completions = []} end,
    Registry = erlmcp_completion:register_completion(#{}, <<"test1">>, Handler1),
    Registry2 = erlmcp_completion:register_completion(Registry, <<"test2">>, Handler2),
    Completions = lists:sort(erlmcp_completion:list_completions(Registry2)),
    ?assertEqual([<<"test1">>, <<"test2">>], Completions).

test_get_completion() ->
    Handler = fun(_Req) -> #mcp_completion_result{completions = []} end,
    Registry = erlmcp_completion:register_completion(#{}, <<"test">>, Handler),
    ?assertMatch({ok, _}, erlmcp_completion:get_completion(Registry, <<"test">>)),
    ?assertEqual({error, not_found}, erlmcp_completion:get_completion(Registry, <<"nonexistent">>)).

%%====================================================================
%% Completion Execution Tests
%%====================================================================

test_complete_success() ->
    %% Create handler that returns specific completions
    Handler = fun(#mcp_completion_request{argument = Arg}) ->
        Prefix = Arg#mcp_completion_argument.value,
        Completions = [
            #mcp_completion{value = <<Prefix/binary, "Option1">>, score = 0.9},
            #mcp_completion{value = <<Prefix/binary, "Option2">>, score = 0.8}
        ],
        #mcp_completion_result{completions = Completions, hasMore = false}
    end,

    Registry = erlmcp_completion:register_completion(#{}, <<"my_prompt">>, Handler),

    Request = #mcp_completion_request{
        ref = #mcp_completion_ref{type = ref_prompt, name = <<"my_prompt">>},
        argument = #mcp_completion_argument{name = <<"arg1">>, value = <<"test">>},
        context = #{}
    },

    {ok, Result} = erlmcp_completion:complete(Registry, <<"my_prompt">>, Request),
    ?assertMatch(#mcp_completion_result{}, Result),
    ?assertEqual(2, length(Result#mcp_completion_result.completions)),
    ?assertEqual(false, Result#mcp_completion_result.hasMore).

test_complete_not_found() ->
    Registry = #{},
    Request = #mcp_completion_request{
        ref = #mcp_completion_ref{type = ref_prompt, name = <<"nonexistent">>},
        argument = #mcp_completion_argument{name = <<"arg">>, value = <<"val">>},
        context = #{}
    },
    ?assertMatch({error, {completion_not_found, _}},
                 erlmcp_completion:complete(Registry, <<"nonexistent">>, Request)).

test_complete_handler_error() ->
    %% Handler that throws an error
    Handler = fun(_Req) -> error(handler_failed) end,
    Registry = erlmcp_completion:register_completion(#{}, <<"bad_handler">>, Handler),

    Request = #mcp_completion_request{
        ref = #mcp_completion_ref{type = ref_prompt, name = <<"bad_handler">>},
        argument = #mcp_completion_argument{name = <<"arg">>, value = <<"val">>},
        context = #{}
    },

    ?assertMatch({error, {completion_failed, _}},
                 erlmcp_completion:complete(Registry, <<"bad_handler">>, Request)).

%%====================================================================
%% Validation Tests
%%====================================================================

test_validate_completion_request() ->
    %% Valid request
    ValidReq = #mcp_completion_request{
        ref = #mcp_completion_ref{type = ref_prompt, name = <<"test">>},
        argument = #mcp_completion_argument{name = <<"arg">>, value = <<"val">>},
        context = #{<<"other">> => <<"data">>}
    },
    ?assertEqual(ok, erlmcp_completion:validate_completion_request(ValidReq)),

    %% Invalid ref type
    InvalidRef = #mcp_completion_request{
        ref = #mcp_completion_ref{type = invalid_type, name = <<"test">>},
        argument = #mcp_completion_argument{name = <<"arg">>, value = <<"val">>},
        context = #{}
    },
    ?assertMatch({error, _}, erlmcp_completion:validate_completion_request(InvalidRef)),

    %% Empty name
    EmptyName = #mcp_completion_request{
        ref = #mcp_completion_ref{type = ref_prompt, name = <<"">>},
        argument = #mcp_completion_argument{name = <<"arg">>, value = <<"val">>},
        context = #{}
    },
    ?assertMatch({error, _}, erlmcp_completion:validate_completion_request(EmptyName)).

test_validate_completion_result() ->
    %% Valid result
    ValidResult = #mcp_completion_result{
        completions = [
            #mcp_completion{value = <<"test1">>, score = 0.9},
            #mcp_completion{value = <<"test2">>}
        ],
        hasMore = false,
        metadata = #{}
    },
    ?assertEqual(ok, erlmcp_completion:validate_completion_result(ValidResult)),

    %% Invalid score
    InvalidScore = #mcp_completion_result{
        completions = [
            #mcp_completion{value = <<"test">>, score = 1.5}  % > 1.0
        ],
        hasMore = false,
        metadata = #{}
    },
    ?assertMatch({error, _}, erlmcp_completion:validate_completion_result(InvalidScore)),

    %% Empty value
    EmptyValue = #mcp_completion_result{
        completions = [
            #mcp_completion{value = <<"">>}
        ],
        hasMore = false,
        metadata = #{}
    },
    ?assertMatch({error, _}, erlmcp_completion:validate_completion_result(EmptyValue)).

%%====================================================================
%% Encoding/Decoding Tests
%%====================================================================

test_encode_completion() ->
    Completion = #mcp_completion{
        value = <<"test_value">>,
        label = <<"Test Label">>,
        description = <<"Test description">>,
        score = 0.85
    },
    Encoded = erlmcp_completion:encode_completion(Completion),
    ?assertEqual(<<"test_value">>, maps:get(<<"value">>, Encoded)),
    ?assertEqual(<<"Test Label">>, maps:get(<<"label">>, Encoded)),
    ?assertEqual(<<"Test description">>, maps:get(<<"description">>, Encoded)),
    ?assertEqual(0.85, maps:get(<<"score">>, Encoded)).

test_encode_completion_result() ->
    Result = #mcp_completion_result{
        completions = [
            #mcp_completion{value = <<"val1">>, score = 0.9},
            #mcp_completion{value = <<"val2">>, score = 0.7}
        ],
        hasMore = true,
        metadata = #{<<"total">> => 10}
    },
    Encoded = erlmcp_completion:encode_completion_result(Result),
    ?assertEqual(true, maps:get(<<"hasMore">>, Encoded)),
    ?assertEqual(2, length(maps:get(<<"completions">>, Encoded))),
    ?assertMatch(#{<<"total">> := 10}, maps:get(<<"metadata">>, Encoded)).

test_decode_completion_request() ->
    Params = #{
        <<"ref">> => #{
            <<"type">> => <<"ref/prompt">>,
            <<"name">> => <<"my_prompt">>
        },
        <<"argument">> => #{
            <<"name">> => <<"arg1">>,
            <<"value">> => <<"test_val">>
        },
        <<"context">> => #{
            <<"arg2">> => <<"context_val">>
        }
    },

    {ok, Request} = erlmcp_completion:decode_completion_request(Params),
    ?assertEqual(ref_prompt, Request#mcp_completion_request.ref#mcp_completion_ref.type),
    ?assertEqual(<<"my_prompt">>, Request#mcp_completion_request.ref#mcp_completion_ref.name),
    ?assertEqual(<<"arg1">>, Request#mcp_completion_request.argument#mcp_completion_argument.name),
    ?assertEqual(<<"test_val">>, Request#mcp_completion_request.argument#mcp_completion_argument.value),
    ?assertMatch(#{<<"arg2">> := <<"context_val">>}, Request#mcp_completion_request.context).

test_decode_invalid_request() ->
    %% Missing required fields
    MissingRef = #{
        <<"argument">> => #{
            <<"name">> => <<"arg">>,
            <<"value">> => <<"val">>
        }
    },
    ?assertMatch({error, {missing_required_fields, _}},
                 erlmcp_completion:decode_completion_request(MissingRef)),

    %% Invalid ref type
    InvalidType = #{
        <<"ref">> => #{
            <<"type">> => <<"invalid_type">>,
            <<"name">> => <<"test">>
        },
        <<"argument">> => #{
            <<"name">> => <<"arg">>,
            <<"value">> => <<"val">>
        }
    },
    ?assertMatch({error, {invalid_ref_type, _}},
                 erlmcp_completion:decode_completion_request(InvalidType)).

%%====================================================================
%% Context Validation Tests
%%====================================================================

test_validate_context() ->
    %% Valid context - all binary keys and values
    ValidContext = #{
        <<"arg1">> => <<"value1">>,
        <<"arg2">> => <<"value2">>
    },
    ?assertEqual(ok, erlmcp_completion:validate_context(ValidContext)),

    %% Empty context is valid
    ?assertEqual(ok, erlmcp_completion:validate_context(#{})),

    %% Invalid context - non-binary key
    InvalidKey = #{
        "not_binary" => <<"value">>
    },
    ?assertMatch({error, invalid_context}, erlmcp_completion:validate_context(InvalidKey)),

    %% Invalid context - non-binary value
    InvalidValue = #{
        <<"key">> => "not_binary"
    },
    ?assertMatch({error, invalid_context}, erlmcp_completion:validate_context(InvalidValue)).
