-module(erlmcp_unwrap_utils_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% unwrap_tuple/2 tests
%%--------------------------------------------------------------------

unwrap_tuple_single_element_test() ->
    ?assertEqual({ok, a}, erlmcp_unwrap_utils:unwrap_tuple({a}, 1)).

unwrap_tuple_multiple_elements_test() ->
    ?assertEqual({ok, b}, erlmcp_unwrap_utils:unwrap_tuple({a, b, c}, 2)),
    ?assertEqual({ok, c}, erlmcp_unwrap_utils:unwrap_tuple({a, b, c}, 3)).

unwrap_tuple_nested_tuple_test() ->
    ?assertEqual({ok, {x, y}}, erlmcp_unwrap_utils:unwrap_tuple({a, {x, y}, c}, 2)).

unwrap_tuple_index_out_of_bounds_test() ->
    ?assertMatch({error, {invalid_structure, {index_out_of_bounds, 5, 3}}},
                 erlmcp_unwrap_utils:unwrap_tuple({a, b, c}, 5)).

unwrap_tuple_not_a_tuple_test() ->
    ?assertMatch({error, {invalid_structure, {not_a_tuple, _}}},
                 erlmcp_unwrap_utils:unwrap_tuple(not_a_tuple, 1)),
    ?assertMatch({error, {invalid_structure, {not_a_tuple, _}}},
                 erlmcp_unwrap_utils:unwrap_tuple(<<"binary">>, 1)),
    ?assertMatch({error, {invalid_structure, {not_a_tuple, _}}},
                 erlmcp_unwrap_utils:unwrap_tuple([list], 1)).

unwrap_tuple_invalid_index_test() ->
    ?assertEqual({ok, a}, erlmcp_unwrap_utils:unwrap_tuple({a, b}, 1)),
    ?assertMatch({error, {invalid_structure, {not_a_tuple, _}}},
                 erlmcp_unwrap_utils:unwrap_tuple({a, b}, 0)).

%%--------------------------------------------------------------------
%% unwrap_binary/2 tests
%%--------------------------------------------------------------------

unwrap_binary_single_byte_test() ->
    ?assertEqual({ok, 97}, erlmcp_unwrap_utils:unwrap_binary(<<"a">>, 0)).

unwrap_binary_multiple_bytes_test() ->
    ?assertEqual({ok, 97}, erlmcp_unwrap_utils:unwrap_binary(<<"abc">>, 0)),
    ?assertEqual({ok, 98}, erlmcp_unwrap_utils:unwrap_binary(<<"abc">>, 1)),
    ?assertEqual({ok, 99}, erlmcp_unwrap_utils:unwrap_binary(<<"abc">>, 2)).

unwrap_binary_position_out_of_bounds_test() ->
    ?assertMatch({error, {invalid_structure, {position_out_of_bounds, 10, 3}}},
                 erlmcp_unwrap_utils:unwrap_binary(<<"abc">>, 10)).

unwrap_binary_not_a_binary_test() ->
    ?assertMatch({error, {invalid_structure, {not_a_binary, _}}},
                 erlmcp_unwrap_utils:unwrap_binary(not_a_binary, 0)),
    ?assertMatch({error, {invalid_structure, {not_a_binary, _}}},
                 erlmcp_unwrap_utils:unwrap_binary({tuple}, 0)),
    ?assertMatch({error, {invalid_structure, {not_a_binary, _}}},
                 erlmcp_unwrap_utils:unwrap_binary([list], 0)).

%%--------------------------------------------------------------------
%% unwrap_tuple_nested/2 tests
%%--------------------------------------------------------------------

unwrap_tuple_nested_simple_test() ->
    ?assertEqual({ok, b}, erlmcp_unwrap_utils:unwrap_tuple_nested({a, b, c}, [2])).

unwrap_tuple_nested_deep_test() ->
    Nested = {{{a, b}, {c, d}}, {e, f}},
    ?assertEqual({ok, c}, erlmcp_unwrap_utils:unwrap_tuple_nested(Nested, [1, 2, 1])).

unwrap_tuple_nested_with_error_test() ->
    ?assertMatch({error, {invalid_structure, {_, _, _}}},
                 erlmcp_unwrap_utils:unwrap_tuple_nested({a, b}, [1, 5])).

unwrap_tuple_nested_empty_path_test() ->
    ?assertEqual({ok, {a, b, c}},
                 erlmcp_unwrap_utils:unwrap_tuple_nested({a, b, c}, [])).

%%--------------------------------------------------------------------
%% unwrap_binary_nested/2 tests
%%--------------------------------------------------------------------

unwrap_binary_nested_single_byte_test() ->
    ?assertEqual({ok, [97]}, erlmcp_unwrap_utils:unwrap_binary_nested(<<"a">>, [0])).

unwrap_binary_nested_multiple_bytes_test() ->
    ?assertEqual({ok, [97, 99]},
                 erlmcp_unwrap_utils:unwrap_binary_nested(<<"abc">>, [0, 2])).

unwrap_binary_nested_empty_path_test() ->
    ?assertEqual({ok, []},
                 erlmcp_unwrap_utils:unwrap_binary_nested(<<"abc">>, [])).

%%--------------------------------------------------------------------
%% extract_rpc_response/1 tests
%%--------------------------------------------------------------------

extract_rpc_response_valid_test() ->
    Response = #{<<"result">> => #{<<"data">> => <<"value">>}, <<"id">> => 1},
    ?assertEqual({ok, #{<<"data">> => <<"value">>}},
                 erlmcp_unwrap_utils:extract_rpc_response(Response)).

extract_rpc_response_atom_key_test() ->
    Response = #{result => #{data => <<"value">>}, id => 1},
    ?assertEqual({ok, #{data => <<"value">>}},
                 erlmcp_unwrap_utils:extract_rpc_response(Response)).

extract_rpc_response_tuple_wrapper_test() ->
    Response = #{<<"result">> => <<"success">>},
    ?assertEqual({ok, <<"success">>},
                 erlmcp_unwrap_utils:extract_rpc_response({ok, Response})).

extract_rpc_response_invalid_test() ->
    ?assertEqual({error, invalid_response},
                 erlmcp_unwrap_utils:extract_rpc_response(not_a_map)),
    ?assertEqual({error, invalid_response},
                 erlmcp_unwrap_utils:extract_rpc_response(#{<<"error">> => <<"oops">>})).

%%--------------------------------------------------------------------
%% extract_rpc_error/1 tests
%%--------------------------------------------------------------------

extract_rpc_error_valid_test() ->
    Response = #{<<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid">>}},
    ?assertEqual({ok, #{<<"code">> => -32600, <<"message">> => <<"Invalid">>}},
                 erlmcp_unwrap_utils:extract_rpc_error(Response)).

extract_rpc_error_atom_key_test() ->
    Response = #{error => #{code => -32600, message => <<"Invalid">>}},
    ?assertEqual({ok, #{code => -32600, message => <<"Invalid">>}},
                 erlmcp_unwrap_utils:extract_rpc_error(Response)).

extract_rpc_error_no_error_field_test() ->
    Response = #{<<"result">> => <<"success">>},
    ?assertEqual({error, invalid_response},
                 erlmcp_unwrap_utils:extract_rpc_error(Response)).

extract_rpc_error_invalid_test() ->
    ?assertEqual({error, invalid_response},
                 erlmcp_unwrap_utils:extract_rpc_error(not_a_map)).

%%--------------------------------------------------------------------
%% extract_nested/3 tests
%%--------------------------------------------------------------------

extract_nested_map_path_test() ->
    Data = #{outer => #{inner => <<"value">>}},
    ?assertEqual({ok, <<"value">>},
                 erlmcp_unwrap_utils:extract_nested(Data, [outer, inner], undefined)).

extract_nested_mixed_path_test() ->
    Data = #{outer => {inner, <<"value">>}},
    ?assertEqual({ok, <<"value">>},
                 erlmcp_unwrap_utils:extract_nested(Data, [outer, 2], undefined)).

extract_nested_default_value_test() ->
    Data = #{outer => #{inner => <<"value">>}},
    ?assertEqual({ok, default},
                 erlmcp_unwrap_utils:extract_nested(Data, [outer, missing], default)).

extract_nested_invalid_map_test() ->
    ?assertEqual({ok, default},
                 erlmcp_unwrap_utils:extract_nested(not_a_map, [key], default)).

%%--------------------------------------------------------------------
%% safe_extract/3 tests
%%--------------------------------------------------------------------

safe_extract_success_test() ->
    Data = #{user => #{profile => #{name => <<"Alice">>}}},
    OnError = fun(Err) -> {error, Err} end,
    ?assertEqual({ok, <<"Alice">>},
                 erlmcp_unwrap_utils:safe_extract(Data, [user, profile, name], OnError)).

safe_extract_error_test() ->
    Data = #{user => #{profile => #{age => 30}}},
    OnError = fun(Err) -> {error, Err} end,
    Result = erlmcp_unwrap_utils:safe_extract(Data, [user, profile, name], OnError),
    ?assertMatch({error, {invalid_path, [user, profile, name], _, _}}, Result).

safe_extract_partial_path_test() ->
    Data = #{user => #{profile => #{age => 30}}},
    OnError = fun(_Err) -> {error, not_found} end,
    Result = erlmcp_unwrap_utils:safe_extract(Data, [user, missing], OnError),
    ?assertEqual({error, not_found}, Result).

%%--------------------------------------------------------------------
%% get_in/2 tests
%%--------------------------------------------------------------------

get_in_simple_map_test() ->
    Data = #{key => <<"value">>},
    ?assertEqual({ok, <<"value">>},
                 erlmcp_unwrap_utils:get_in(Data, [key])).

get_in_nested_map_test() ->
    Data = #{user => #{profile => #{name => <<"Alice">>}}},
    ?assertEqual({ok, <<"Alice">>},
                 erlmcp_unwrap_utils:get_in(Data, [user, profile, name])).

get_in_mixed_structures_test() ->
    Data = #{data => {<<"nested">>, #{value => 42}}},
    ?assertEqual({ok, #{value => 42}},
                 erlmcp_unwrap_utils:get_in(Data, [data, 2])).

get_in_key_not_found_test() ->
    Data = #{user => #{profile => #{age => 30}}},
    Result = erlmcp_unwrap_utils:get_in(Data, [user, profile, name]),
    ?assertMatch({error, {not_found, [user, profile, name], _}}, Result).

get_in_invalid_structure_test() ->
    Result = erlmcp_unwrap_utils:get_in(not_a_map, [key]),
    ?assertMatch({error, {not_found, [key], _}}, Result).

%%--------------------------------------------------------------------
%% safe_get_in/2 tests
%%--------------------------------------------------------------------

safe_get_in_found_test() ->
    Data = #{key => <<"value">>},
    ?assertEqual(<<"value">>,
                 erlmcp_unwrap_utils:safe_get_in(Data, [key])).

safe_get_in_not_found_test() ->
    Data = #{key => <<"value">>},
    ?assertEqual(undefined,
                 erlmcp_unwrap_utils:safe_get_in(Data, [missing_key])).

safe_get_in_nested_test() ->
    Data = #{a => #{b => #{c => <<"deep">>}}},
    ?assertEqual(<<"deep">>,
                 erlmcp_unwrap_utils:safe_get_in(Data, [a, b, c])).

%%--------------------------------------------------------------------
%% gen_server tests
%%--------------------------------------------------------------------

start_link_test() ->
    {ok, Pid} = erlmcp_unwrap_utils:start_link(),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid),
    ?assertEqual(false, erlang:is_process_alive(Pid)).

start_link_with_options_test() ->
    {ok, Pid} = erlmcp_unwrap_utils:start_link([]),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

%%%===================================================================
%%% Test Generators
%%%===================================================================

unwrap_tuple_property_test() ->
    ?FORALL({Tuple, Index},
             {tuple(integer(0, 255)), integer(1, 10)},
             begin
                 case erlmcp_unwrap_utils:unwrap_tuple(Tuple, Index) of
                     {ok, _Element} when Index =< tuple_size(Tuple) ->
                         true;
                     {error, {invalid_structure, {index_out_of_bounds, _, _}}}
                       when Index > tuple_size(Tuple) ->
                         true;
                     _ ->
                         false
                 end
             end).

unwrap_binary_property_test() ->
    ?FORALL({Binary, Position},
             {binary(), integer(0, 100)},
             begin
                 case erlmcp_unwrap_utils:unwrap_binary(Binary, Position) of
                     {ok, _Byte} when Position < byte_size(Binary) ->
                         true;
                     {error, {invalid_structure, {position_out_of_bounds, _, _}}}
                       when Position >= byte_size(Binary) ->
                         true;
                     _ ->
                         false
                 end
             end).
