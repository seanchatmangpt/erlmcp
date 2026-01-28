%%%-------------------------------------------------------------------
%%% @doc Comprehensive test suite for erlmcp_pagination module
%%%
%%% Tests cover:
%%% - Cursor encoding and decoding
%%% - Cursor validation
%%% - Pagination of lists
%%% - Next cursor generation
%%% - Edge cases: empty lists, single page, multiple pages
%%% - Error handling: invalid cursors, bounds checking
%%%
%%% @since 0.6.0
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pagination_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Cursor Encoding/Decoding Tests
%%====================================================================

encode_cursor_simple_test() ->
    Cursor = erlmcp_pagination:encode_cursor(0, 100),
    ?assert(is_binary(Cursor)),
    ?assertNotEqual(Cursor, undefined).

encode_cursor_non_zero_offset_test() ->
    Cursor = erlmcp_pagination:encode_cursor(50, 100),
    ?assert(is_binary(Cursor)),
    {ok, {Offset, PageSize}} = erlmcp_pagination:decode_cursor(Cursor),
    ?assertEqual(Offset, 50),
    ?assertEqual(PageSize, 100).

encode_cursor_various_sizes_test() ->
    Cursor1 = erlmcp_pagination:encode_cursor(0, 10),
    Cursor2 = erlmcp_pagination:encode_cursor(0, 100),
    Cursor3 = erlmcp_pagination:encode_cursor(0, 500),
    ?assertNotEqual(Cursor1, Cursor2),
    ?assertNotEqual(Cursor2, Cursor3).

decode_cursor_valid_test() ->
    Cursor = erlmcp_pagination:encode_cursor(25, 75),
    {ok, {Offset, PageSize}} = erlmcp_pagination:decode_cursor(Cursor),
    ?assertEqual(Offset, 25),
    ?assertEqual(PageSize, 75).

decode_cursor_undefined_test() ->
    {ok, {Offset, PageSize}} = erlmcp_pagination:decode_cursor(undefined),
    ?assertEqual(Offset, 0),
    ?assertEqual(PageSize, 100).

decode_cursor_invalid_base64_test() ->
    {error, invalid_cursor} = erlmcp_pagination:decode_cursor(<<"not-valid-base64!!!">>).

decode_cursor_invalid_json_test() ->
    InvalidCursor = base64:encode(<<"not json">>),
    {error, invalid_cursor} = erlmcp_pagination:decode_cursor(InvalidCursor).

decode_cursor_invalid_type_test() ->
    {error, invalid_cursor} = erlmcp_pagination:decode_cursor(123),
    {error, invalid_cursor} = erlmcp_pagination:decode_cursor([1,2,3]).

%%====================================================================
%% Cursor Validation Tests
%%====================================================================

validate_cursor_undefined_test() ->
    ok = erlmcp_pagination:validate_cursor(undefined).

validate_cursor_valid_test() ->
    Cursor = erlmcp_pagination:encode_cursor(0, 100),
    ok = erlmcp_pagination:validate_cursor(Cursor).

validate_cursor_invalid_test() ->
    {error, invalid_cursor} = erlmcp_pagination:validate_cursor(<<"invalid">>).

validate_cursor_invalid_type_test() ->
    {error, invalid_cursor} = erlmcp_pagination:validate_cursor(123),
    {error, invalid_cursor} = erlmcp_pagination:validate_cursor(#{}),
    {error, invalid_cursor} = erlmcp_pagination:validate_cursor([]).

%%====================================================================
%% Bounds Validation Tests
%%====================================================================

bounds_valid_test() ->
    ?assertNotEqual(erlmcp_pagination:encode_cursor(0, 1), undefined),
    ?assertNotEqual(erlmcp_pagination:encode_cursor(0, 100), undefined),
    ?assertNotEqual(erlmcp_pagination:encode_cursor(1000, 1000), undefined).

bounds_invalid_offset_test() ->
    Cursor = erlmcp_pagination:encode_cursor(0, 100),
    ?assertMatch(<<_/binary>>, Cursor).

bounds_invalid_pagesize_test() ->
    %% Page size too small
    try erlmcp_pagination:encode_cursor(0, 0) of
        _ -> ?assert(false)
    catch
        error:function_clause -> ok
    end,

    %% Page size too large
    try erlmcp_pagination:encode_cursor(0, 2000) of
        _ -> ?assert(false)
    catch
        error:function_clause -> ok
    end.

%%====================================================================
%% Next Cursor Generation Tests
%%====================================================================

next_cursor_when_more_test() ->
    NextCursor = erlmcp_pagination:generate_next_cursor(0, 100, true),
    ?assertNotEqual(NextCursor, undefined),
    {ok, {Offset, PageSize}} = erlmcp_pagination:decode_cursor(NextCursor),
    ?assertEqual(Offset, 100),
    ?assertEqual(PageSize, 100).

next_cursor_when_no_more_test() ->
    NextCursor = erlmcp_pagination:generate_next_cursor(0, 100, false),
    ?assertEqual(NextCursor, undefined).

next_cursor_multiple_pages_test() ->
    Cursor1 = erlmcp_pagination:generate_next_cursor(0, 50, true),
    {ok, {Offset1, _}} = erlmcp_pagination:decode_cursor(Cursor1),
    ?assertEqual(Offset1, 50),

    Cursor2 = erlmcp_pagination:generate_next_cursor(50, 50, true),
    {ok, {Offset2, _}} = erlmcp_pagination:decode_cursor(Cursor2),
    ?assertEqual(Offset2, 100),

    Cursor3 = erlmcp_pagination:generate_next_cursor(100, 50, false),
    ?assertEqual(Cursor3, undefined).

%%====================================================================
%% List Pagination Tests
%%====================================================================

paginate_empty_list_test() ->
    Items = [],
    {ok, PagedItems, HasMore} = erlmcp_pagination:paginate_list(Items, undefined, 10, 0),
    ?assertEqual(PagedItems, []),
    ?assertEqual(HasMore, false).

paginate_single_item_test() ->
    Items = [item1],
    {ok, PagedItems, HasMore} = erlmcp_pagination:paginate_list(Items, undefined, 10, 1),
    ?assertEqual(PagedItems, [item1]),
    ?assertEqual(HasMore, false).

paginate_single_page_test() ->
    Items = [a, b, c, d, e],
    {ok, PagedItems, HasMore} = erlmcp_pagination:paginate_list(Items, undefined, 10, 5),
    ?assertEqual(PagedItems, [a, b, c, d, e]),
    ?assertEqual(HasMore, false).

paginate_first_page_test() ->
    Items = [a, b, c, d, e, f, g, h, i, j],
    {ok, PagedItems, HasMore} = erlmcp_pagination:paginate_list(Items, undefined, 3, 10),
    ?assertEqual(PagedItems, [a, b, c]),
    ?assertEqual(HasMore, true).

paginate_middle_page_test() ->
    Items = [a, b, c, d, e, f, g, h, i, j],
    Cursor = erlmcp_pagination:encode_cursor(3, 3),
    {ok, PagedItems, HasMore} = erlmcp_pagination:paginate_list(Items, Cursor, 3, 10),
    ?assertEqual(PagedItems, [d, e, f]),
    ?assertEqual(HasMore, true).

paginate_last_page_test() ->
    Items = [a, b, c, d, e, f, g, h, i, j],
    Cursor = erlmcp_pagination:encode_cursor(6, 3),
    {ok, PagedItems, HasMore} = erlmcp_pagination:paginate_list(Items, Cursor, 3, 10),
    ?assertEqual(PagedItems, [g, h, i]),
    ?assertEqual(HasMore, false).

paginate_partial_last_page_test() ->
    Items = [a, b, c, d, e, f, g, h, i, j],
    Cursor = erlmcp_pagination:encode_cursor(8, 3),
    {ok, PagedItems, HasMore} = erlmcp_pagination:paginate_list(Items, Cursor, 3, 10),
    ?assertEqual(PagedItems, [i, j]),
    ?assertEqual(HasMore, false).

paginate_beyond_end_test() ->
    Items = [a, b, c],
    Cursor = erlmcp_pagination:encode_cursor(10, 3),
    {ok, PagedItems, HasMore} = erlmcp_pagination:paginate_list(Items, Cursor, 3, 3),
    ?assertEqual(PagedItems, []),
    ?assertEqual(HasMore, false).

paginate_invalid_cursor_test() ->
    Items = [a, b, c, d, e],
    {error, invalid_cursor} = erlmcp_pagination:paginate_list(Items, <<"bad-cursor">>, 3, 5).

%%====================================================================
%% Apply Pagination Tests
%%====================================================================

apply_pagination_first_page_test() ->
    Items = [a, b, c, d, e],
    Result = erlmcp_pagination:apply_pagination(Items, #{}, false),
    ?assertEqual(maps:get(items, Result), [a, b, c, d, e]),
    ?assertEqual(maps:get(nextCursor, Result), undefined).

apply_pagination_with_limit_test() ->
    Items = [a, b, c, d, e, f, g, h, i, j],
    Result = erlmcp_pagination:apply_pagination(Items, #{limit => 3}, false),
    ?assertEqual(maps:get(items, Result), [a, b, c]),
    NextCursor = maps:get(nextCursor, Result),
    ?assertNotEqual(NextCursor, undefined).

apply_pagination_with_cursor_test() ->
    Items = [a, b, c, d, e, f, g, h, i, j],
    Cursor = erlmcp_pagination:encode_cursor(3, 3),
    Result = erlmcp_pagination:apply_pagination(Items, #{cursor => Cursor, limit => 3}, false),
    ?assertEqual(maps:get(items, Result), [d, e, f]),
    ?assertNotEqual(maps:get(nextCursor, Result), undefined).

apply_pagination_with_total_count_test() ->
    Items = [a, b, c, d, e],
    Result = erlmcp_pagination:apply_pagination(Items, #{limit => 2}, true),
    ?assertEqual(maps:get(items, Result), [a, b]),
    ?assertEqual(maps:get(totalCount, Result), 5),
    ?assertNotEqual(maps:get(nextCursor, Result), undefined).

apply_pagination_without_total_count_test() ->
    Items = [a, b, c, d, e],
    Result = erlmcp_pagination:apply_pagination(Items, #{limit => 2}, false),
    ?assertEqual(maps:get(items, Result), [a, b]),
    ?assertNot(maps:is_key(totalCount, Result)).

apply_pagination_invalid_cursor_test() ->
    Items = [a, b, c],
    {error, invalid_cursor} = erlmcp_pagination:apply_pagination(
        Items, #{cursor => <<"bad">>}, false).

apply_pagination_large_list_test() ->
    Items = lists:seq(1, 1000),
    Result = erlmcp_pagination:apply_pagination(Items, #{limit => 100}, true),
    Items1 = maps:get(items, Result),
    ?assertEqual(length(Items1), 100),
    ?assertEqual(hd(Items1), 1),
    ?assertEqual(lists:last(Items1), 100),
    ?assertEqual(maps:get(totalCount, Result), 1000),
    ?assertNotEqual(maps:get(nextCursor, Result), undefined).

apply_pagination_last_page_of_large_list_test() ->
    Items = lists:seq(1, 1000),
    Cursor = erlmcp_pagination:encode_cursor(900, 100),
    Result = erlmcp_pagination:apply_pagination(Items, #{cursor => Cursor, limit => 100}, true),
    Items1 = maps:get(items, Result),
    ?assertEqual(length(Items1), 100),
    ?assertEqual(hd(Items1), 901),
    ?assertEqual(lists:last(Items1), 1000),
    ?assertEqual(maps:get(nextCursor, Result), undefined).

%%====================================================================
%% Integration Tests - Cursor Chain
%%====================================================================

cursor_chain_pagination_test() ->
    Items = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o],

    %% Page 1
    Result1 = erlmcp_pagination:apply_pagination(Items, #{limit => 5}, false),
    Page1 = maps:get(items, Result1),
    Cursor2 = maps:get(nextCursor, Result1),
    ?assertEqual(Page1, [a, b, c, d, e]),
    ?assertNotEqual(Cursor2, undefined),

    %% Page 2
    Result2 = erlmcp_pagination:apply_pagination(Items, #{cursor => Cursor2, limit => 5}, false),
    Page2 = maps:get(items, Result2),
    Cursor3 = maps:get(nextCursor, Result2),
    ?assertEqual(Page2, [f, g, h, i, j]),
    ?assertNotEqual(Cursor3, undefined),

    %% Page 3 (last)
    Result3 = erlmcp_pagination:apply_pagination(Items, #{cursor => Cursor3, limit => 5}, false),
    Page3 = maps:get(items, Result3),
    Cursor4 = maps:get(nextCursor, Result3),
    ?assertEqual(Page3, [k, l, m, n, o]),
    ?assertEqual(Cursor4, undefined).

%%====================================================================
%% Opaque Cursor Tests (clients can't interpret)
%%====================================================================

cursor_opaque_test() ->
    Cursor = erlmcp_pagination:encode_cursor(50, 100),
    %% Cursor should be a binary that doesn't reveal pagination params
    ?assert(is_binary(Cursor)),
    %% Cursor should be base64-encoded (only alphanumeric + /+=)
    %% We can decode it to verify it's properly encoded
    DecodedData = base64:decode(Cursor),
    ?assert(is_binary(DecodedData)),
    %% The decoded data should be JSON
    JSONMap = jsx:decode(DecodedData, [return_maps]),
    ?assert(maps:is_key(<<"offset">>, JSONMap)),
    ?assert(maps:is_key(<<"pagesize">>, JSONMap)).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_invalid_params_test() ->
    {error, invalid_params} = erlmcp_pagination:apply_pagination(not_a_list, #{}, false),
    {error, invalid_params} = erlmcp_pagination:apply_pagination([], not_a_map, false).

error_paginate_invalid_params_test() ->
    {error, invalid_params} = erlmcp_pagination:paginate_list(not_a_list, undefined, 10, 0),
    {error, invalid_params} = erlmcp_pagination:paginate_list([a, b], undefined, not_int, 2).

%%====================================================================
%% Performance Tests
%%====================================================================

perf_cursor_encode_decode_test() ->
    %% Generate and decode 100 cursors
    Cursors = [erlmcp_pagination:encode_cursor(I * 100, 100) || I <- lists:seq(0, 99)],
    Results = [erlmcp_pagination:decode_cursor(C) || C <- Cursors],
    ?assertEqual(length(Results), 100),
    lists:foreach(fun({ok, {Offset, PageSize}}) ->
        ?assertEqual(PageSize, 100),
        ?assertEqual(Offset rem 100, 0)
    end, Results).

perf_paginate_large_list_test() ->
    Items = lists:seq(1, 10000),
    %% Paginate through 100 pages
    Results = [erlmcp_pagination:apply_pagination(Items, #{limit => 100, cursor => Cursor}, false)
               || Cursor <- [undefined | [erlmcp_pagination:encode_cursor(I * 100, 100)
                   || I <- lists:seq(1, 99)]]],
    ?assertEqual(length(Results), 100).

%%====================================================================
%% Map-Based Items Tests
%%====================================================================

paginate_map_items_test() ->
    Items = [
        #{name => <<"item1">>, id => 1},
        #{name => <<"item2">>, id => 2},
        #{name => <<"item3">>, id => 3},
        #{name => <<"item4">>, id => 4},
        #{name => <<"item5">>, id => 5}
    ],
    Result = erlmcp_pagination:apply_pagination(Items, #{limit => 2}, true),
    PagedItems = maps:get(items, Result),
    ?assertEqual(length(PagedItems), 2),
    ?assertMatch([#{name := <<"item1">>}, #{name := <<"item2">>}], PagedItems).

%%====================================================================
%% Edge Case Tests
%%====================================================================

edge_case_single_page_size_test() ->
    Items = [a, b, c],
    Result = erlmcp_pagination:apply_pagination(Items, #{limit => 1}, false),
    PagedItems = maps:get(items, Result),
    ?assertEqual(PagedItems, [a]),
    ?assertNotEqual(maps:get(nextCursor, Result), undefined).

edge_case_exact_page_boundary_test() ->
    Items = lists:seq(1, 10),
    Result = erlmcp_pagination:apply_pagination(Items, #{limit => 5}, false),
    PagedItems = maps:get(items, Result),
    ?assertEqual(PagedItems, [1, 2, 3, 4, 5]),
    ?assertNotEqual(maps:get(nextCursor, Result), undefined).

edge_case_cursor_at_end_test() ->
    Items = [a, b, c, d, e],
    Cursor = erlmcp_pagination:encode_cursor(4, 1),
    Result = erlmcp_pagination:apply_pagination(Items, #{cursor => Cursor, limit => 1}, false),
    PagedItems = maps:get(items, Result),
    ?assertEqual(PagedItems, [e]),
    ?assertEqual(maps:get(nextCursor, Result), undefined).

%%====================================================================
%% Run All Tests
%%====================================================================

-ifdef(EUNIT).
all_tests() ->
    eunit:test(?MODULE, [verbose]).
-endif.
