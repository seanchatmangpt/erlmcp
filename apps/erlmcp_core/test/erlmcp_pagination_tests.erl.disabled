%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for erlmcp_pagination module.
%%% Tests cursor-based pagination functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pagination_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%% Define cursor record for testing (matching pagination module)
-record(cursor, {
    offset = 0 :: non_neg_integer(),
    timestamp :: integer()
}).

%%%===================================================================
%%% Test Data Generators
%%%===================================================================

%% Generate a list of items for testing
generate_items(Count) ->
    [#{<<"id">> => integer_to_binary(I), <<"name">> => <<"Item ", (integer_to_binary(I))/binary>>}
     || I <- lists:seq(1, Count)].

%%%===================================================================
%%% Basic Pagination Tests
%%%===================================================================

%% Test pagination with first page (null cursor)
paginate_first_page_test() ->
    Items = generate_items(100),
    PageSize = 10,

    {PageItems, PageInfo} = erlmcp_pagination:paginate(Items, PageSize, null, 100),

    ?assertEqual(10, length(PageItems)),
    ?assertNotEqual(null, maps:get(<<"cursor">>, PageInfo)),
    ?assertEqual(true, maps:get(<<"hasMore">>, PageInfo)),
    ?assertEqual(100, maps:get(<<"total">>, PageInfo)),

    %% Verify items are from 1 to 10
    FirstItem = lists:nth(1, PageItems),
    LastItem = lists:nth(10, PageItems),
    ?assertEqual(<<"1">>, maps:get(<<"id">>, FirstItem)),
    ?assertEqual(<<"10">>, maps:get(<<"id">>, LastItem)).

%% Test pagination with last page
paginate_last_page_test() ->
    Items = generate_items(25),
    PageSize = 10,

    %% Get first page
    {Page1, PageInfo1} = erlmcp_pagination:paginate(Items, PageSize, null, 25),
    Cursor1 = maps:get(<<"cursor">>, PageInfo1),

    %% Get second page
    {Page2, PageInfo2} = erlmcp_pagination:paginate(Items, PageSize, Cursor1, 25),
    Cursor2 = maps:get(<<"cursor">>, PageInfo2),

    %% Get third page (last)
    {Page3, PageInfo3} = erlmcp_pagination:paginate(Items, PageSize, Cursor2, 25),

    ?assertEqual(5, length(Page3)),
    ?assertEqual(null, maps:get(<<"cursor">>, PageInfo3)),
    ?assertEqual(false, maps:get(<<"hasMore">>, PageInfo3)),
    ?assertEqual(25, maps:get(<<"total">>, PageInfo3)).

%% Test pagination with empty list
paginate_empty_list_test() ->
    Items = [],
    PageSize = 10,

    {PageItems, PageInfo} = erlmcp_pagination:paginate(Items, PageSize, null, 0),

    ?assertEqual(0, length(PageItems)),
    ?assertEqual(null, maps:get(<<"cursor">>, PageInfo)),
    ?assertEqual(false, maps:get(<<"hasMore">>, PageInfo)),
    ?assertEqual(0, maps:get(<<"total">>, PageInfo)).

%% Test pagination with page size equal to total count
paginate_exact_page_size_test() ->
    Items = generate_items(50),
    PageSize = 50,

    {PageItems, PageInfo} = erlmcp_pagination:paginate(Items, PageSize, null, 50),

    ?assertEqual(50, length(PageItems)),
    ?assertEqual(null, maps:get(<<"cursor">>, PageInfo)),
    ?assertEqual(false, maps:get(<<"hasMore">>, PageInfo)).

%% Test pagination with page size larger than total count
paginate_oversized_page_test() ->
    Items = generate_items(10),
    PageSize = 100,

    {PageItems, PageInfo} = erlmcp_pagination:paginate(Items, PageSize, null, 10),

    %% Should return all items (bounded by actual count)
    ?assertEqual(10, length(PageItems)),
    ?assertEqual(null, maps:get(<<"cursor">>, PageInfo)),
    ?assertEqual(false, maps:get(<<"hasMore">>, PageInfo)).

%%%===================================================================
%%% Cursor Encoding/Decoding Tests
%%%===================================================================

%% Test cursor encoding and decoding
encode_decode_cursor_test() ->
    Cursor = #cursor{offset = 100, timestamp = 1234567890},
    Encoded = erlmcp_pagination:encode_cursor(Cursor),
    Decoded = erlmcp_pagination:decode_cursor(Encoded),

    ?assertEqual(Cursor#cursor.offset, Decoded#cursor.offset),
    ?assertEqual(Cursor#cursor.timestamp, Decoded#cursor.timestamp).

%% Test decoding null cursor
decode_null_cursor_test() ->
    Cursor = erlmcp_pagination:decode_cursor(null),

    ?assertEqual(0, Cursor#cursor.offset),
    ?assertEqual(0, Cursor#cursor.timestamp).

%% Test decoding invalid cursor returns default
decode_invalid_cursor_test() ->
    InvalidCursor = <<"invalid_base64_string">>,
    Cursor = erlmcp_pagination:decode_cursor(InvalidCursor),

    %% Should return default cursor on error
    ?assertEqual(0, Cursor#cursor.offset),
    ?assertEqual(0, Cursor#cursor.timestamp).

%% Test cursor encoding produces valid base64
cursor_encoding_produces_valid_base64_test() ->
    Cursor = #cursor{offset = 50, timestamp = erlang:system_time(millisecond)},
    Encoded = erlmcp_pagination:encode_cursor(Cursor),

    %% Should be valid base64 (alphanumeric + +/ = padding)
    ?assertMatch(<<_, _/binary>>, Encoded),
    ?assert(is_binary(Encoded)).

%%%===================================================================
%%% Page Size Validation Tests
%%%===================================================================

%% Test page size validation with valid size
validate_page_size_valid_test() ->
    PageSize = 50,
    ?assertEqual(true, erlmcp_pagination:validate_page_size(PageSize)).

%% Test page size validation with zero
validate_page_size_zero_test() ->
    ?assertEqual(false, erlmcp_pagination:validate_page_size(0)).

%% Test page size validation with negative
validate_page_size_negative_test() ->
    ?assertEqual(false, erlmcp_pagination:validate_page_size(-1)).

%% Test page size validation with non-integer
validate_page_size_invalid_type_test() ->
    ?assertEqual(false, erlmcp_pagination:validate_page_size(<<"50">>)),
    ?assertEqual(false, erlmcp_pagination:validate_page_size("50")),
    ?assertEqual(false, erlmcp_pagination:validate_page_size(nil)).

%% Test page size bounded by max
page_size_bounded_by_max_test() ->
    Items = generate_items(10000),
    PageSize = 5000,  %% Exceeds default max of 1000

    {PageItems, _PageInfo} = erlmcp_pagination:paginate(Items, PageSize, null, 10000),

    %% Should be bounded to max page size (1000)
    ?assertEqual(1000, length(PageItems)).

%% Test page size uses default when undefined
page_size_uses_default_when_undefined_test() ->
    Items = generate_items(100),

    {PageItems, _PageInfo} = erlmcp_pagination:paginate(Items, undefined, null, 100),

    %% Should use default page size (50)
    ?assertEqual(50, length(PageItems)).

%%%===================================================================
%%% Has More Calculation Tests
%%%===================================================================

%% Test has_more calculation with more pages available
calculate_has_more_true_test() ->
    HasMore = erlmcp_pagination:calculate_has_more(0, 10, 100),
    ?assertEqual(true, HasMore).

%% Test has_more calculation on last page
calculate_has_more_false_test() ->
    HasMore = erlmcp_pagination:calculate_has_more(90, 10, 100),
    ?assertEqual(false, HasMore).

%% Test has_more calculation with undefined total
calculate_has_more_undefined_test() ->
    HasMore = erlmcp_pagination:calculate_has_more(50, 10, undefined),
    ?assertEqual(undefined, HasMore).

%% Test has_more calculation at boundary
calculate_has_more_boundary_test() ->
    %% Exactly at the end
    HasMore1 = erlmcp_pagination:calculate_has_more(100, 10, 100),
    ?assertEqual(false, HasMore1),

    %% One past the end
    HasMore2 = erlmcp_pagination:calculate_has_more(91, 10, 100),
    ?assertEqual(false, HasMore2),

    %% One before the end
    HasMore3 = erlmcp_pagination:calculate_has_more(89, 10, 100),
    ?assertEqual(true, HasMore3).

%%%===================================================================
%%% Multi-Page Navigation Tests
%%%===================================================================

%% Test navigating through multiple pages
navigate_multiple_pages_test() ->
    Items = generate_items(100),
    PageSize = 15,

    %% First page
    {Page1, Info1} = erlmcp_pagination:paginate(Items, PageSize, null, 100),
    ?assertEqual(15, length(Page1)),
    ?assertEqual(true, maps:get(<<"hasMore">>, Info1)),

    %% Second page
    Cursor1 = maps:get(<<"cursor">>, Info1),
    {Page2, Info2} = erlmcp_pagination:paginate(Items, PageSize, Cursor1, 100),
    ?assertEqual(15, length(Page2)),
    ?assertEqual(true, maps:get(<<"hasMore">>, Info2)),

    %% Third page
    Cursor2 = maps:get(<<"cursor">>, Info2),
    {Page3, Info3} = erlmcp_pagination:paginate(Items, PageSize, Cursor2, 100),
    ?assertEqual(15, length(Page3)),
    ?assertEqual(true, maps:get(<<"hasMore">>, Info3)),

    %% Continue through pages
    Cursor3 = maps:get(<<"cursor">>, Info3),
    {Page4, Info4} = erlmcp_pagination:paginate(Items, PageSize, Cursor3, 100),
    Cursor4 = maps:get(<<"cursor">>, Info4),
    {Page5, Info5} = erlmcp_pagination:paginate(Items, PageSize, Cursor4, 100),
    Cursor5 = maps:get(<<"cursor">>, Info5),
    {Page6, Info6} = erlmcp_pagination:paginate(Items, PageSize, Cursor5, 100),
    Cursor6 = maps:get(<<"cursor">>, Info6),
    {Page7, Info7} = erlmcp_pagination:paginate(Items, PageSize, Cursor6, 100),

    %% Last page (10 items)
    ?assertEqual(10, length(Page7)),
    ?assertEqual(false, maps:get(<<"hasMore">>, Info7)),

    %% Verify no duplicates
    AllPages = Page1 ++ Page2 ++ Page3 ++ Page4 ++ Page5 ++ Page6 ++ Page7,
    UniqueIds = lists:usort([maps:get(<<"id">>, Item) || Item <- AllPages]),
    ?assertEqual(100, length(UniqueIds)).

%% Test pagination with undefined total count
paginate_undefined_total_test() ->
    Items = generate_items(50),
    PageSize = 10,

    {PageItems, PageInfo} = erlmcp_pagination:paginate(Items, PageSize, null, undefined),

    ?assertEqual(10, length(PageItems)),
    ?assertEqual(true, maps:get(<<"hasMore">>, PageInfo)),
    ?assertEqual(undefined, maps:get(<<"total">>, PageInfo)).

%%%===================================================================
%%% Edge Cases Tests
%%%===================================================================

%% Test pagination with single item
paginate_single_item_test() ->
    Items = generate_items(1),
    PageSize = 10,

    {PageItems, PageInfo} = erlmcp_pagination:paginate(Items, PageSize, null, 1),

    ?assertEqual(1, length(PageItems)),
    ?assertEqual(null, maps:get(<<"cursor">>, PageInfo)),
    ?assertEqual(false, maps:get(<<"hasMore">>, PageInfo)).

%% Test pagination with page size of 1
paginate_page_size_one_test() ->
    Items = generate_items(5),
    PageSize = 1,

    {Page1, Info1} = erlmcp_pagination:paginate(Items, PageSize, null, 5),
    ?assertEqual(1, length(Page1)),
    ?assertEqual(true, maps:get(<<"hasMore">>, Info1)),

    Cursor1 = maps:get(<<"cursor">>, Info1),
    {Page2, Info2} = erlmcp_pagination:paginate(Items, PageSize, Cursor1, 5),
    ?assertEqual(1, length(Page2)),
    ?assertEqual(true, maps:get(<<"hasMore">>, Info2)),

    Cursor2 = maps:get(<<"cursor">>, Info2),
    {Page3, Info3} = erlmcp_pagination:paginate(Items, PageSize, Cursor2, 5),
    ?assertEqual(1, length(Page3)),
    ?assertEqual(true, maps:get(<<"hasMore">>, Info3)),

    Cursor3 = maps:get(<<"cursor">>, Info3),
    {Page4, Info4} = erlmcp_pagination:paginate(Items, PageSize, Cursor3, 5),
    ?assertEqual(1, length(Page4)),
    ?assertEqual(true, maps:get(<<"hasMore">>, Info4)),

    Cursor4 = maps:get(<<"cursor">>, Info4),
    {Page5, Info5} = erlmcp_pagination:paginate(Items, PageSize, Cursor4, 5),
    ?assertEqual(1, length(Page5)),
    ?assertEqual(false, maps:get(<<"hasMore">>, Info5)).

%% Test pagination preserves order
paginate_preserves_order_test() ->
    Items = [#{<<"id">> => I} || I <- lists:seq(1, 20)],
    PageSize = 5,

    {Page1, Info1} = erlmcp_pagination:paginate(Items, PageSize, null, 20),
    Cursor1 = maps:get(<<"cursor">>, Info1),
    {Page2, Info2} = erlmcp_pagination:paginate(Items, PageSize, Cursor1, 20),
    Cursor2 = maps:get(<<"cursor">>, Info2),
    {Page3, Info3} = erlmcp_pagination:paginate(Items, PageSize, Cursor2, 20),
    Cursor3 = maps:get(<<"cursor">>, Info3),
    {Page4, _Info4} = erlmcp_pagination:paginate(Items, PageSize, Cursor3, 20),

    %% Verify order is preserved
    AllPages = Page1 ++ Page2 ++ Page3 ++ Page4,
    ExpectedIds = [I || I <- lists:seq(1, 20)],
    ActualIds = [maps:get(<<"id">>, Item) || Item <- AllPages],
    ?assertEqual(ExpectedIds, ActualIds).

%% Test cursor timestamp changes
cursor_timestamp_changes_test() ->
    Items = generate_items(20),
    PageSize = 5,

    {Page1, Info1} = erlmcp_pagination:paginate(Items, PageSize, null, 20),
    Cursor1 = maps:get(<<"cursor">>, Info1),

    %% Wait a bit to ensure timestamp difference
    timer:sleep(10),

    {Page2, Info2} = erlmcp_pagination:paginate(Items, PageSize, Cursor1, 20),
    Cursor2 = maps:get(<<"cursor">>, Info2),

    %% Decode cursors and verify timestamps are different
    Decoded1 = erlmcp_pagination:decode_cursor(Cursor1),
    Decoded2 = erlmcp_pagination:decode_cursor(Cursor2),

    ?assert(Decoded2#cursor.timestamp >= Decoded1#cursor.timestamp).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

%% Test pagination mimicking tools/list endpoint
tools_list_pagination_test() ->
    %% Simulate tools list
    Tools = [#{<<"name">> => <<"tool_", (integer_to_binary(I))/binary>>}
             || I <- lists:seq(1, 75)],
    PageSize = 20,

    %% First request
    {Page1, Info1} = erlmcp_pagination:paginate(Tools, PageSize, null, 75),

    Response1 = #{
        <<"tools">> => Page1,
        <<"nextCursor">> => maps:get(<<"cursor">>, Info1),
        <<"hasMore">> => maps:get(<<"hasMore">>, Info1),
        <<"total">> => maps:get(<<"total">>, Info1)
    },

    ?assertEqual(20, length(maps:get(<<"tools">>, Response1))),
    ?assertEqual(true, maps:get(<<"hasMore">>, Response1)),

    %% Second request with cursor
    Cursor1 = maps:get(<<"nextCursor">>, Response1),
    {Page2, Info2} = erlmcp_pagination:paginate(Tools, PageSize, Cursor1, 75),

    Response2 = #{
        <<"tools">> => Page2,
        <<"nextCursor">> => maps:get(<<"cursor">>, Info2),
        <<"hasMore">> => maps:get(<<"hasMore">>, Info2),
        <<"total">> => maps:get(<<"total">>, Info2)
    },

    ?assertEqual(20, length(maps:get(<<"tools">>, Response2))),
    ?assertEqual(true, maps:get(<<"hasMore">>, Response2)).

%% Test pagination mimicking resources/list endpoint
resources_list_pagination_test() ->
    %% Simulate resources list
    Resources = [#{<<"uri">> => <<"resource://", (integer_to_binary(I))/binary>>}
                 || I <- lists:seq(1, 150)],
    PageSize = 50,

    %% First page
    {Page1, Info1} = erlmcp_pagination:paginate(Resources, PageSize, null, 150),

    ?assertEqual(50, length(Page1)),
    ?assertEqual(true, maps:get(<<"hasMore">>, Info1)),
    ?assertEqual(150, maps:get(<<"total">>, Info1)).

%% Test pagination mimicking prompts/list endpoint
prompts_list_pagination_test() ->
    %% Simulate prompts list
    Prompts = [#{<<"name">> => <<"prompt_", (integer_to_binary(I))/binary>>}
               || I <- lists:seq(1, 30)],
    PageSize = 10,

    %% Navigate through all pages
    AllPages = paginate_all(Prompts, PageSize, 30),

    TotalItems = lists:foldl(fun(Page, Acc) ->
        Acc + length(Page)
    end, 0, AllPages),

    ?assertEqual(30, TotalItems).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Helper to paginate through all pages
paginate_all(Items, PageSize, Total) ->
    paginate_all(Items, PageSize, null, Total, []).

paginate_all(_Items, _PageSize, null, _Total, Acc) ->
    lists:reverse(Acc);
paginate_all(Items, PageSize, Cursor, Total, Acc) ->
    {Page, Info} = erlmcp_pagination:paginate(Items, PageSize, Cursor, Total),
    NextCursor = maps:get(<<"cursor">>, Info),
    paginate_all(Items, PageSize, NextCursor, Total, [Page | Acc]).
