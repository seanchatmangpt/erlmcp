#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc
%%% Manual test script for pagination module.
%%% Tests basic functionality without requiring full rebar3 setup.
%%% @end
%%%-------------------------------------------------------------------

main(_) ->
    code:add_patha("apps/erlmcp_core/src"),
    code:add_patha("ebin"),

    io:format("Testing Pagination Module...~n~n"),

    %% Test 1: Basic pagination
    io:format("Test 1: Basic pagination with first page~n"),
    Items = generate_items(100),
    PageSize = 10,
    {PageItems, PageInfo} = erlmcp_pagination:paginate(Items, PageSize, null, 100),
    io:format("  Page items: ~p~n", [length(PageItems)]),
    io:format("  Has more: ~p~n", [maps:get(<<"hasMore">>, PageInfo)]),
    io:format("  Total: ~p~n", [maps:get(<<"total">>, PageInfo)]),
    case length(PageItems) =:= 10 andalso maps:get(<<"hasMore">>, PageInfo) =:= true of
        true -> io:format("  ✓ PASS~n~n");
        false -> io:format("  ✗ FAIL~n~n")
    end,

    %% Test 2: Cursor encoding/decoding
    io:format("Test 2: Cursor encoding/decoding~n"),
    Cursor = #cursor{offset = 50, timestamp = 1234567890},
    Encoded = erlmcp_pagination:encode_cursor(Cursor),
    Decoded = erlmcp_pagination:decode_cursor(Encoded),
    io:format("  Original offset: ~p~n", [Cursor#cursor.offset]),
    io:format("  Decoded offset: ~p~n", [Decoded#cursor.offset]),
    case Cursor#cursor.offset =:= Decoded#cursor.offset of
        true -> io:format("  ✓ PASS~n~n");
        false -> io:format("  ✗ FAIL~n~n")
    end,

    %% Test 3: Multi-page navigation
    io:format("Test 3: Multi-page navigation~n"),
    {Page1, Info1} = erlmcp_pagination:paginate(Items, PageSize, null, 100),
    Cursor1 = maps:get(<<"cursor">>, Info1),
    {Page2, Info2} = erlmcp_pagination:paginate(Items, PageSize, Cursor1, 100),
    Cursor2 = maps:get(<<"cursor">>, Info2),
    {Page3, _Info3} = erlmcp_pagination:paginate(Items, PageSize, Cursor2, 100),
    io:format("  Page 1 items: ~p~n", [length(Page1)]),
    io:format("  Page 2 items: ~p~n", [length(Page2)]),
    io:format("  Page 3 items: ~p~n", [length(Page3)]),
    case length(Page1) + length(Page2) + length(Page3) =:= 30 of
        true -> io:format("  ✓ PASS~n~n");
        false -> io:format("  ✗ FAIL~n~n")
    end,

    %% Test 4: Last page detection
    io:format("Test 4: Last page detection~n"),
    SmallItems = generate_items(25),
    {Page1Small, Info1Small} = erlmcp_pagination:paginate(SmallItems, PageSize, null, 25),
    Cursor1Small = maps:get(<<"cursor">>, Info1Small),
    {Page2Small, Info2Small} = erlmcp_pagination:paginate(SmallItems, PageSize, Cursor1Small, 25),
    Cursor2Small = maps:get(<<"cursor">>, Info2Small),
    {Page3Small, Info3Small} = erlmcp_pagination:paginate(SmallItems, PageSize, Cursor2Small, 25),
    io:format("  Page 3 items: ~p~n", [length(Page3Small)]),
    io:format("  Has more on last page: ~p~n", [maps:get(<<"hasMore">>, Info3Small)]),
    case length(Page3Small) =:= 5 andalso maps:get(<<"hasMore">>, Info3Small) =:= false of
        true -> io:format("  ✓ PASS~n~n");
        false -> io:format("  ✗ FAIL~n~n")
    end,

    %% Test 5: Page size bounding
    io:format("Test 5: Page size bounding~n"),
    LargeItems = generate_items(10000),
    LargePageSize = 5000,
    {BoundedPage, _BoundedInfo} = erlmcp_pagination:paginate(LargeItems, LargePageSize, null, 10000),
    io:format("  Requested page size: ~p~n", [LargePageSize]),
    io:format("  Actual page size: ~p~n", [length(BoundedPage)]),
    case length(BoundedPage) =:= 1000 of  % Default max is 1000
        true -> io:format("  ✓ PASS (bounded to max)~n~n");
        false -> io:format("  ✗ FAIL~n~n")
    end,

    io:format("All tests completed!~n").

%% Helper to generate test items
generate_items(Count) ->
    [#{<<"id">> => integer_to_binary(I),
       <<"name">> => <<"Item ", (integer_to_binary(I))/binary>>}
     || I <- lists:seq(1, Count)].

%% Include cursor record definition
-include("apps/erlmcp_core/src/erlmcp_pagination.erl").
