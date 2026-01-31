#!/usr/bin/env escript
main(_) ->
    io:format("Debugging audit log range verification~n"),

    % Simulate what read_range_entries does
    Lines = [
        <<"{\"sequence\":1,\"timestamp\":123,\"event_type\":\"auth_success\",\"user_id\":\"user_1\",\"session_id\":null,\"resource\":null,\"action\":null,\"result\":\"success\",\"metadata\":{},\"previous_hash\":\"abc\",\"entry_hash\":\"def\"}">>,
        <<"{\"sequence\":2,\"timestamp\":124,\"event_type\":\"auth_success\",\"user_id\":\"user_2\",\"session_id\":null,\"resource\":null,\"action\":null,\"result\":\"success\",\"metadata\":{},\"previous_hash\":\"def\",\"entry_hash\":\"ghi\"}">>,
        <<"{\"sequence\":3,\"timestamp\":125,\"event_type\":\"auth_success\",\"user_id\":\"user_3\",\"session_id\":null,\"resource\":null,\"action\":null,\"result\":\"success\",\"metadata\":{},\"previous_hash\":\"ghi\",\"entry_hash\":\"jkl\"}">>,
        <<"{\"sequence\":4,\"timestamp\":126,\"event_type\":\"auth_success\",\"user_id\":\"user_4\",\"session_id\":null,\"resource\":null,\"action\":null,\"result\":\"success\",\"metadata\":{},\"previous_hash\":\"jkl\",\"entry_hash\":\"mno\"}">>,
        <<"{\"sequence\":5,\"timestamp\":127,\"event_type\":\"auth_success\",\"user_id\":\"user_5\",\"session_id\":null,\"resource\":null,\"action\":null,\"result\":\"success\",\"metadata\":{},\"previous_hash\":\"mno\",\"entry_hash\":\"pqr\"}">>
    ],

    FromSeq = 0,
    ToSeq = 2,

    AllEntries = lists:map(fun(Line) ->
        jsx:decode(Line, [return_maps])
    end, Lines),

    io:format("Total entries: ~p~n", [length(AllEntries)]),
    io:format("Entries: ~p~n", [[maps:get(<<"sequence">>, E) || E <- AllEntries]]),

    % Filter entries in range
    RangeEntries = lists:filter(fun(Entry) ->
        Seq = maps:get(<<"sequence">>, Entry),
        Seq >= FromSeq andalso Seq =< ToSeq
    end, AllEntries),

    io:format("Range entries [~p, ~p]: ~p~n", [FromSeq, ToSeq, [maps:get(<<"sequence">>, E) || E <- RangeEntries]]),
    io:format("Range count: ~p~n", [length(RangeEntries)]),

    % Check we have all expected entries
    ExpectedCount = ToSeq - FromSeq + 1,
    ActualCount = length(RangeEntries),

    io:format("Expected count: ~p~n", [ExpectedCount]),
    io:format("Actual count: ~p~n", [ActualCount]),
    io:format("Difference: ~p~n", [ActualCount - ExpectedCount]),

    case ActualCount of
        ExpectedCount ->
            io:format("Result: ok~n");
        _ when ActualCount < ExpectedCount ->
            io:format("Result: {error, {missing_entries, ~p, ~p, ~p}}~n", [FromSeq, ToSeq, ExpectedCount - ActualCount]);
        _ when ActualCount > ExpectedCount ->
            io:format("Result: {error, {duplicate_entries, ~p, ~p, ~p}}~n", [FromSeq, ToSeq, ActualCount - ExpectedCount])
    end.
