-module(debug_audit_log).

-include_lib("eunit/include/eunit.hrl").

debug_test() ->
    %% Create test entries
    Entries =
        [#{<<"sequence">> => 0},
         #{<<"sequence">> => 1},
         #{<<"sequence">> => 2},
         #{<<"sequence">> => 3},
         #{<<"sequence">> => 4}],

    %% Test filtering
    FromSeq = 0,
    ToSeq = 2,

    RangeEntries =
        lists:filter(fun(Entry) ->
                        Seq = maps:get(<<"sequence">>, Entry),
                        Seq >= FromSeq andalso Seq =< ToSeq
                     end,
                     Entries),

    io:format("All entries: ~p~n", [[maps:get(<<"sequence">>, E) || E <- Entries]]),
    io:format("Range entries [~p, ~p]: ~p~n",
              [FromSeq, ToSeq, [maps:get(<<"sequence">>, E) || E <- RangeEntries]]),
    io:format("Expected count: ~p~n", [ToSeq - FromSeq + 1]),
    io:format("Actual count: ~p~n", [length(RangeEntries)]),

    ?assertEqual(3, length(RangeEntries)),
    ok.
