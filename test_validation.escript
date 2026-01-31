#!/usr/bin/env escript
main(_) ->
    %% Test 1MB message
    LargeData = << <<X>> || <<X>> <= <<0:8000000>> >>,
    MaxSize = 16 * 1024 * 1024,
    
    %% This should be ok
    Result1 = case byte_size(LargeData) =< MaxSize of
        true -> ok;
        false -> {error, too_large}
    end,
    io:format("1MB message test: ~p (size: ~p)~n", [Result1, byte_size(LargeData)]),
    
    %% Test 20MB message  
    HugeMessage = << <<X>> || <<X>> <= <<0:20000000>> >>,
    Result2 = case byte_size(HugeMessage) =< MaxSize of
        true -> ok;
        false -> {error, too_large}
    end,
    io:format("20MB message test: ~p (size: ~p)~n", [Result2, byte_size(HugeMessage)]),
    
    ok.
