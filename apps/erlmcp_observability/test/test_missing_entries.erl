-module(test_missing_entries).
-include_lib("eunit/include/eunit.hrl").

debug_missing_entries_test() ->
    % Create temp log
    LogDir = "/tmp/test_missing_" ++ integer_to_list(erlang:unique_integer([positive])),
    LogPath = LogDir ++ "/audit.log",

    Config = #{
        log_path => LogPath,
        buffer_size => 5,
        flush_interval_ms => 100
    },

    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log 2 events
    ok = erlmcp_audit_log:log_auth_success(<<"user1">>, #{}),
    ok = erlmcp_audit_log:log_auth_success(<<"user2">>, #{}),

    % Wait for flush
    timer:sleep(200),

    % Read the file
    {ok, Content} = file:read_file(LogPath),
    Lines = binary:split(Content, <<"\n">>, [global, trim]),

    io:format("Number of lines in file: ~p~n", [length(Lines)]),
    io:format("Lines: ~p~n", [Lines]),

    % Try to read range [5, 10]
    case erlmcp_audit_log:read_range_entries(Lines, 5, 10) of
        {ok, Entries} ->
            io:format("read_range_entries returned OK with ~p entries~n", [length(Entries)]),
            io:format("Entries: ~p~n", [[maps:get(<<"sequence">>, E) || E <- Entries]]);
        {error, Reason} ->
            io:format("read_range_entries returned error: ~p~n", [Reason])
    end,

    % Now verify the range
    VerifyResult = erlmcp_audit_log:verify_chain(5, 10),
    io:format("Verify result: ~p~n", [VerifyResult]),

    % Cleanup
    erlmcp_audit_log:stop(),
    os:cmd("rm -rf " ++ LogDir),

    ?assertMatch({error, {missing_entries, 5, 10, _}}, VerifyResult).
