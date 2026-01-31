%% Run the supervisor collapse test
code:add_patha("_build/default/lib/erlmcp_core/ebin").
code:add_patha("_build/default/lib/erlmcp_transports/ebin").
code:add_patha("_build/default/lib/erlmcp_observability/ebin").
code:add_patha("_build/default/lib/gproc/ebin").
code:add_patha("_build/default/lib/jsx/ebin").
code:add_patha("_build/default/lib/jesse/ebin").
code:add_patha("_build/default/lib/ranch/ebin").
code:add_patha("_build/default/lib/gun/ebin").
code:add_patha("_build/default/lib/poolboy/ebin").
code:add_patha("_build/default/lib/cowboy/ebin").
code:add_patha("_build/default/lib/opentelemetry_api/ebin").
code:add_patha("_build/default/lib/opentelemetry/ebin").
code:add_patha("_build/default/lib/opentelemetry_exporter/ebin").
code:add_patha("_build/default/lib/jobs/ebin").
code:add_patha("_build/default/lib/fs/ebin").
code:add_patha("_build/default/lib/bbmustache/ebin").
code:add_patha("bench/").

%% Start the application
io:format("Starting erlmcp_core application...~n"),
application:ensure_all_started(erlmcp_core),

%% Wait for startup
timer:sleep(2000),

%% Run the collapse test
io:format("~nStarting SUPERVISOR TREE COLLAPSE TEST...~n"),
case erlmcp_bench_supervisor_collapse:run("supervisor_collapse_test") of
    {ok, Status} ->
        io:format("~nTest completed successfully~n"),
        io:format("Final status: ~p~n", [Status]);
    {error, Reason} ->
        io:format("~nTest failed: ~p~n", [Reason])
end,

%% Wait before exit
timer:sleep(2000),
halt().
