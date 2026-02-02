%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_binary_join - OTP 28 binary:join/2 Benchmarks
%%%
%%% Compares binary:join/2 (OTP 28) vs iolist_to_binary/1 (traditional).
%%%
%%% Benchmarks cover:
%%% - SSE chunk assembly (10K chunks)
%%% - JSON batch arrays (10K elements)
%%% - HTTP header formatting (100 headers)
%%% - Line joining (10K lines)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_binary_join).

-behaviour(gen_server).

%% API
-export([start_link/0,
         run_benchmark/1,
         run_all_benchmarks/0,
         get_results/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% State
-record(state, {
    results = #{} :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Run specific benchmark
-spec run_benchmark(atom()) -> {ok, map()}.
run_benchmark(BenchmarkName) ->
    gen_server:call(?SERVER, {run_benchmark, BenchmarkName}, 60000).

%% @doc Run all benchmarks
-spec run_all_benchmarks() -> {ok, map()}.
run_all_benchmarks() ->
    gen_server:call(?SERVER, run_all_benchmarks, 300000).

%% @doc Get benchmark results
-spec get_results() -> {ok, map()}.
get_results() ->
    gen_server:call(?SERVER, get_results).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}}.

handle_call({run_benchmark, BenchmarkName}, _From, State) ->
    Result = execute_benchmark(BenchmarkName),
    NewResults = maps:put(BenchmarkName, Result, State#state.results),
    {reply, {ok, Result}, State#state{results = NewResults}};

handle_call(run_all_benchmarks, _From, State) ->
    Benchmarks = [
        sse_chunks,
        json_array,
        http_headers,
        line_joining,
        trace_context,
        metrics
    ],
    Results = lists:foldl(fun(Name, Acc) ->
        Result = execute_benchmark(Name),
        maps:put(Name, Result, Acc)
    end, #{}, Benchmarks),
    {reply, {ok, Results}, State#state{results = Results}};

handle_call(get_results, _From, State) ->
    {reply, {ok, State#state.results}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Benchmark Executors
%%====================================================================

%% @private Execute benchmark by name
execute_benchmark(sse_chunks) ->
    benchmark_sse_chunks();
execute_benchmark(json_array) ->
    benchmark_json_array();
execute_benchmark(http_headers) ->
    benchmark_http_headers();
execute_benchmark(line_joining) ->
    benchmark_line_joining();
execute_benchmark(trace_context) ->
    benchmark_trace_context();
execute_benchmark(metrics) ->
    benchmark_metrics().

%% @doc Benchmark SSE chunk assembly (10K chunks)
%% @private
benchmark_sse_chunks() ->
    %% Generate test data: 10K SSE chunks
    Chunks = lists:duplicate(10000, <<"event: message\ndata: test">>),

    %% Warm-up
    lists:foreach(fun(_) ->
        benchmark_join_iolist(Chunks)
    end, lists:seq(1, 100)),

    %% Benchmark: iolist_to_binary (traditional)
    {IolistTime, _IolistResult} = timer:tc(fun() ->
        benchmark_join_iolist(Chunks)
    end, 100),

    %% Benchmark: binary:join (OTP 28)
    {BinaryJoinTime, _BinaryJoinResult} = timer:tc(fun() ->
        binary:join(<<"\n">>, Chunks)
    end, 100),

    %% Calculate improvement
    Improvement = ((IolistTime - BinaryJoinTime) / IolistTime) * 100,

    #{
        benchmark => sse_chunks,
        iterations => 100,
        chunk_count => 10000,
        iolist_time_us => IolistTime,
        binary_join_time_us => BinaryJoinTime,
        improvement_percent => Improvement,
        winner => case Improvement > 0 of true -> binary_join; false -> iolist end
    }.

%% @doc Benchmark JSON array assembly (10K elements)
%% @private
benchmark_json_array() ->
    %% Generate test data: 10K JSON objects
    Items = lists:duplicate(10000, <<"{\"id\": 1, \"result\": \"ok\"}">>),

    %% Warm-up
    lists:foreach(fun(_) ->
        benchmark_json_iolist(Items)
    end, lists:seq(1, 100)),

    %% Benchmark: iolist_to_binary (traditional)
    {IolistTime, _IolistResult} = timer:tc(fun() ->
        benchmark_json_iolist(Items)
    end, 100),

    %% Benchmark: binary:join (OTP 28)
    {BinaryJoinTime, _BinaryJoinResult} = timer:tc(fun() ->
        Inner = binary:join(<<",">>, Items),
        <<"[", Inner/binary, "]">>
    end, 100),

    %% Calculate improvement
    Improvement = ((IolistTime - BinaryJoinTime) / IolistTime) * 100,

    #{
        benchmark => json_array,
        iterations => 100,
        item_count => 10000,
        iolist_time_us => IolistTime,
        binary_join_time_us => BinaryJoinTime,
        improvement_percent => Improvement,
        winner => case Improvement > 0 of true -> binary_join; false -> iolist end
    }.

%% @doc Benchmark HTTP header formatting (100 headers)
%% @private
benchmark_http_headers() ->
    %% Generate test data: 100 headers
    Headers = [{<<"header-", (integer_to_binary(N))/binary>>, <<"value-", (integer_to_binary(N))/binary>>}
               || N <- lists:seq(1, 100)],

    %% Warm-up
    lists:foreach(fun(_) ->
        benchmark_headers_iolist(Headers)
    end, lists:seq(1, 1000)),

    %% Benchmark: iolist_to_binary (traditional)
    {IolistTime, _IolistResult} = timer:tc(fun() ->
        benchmark_headers_iolist(Headers)
    end, 1000),

    %% Benchmark: binary:join (OTP 28)
    {BinaryJoinTime, _BinaryJoinResult} = timer:tc(fun() ->
        Lines = [<<K/binary, ": ", V/binary>> || {K, V} <- Headers],
        binary:join(<<"\r\n">>, Lines)
    end, 1000),

    %% Calculate improvement
    Improvement = ((IolistTime - BinaryJoinTime) / IolistTime) * 100,

    #{
        benchmark => http_headers,
        iterations => 1000,
        header_count => 100,
        iolist_time_us => IolistTime,
        binary_join_time_us => BinaryJoinTime,
        improvement_percent => Improvement,
        winner => case Improvement > 0 of true -> binary_join; false -> iolist end
    }.

%% @doc Benchmark line joining (10K lines)
%% @private
benchmark_line_joining() ->
    %% Generate test data: 10K lines
    Lines = [<<"Line ", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 10000)],

    %% Warm-up
    lists:foreach(fun(_) ->
        benchmark_lines_iolist(Lines)
    end, lists:seq(1, 100)),

    %% Benchmark: iolist_to_binary (traditional)
    {IolistTime, _IolistResult} = timer:tc(fun() ->
        benchmark_lines_iolist(Lines)
    end, 100),

    %% Benchmark: binary:join (OTP 28)
    {BinaryJoinTime, _BinaryJoinResult} = timer:tc(fun() ->
        binary:join(<<"\n">>, Lines)
    end, 100),

    %% Calculate improvement
    Improvement = ((IolistTime - BinaryJoinTime) / IolistTime) * 100,

    #{
        benchmark => line_joining,
        iterations => 100,
        line_count => 10000,
        iolist_time_us => IolistTime,
        binary_join_time_us => BinaryJoinTime,
        improvement_percent => Improvement,
        winner => case Improvement > 0 of true -> binary_join; false -> iolist end
    }.

%% @doc Benchmark trace context formatting
%% @private
benchmark_trace_context() ->
    %% Generate test data: trace entries
    Entries = [{<<"trace-", (integer_to_binary(N))/binary>>,
                 <<"value-", (integer_to_binary(N))/binary>>}
               || N <- lists:seq(1, 100)],

    %% Warm-up
    lists:foreach(fun(_) ->
        benchmark_trace_iolist(Entries)
    end, lists:seq(1, 1000)),

    %% Benchmark: iolist_to_binary (traditional)
    {IolistTime, _IolistResult} = timer:tc(fun() ->
        benchmark_trace_iolist(Entries)
    end, 1000),

    %% Benchmark: binary:join (OTP 28)
    {BinaryJoinTime, _BinaryJoinResult} = timer:tc(fun() ->
        Formatted = [<<K/binary, "=", V/binary>> || {K, V} <- Entries],
        binary:join(<<";">>, Formatted)
    end, 1000),

    %% Calculate improvement
    Improvement = ((IolistTime - BinaryJoinTime) / IolistTime) * 100,

    #{
        benchmark => trace_context,
        iterations => 1000,
        entry_count => 100,
        iolist_time_us => IolistTime,
        binary_join_time_us => BinaryJoinTime,
        improvement_percent => Improvement,
        winner => case Improvement > 0 of true -> binary_join; false -> iolist end
    }.

%% @doc Benchmark metrics formatting
%% @private
benchmark_metrics() ->
    %% Generate test data: metrics
    Metrics = [{<<"metric-", (integer_to_binary(N))/binary>>, N}
               || N <- lists:seq(1, 10000)],

    %% Warm-up
    lists:foreach(fun(_) ->
        benchmark_metrics_iolist(Metrics)
    end, lists:seq(1, 100)),

    %% Benchmark: iolist_to_binary (traditional)
    {IolistTime, _IolistResult} = timer:tc(fun() ->
        benchmark_metrics_iolist(Metrics)
    end, 100),

    %% Benchmark: binary:join (OTP 28)
    {BinaryJoinTime, _BinaryJoinResult} = timer:tc(fun() ->
        Lines = [<<K/binary, " ", (integer_to_binary(V))/binary>> || {K, V} <- Metrics],
        Joined = binary:join(<<"\n">>, Lines),
        <<Joined/binary, "\n">>
    end, 100),

    %% Calculate improvement
    Improvement = ((IolistTime - BinaryJoinTime) / IolistTime) * 100,

    #{
        benchmark => metrics,
        iterations => 100,
        metric_count => 10000,
        iolist_time_us => IolistTime,
        binary_join_time_us => BinaryJoinTime,
        improvement_percent => Improvement,
        winner => case Improvement > 0 of true -> binary_join; false -> iolist end
    }.

%%====================================================================
%% Traditional Methods (iolist_to_binary)
%%====================================================================

%% @private Traditional iolist approach for SSE chunks
benchmark_join_iolist(Chunks) ->
    Parts = lists:join(<<"\n">>, Chunks),
    iolist_to_binary(Parts).

%% @private Traditional iolist approach for JSON array
benchmark_json_iolist(Items) ->
    Inner = lists:join(<<",">>, Items),
    iolist_to_binary([<<"[">>, Inner, <<"]">>]).

%% @private Traditional iolist approach for headers
benchmark_headers_iolist(Headers) ->
    Lines = [<<K/binary, ": ", V/binary>> || {K, V} <- Headers],
    Parts = lists:join(<<"\r\n">>, Lines),
    iolist_to_binary(Parts).

%% @private Traditional iolist approach for lines
benchmark_lines_iolist(Lines) ->
    Parts = lists:join(<<"\n">>, Lines),
    iolist_to_binary(Parts).

%% @private Traditional iolist approach for trace context
benchmark_trace_iolist(Entries) ->
    Formatted = [<<K/binary, "=", V/binary>> || {K, V} <- Entries],
    Parts = lists:join(<<";">>, Formatted),
    iolist_to_binary(Parts).

%% @private Traditional iolist approach for metrics
benchmark_metrics_iolist(Metrics) ->
    Lines = [<<K/binary, " ", (integer_to_binary(V))/binary>> || {K, V} <- Metrics],
    Parts = lists:join(<<"\n">>, Lines),
    iolist_to_binary([Parts, <<"\n">>]).
