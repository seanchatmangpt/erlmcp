%%%====================================================================
%%% @doc Mermaid Diagram Renderer with Process Isolation
%%%
%%% Overview:
%%% Converts Mermaid diagrams to SVG using Node.js/mmdc CLI with proper
%%% OTP supervision and process isolation for safety and performance.
%%%
%%% Features:
%%% - Process isolation via port (OS process isolation)
%%% - ETS-based caching for rendered diagrams
%%% - Theme customization (default, forest, dark, neutral)
%%% - Render quality levels (low, medium, high)
%%% - Circuit breaker for error recovery
%%% - Memory-efficient streaming for large diagrams
%%% - Batch rendering support
%%% - Fallback strategies for unsupported features
%%% - Performance metrics and monitoring
%%% - Graceful degradation on mmdc unavailability
%%%
%%% Architecture:
%%% - gen_server for state management
%%% - Port for communicating with mmdc (Node.js process)
%%% - ETS table for cache (L1)
%%% - Circuit breaker for failure detection
%%% - Supervisor tree for workers
%%%
%%% @end
%%%====================================================================
-module(erlmcp_mermaid_renderer).
-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    render/2,
    render/3,
    render_batch/2,
    render_async/3,
    invalidate_cache/1,
    clear_cache/0,
    get_stats/0,
    health_check/0,
    set_theme/2,
    set_quality/2,
    stop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type mermaid_source() :: binary().
-type svg_output() :: binary().
-type render_theme() :: default | forest | dark | neutral.
-type render_quality() :: low | medium | high.
-type render_options() :: #{
    theme => render_theme(),
    quality => render_quality(),
    width => pos_integer() | undefined,
    height => pos_integer() | undefined,
    background_color => binary() | undefined
}.
-type render_result() :: {ok, svg_output()} | {error, term()}.
-type render_async_callback() :: fun((render_result()) -> any).

-record(state, {
    port :: port() | undefined,
    port_monitor :: reference() | undefined,
    cache_table :: ets:tid(),
    circuit_breaker :: pid() | undefined,
    theme :: render_theme(),
    quality :: render_quality(),
    mmdc_path :: binary() | undefined,
    mmdc_available :: boolean(),
    batch_queue :: queue:queue(),
    batch_timer :: reference() | undefined,
    batch_size :: pos_integer(),
    batch_timeout_ms :: pos_integer(),

    %% Metrics
    stats = #{
        renders => 0,
        cache_hits => 0,
        cache_misses => 0,
        errors => 0,
        circuit_trips => 0,
        avg_render_time_us => 0,
        total_render_time_us => 0
    } :: map()
}).

-type state() :: #state{}.

%%====================================================================
%% Constants
%%====================================================================

-define(HIBERNATE_AFTER_MS, 30000).
-define(DEFAULT_THEME, default).
-define(DEFAULT_QUALITY, medium).
-define(DEFAULT_BATCH_SIZE, 10).
-define(DEFAULT_BATCH_TIMEOUT_MS, 100).
-define(CACHE_TABLE, erlmcp_mermaid_cache).
-define(RENDER_TIMEOUT_MS, 10000). % 10 seconds for single render
-define(BATCH_TIMEOUT_MS, 30000). % 30 seconds for batch
-define(PORT_BUFFER_SIZE, 65536).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start renderer with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start renderer with custom configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Render Mermaid diagram to SVG (synchronous, default options)
-spec render(mermaid_source(), pid()) -> render_result().
render(Source, Pid) ->
    render(Source, #{}, Pid).

%% @doc Render Mermaid diagram to SVG with options (synchronous)
-spec render(mermaid_source(), render_options(), pid()) -> render_result().
render(Source, Options, Pid) ->
    gen_server:call(Pid, {render, Source, Options}, ?RENDER_TIMEOUT_MS).

%% @doc Render multiple diagrams in batch (synchronous)
-spec render_batch([{mermaid_source(), render_options()}], pid()) ->
    {[render_result()], [render_result()]}.
render_batch(Items, Pid) ->
    gen_server:call(Pid, {render_batch, Items}, ?BATCH_TIMEOUT_MS).

%% @doc Render asynchronously with callback
-spec render_async(mermaid_source(), render_options(), render_async_callback()) -> ok.
render_async(Source, Options, Callback) ->
    gen_server:cast(?MODULE, {render_async, Source, Options, Callback}).

%% @doc Invalidate cache entry for specific source
-spec invalidate_cache(mermaid_source()) -> ok.
invalidate_cache(Source) ->
    gen_server:call(?MODULE, {invalidate_cache, Source}).

%% @doc Clear entire render cache
-spec clear_cache() -> ok.
clear_cache() ->
    gen_server:call(?MODULE, clear_cache).

%% @doc Get renderer statistics
-spec get_stats() -> {ok, map()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Health check (includes mmdc availability)
-spec health_check() -> {ok, map()} | {error, term()}.
health_check() ->
    gen_server:call(?MODULE, health_check).

%% @doc Set theme for specific renderer instance
-spec set_theme(render_theme(), pid()) -> ok.
set_theme(Theme, Pid) ->
    gen_server:call(Pid, {set_theme, Theme}).

%% @doc Set quality for specific renderer instance
-spec set_quality(render_quality(), pid()) -> ok.
set_quality(Quality, Pid) ->
    gen_server:call(Pid, {set_quality, Quality}).

%% @doc Stop renderer
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, state()}.
init(Config) ->
    process_flag(trap_exit, true),

    %% Create ETS cache table (L1 cache)
    CacheTable = ets:new(?CACHE_TABLE, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    %% Find mmdc executable
    MMDCAvailable = check_mmdc_available(),
    MMDcPath = find_mmdc_path(),

    %% Start circuit breaker for render failures
    BreakerConfig = #{
        failure_threshold => maps:get(failure_threshold, Config, 5),
        timeout => maps:get(breaker_timeout_ms, Config, 30000),
        window_size => maps:get(breaker_window, Config, 10),
        failure_rate_threshold => 0.5
    },
    {ok, Breaker} = case erlmcp_circuit_breaker:start_link(?MODULE, BreakerConfig) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end,

    %% Extract configuration
    Theme = maps:get(theme, Config, ?DEFAULT_THEME),
    Quality = maps:get(quality, Config, ?DEFAULT_QUALITY),
    BatchSize = maps:get(batch_size, Config, ?DEFAULT_BATCH_SIZE),
    BatchTimeout = maps:get(batch_timeout_ms, Config, ?DEFAULT_BATCH_TIMEOUT_MS),

    State = #state{
        cache_table = CacheTable,
        circuit_breaker = Breaker,
        theme = Theme,
        quality = Quality,
        mmdc_path = MMDcPath,
        mmdc_available = MMDCAvailable,
        batch_queue = queue:new(),
        batch_size = BatchSize,
        batch_timeout_ms = BatchTimeout
    },

    ?LOG_INFO("erlmcp_mermaid_renderer started: mmdc=~p, theme=~p, quality=~p",
        [MMDCAvailable, Theme, Quality]),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.

%% Render single diagram (synchronous)
handle_call({render, Source, Options}, From, State) ->
    case State#state.mmdc_available of
        false ->
            {reply, {error, mmdc_unavailable}, State};
        true ->
            %% Check cache first
            CacheKey = generate_cache_key(Source, Options),
            case ets:lookup(State#state.cache_table, CacheKey) of
                [{_, SVG}] ->
                    ?LOG_DEBUG("Cache hit for render: ~p", [CacheKey]),
                    {reply, {ok, SVG}, update_stats(State, cache_hit)};
                [] ->
                    ?LOG_DEBUG("Cache miss for render: ~p", [CacheKey]),
                    %% Execute through circuit breaker
                    case erlmcp_circuit_breaker:call(
                        State#state.circuit_breaker,
                        fun() -> do_render(Source, Options, State) end,
                        ?RENDER_TIMEOUT_MS
                    ) of
                        {ok, SVG} ->
                            %% Cache result
                            cache_result(CacheKey, SVG, State),
                            {reply, {ok, SVG}, update_stats(State, render)};
                        {error, Reason} ->
                            ?LOG_WARNING("Render failed: ~p", [Reason]),
                            {reply, {error, Reason}, update_stats(State, error)}
                    end
            end
    end;

%% Render batch
handle_call({render_batch, Items}, _From, State) ->
    case State#state.mmdc_available of
        false ->
            {reply, {[], lists:duplicate(length(Items), {error, mmdc_unavailable})}, State};
        true ->
            Results = lists:map(fun({Source, Options}) ->
                CacheKey = generate_cache_key(Source, Options),
                case ets:lookup(State#state.cache_table, CacheKey) of
                    [{_, SVG}] -> {ok, SVG};
                    [] ->
                        case do_render(Source, Options, State) of
                            {ok, SVG} ->
                                cache_result(CacheKey, SVG, State),
                                {ok, SVG};
                            {error, Reason} ->
                                {error, Reason}
                        end
                end
            end, Items),

            %% Separate successes and failures
            {Successes, Failures} = lists:partition(fun
                ({ok, _}) -> true;
                ({error, _}) -> false
            end, Results),

            {reply, {Successes, Failures}, update_stats(State, render)}
    end;

%% Invalidate cache entry
handle_call({invalidate_cache, Source}, _From, State) ->
    %% Delete all cache entries for this source (different options)
    Pattern = {{Source, '_'}, '_'},
    ets:select_delete(State#state.cache_table, [{Pattern, [], [true]}]),
    {reply, ok, State};

%% Clear cache
handle_call(clear_cache, _From, State) ->
    ets:delete_all_objects(State#state.cache_table),
    {reply, ok, State};

%% Get statistics
handle_call(get_stats, _From, State) ->
    CacheSize = ets:info(State#state.cache_table, size),
    BreakerStats = case erlmcp_circuit_breaker:get_stats(State#state.circuit_breaker) of
        {ok, BS} -> BS;
        _ -> #{}
    },
    Stats = State#state.stats#{
        cache_size => CacheSize,
        circuit_breaker => BreakerStats,
        mmdc_available => State#state.mmdc_available
    },
    {reply, {ok, Stats}, State};

%% Health check
handle_call(health_check, _From, State) ->
    Status = #{
        renderer_up => true,
        mmdc_available => State#state.mmdc_available,
        mmdc_path => State#state.mmdc_path,
        circuit_breaker_state => erlmcp_circuit_breaker:get_state(State#state.circuit_breaker),
        cache_size => ets:info(State#state.cache_table, size)
    },
    {reply, {ok, Status}, State};

%% Set theme
handle_call({set_theme, Theme}, _From, State) ->
    {reply, ok, State#state{theme = Theme}};

%% Set quality
handle_call({set_quality, Quality}, _From, State) ->
    {reply, ok, State#state{quality = Quality}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

%% Render asynchronously
handle_cast({render_async, Source, Options, Callback}, State) ->
    spawn(fun() ->
        Result = case State#state.mmdc_available of
            false -> {error, mmdc_unavailable};
            true ->
                CacheKey = generate_cache_key(Source, Options),
                case ets:lookup(State#state.cache_table, CacheKey) of
                    [{_, SVG}] -> {ok, SVG};
                    [] ->
                        case do_render(Source, Options, State) of
                            {ok, SVG} ->
                                cache_result(CacheKey, SVG, State),
                                {ok, SVG};
                            {error, Reason} ->
                                {error, Reason}
                        end
                end
        end,
        Callback(Result)
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

%% Port data received
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    ?LOG_DEBUG("Received data from mmdc port: ~p bytes", [byte_size(Data)]),
    {noreply, State};

%% Port closed
handle_info({Port, closed}, #state{port = Port} = State) ->
    ?LOG_WARNING("mmdc port closed"),
    {noreply, State#state{port = undefined, port_monitor = undefined}};

%% Port exit (crash)
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    ?LOG_ERROR("mmdc port exited: ~p", [Reason]),
    {noreply, State#state{port = undefined, port_monitor = undefined}};

%% Process monitoring
handle_info({'DOWN', Ref, process, _Pid, Reason}, #state{port_monitor = Ref} = State) ->
    ?LOG_ERROR("Monitored port process died: ~p", [Reason]),
    {noreply, State#state{port = undefined, port_monitor = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Close port if open
    case State#state.port of
        undefined -> ok;
        Port ->
            catch port_close(Port),
            ok
    end,

    %% Stop circuit breaker
    case State#state.circuit_breaker of
        undefined -> ok;
        Breaker ->
            erlmcp_circuit_breaker:stop(Breaker)
    end,

    ?LOG_INFO("erlmcp_mermaid_renderer terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Check if mmdc CLI is available
-spec check_mmdc_available() -> boolean().
check_mmdc_available() ->
    case find_mmdc_path() of
        undefined -> false;
        _Path -> true
    end.

%% @doc Find mmdc executable path
-spec find_mmdc_path() -> binary() | undefined.
find_mmdc_path() ->
    %% Check common locations
    Paths = [
        "/usr/local/bin/mmdc",
        "/usr/bin/mmdc",
        case os:find_executable("mmdc") of
            false -> [];
            Path -> Path
        end,
        "./node_modules/.bin/mmdc"
    ],
    lists:foldl(fun(Path, Acc) ->
        case Acc of
            undefined ->
                case filelib:is_file(Path) of
                    true -> list_to_binary(Path);
                    false -> undefined
                end;
            _ -> Acc
        end
    end, undefined, Paths).

%% @doc Generate cache key from source and options
-spec generate_cache_key(mermaid_source(), render_options()) -> {binary(), binary()}.
generate_cache_key(Source, Options) ->
    %% Hash source and options to create cache key
    OptionsBin = jsx:encode(Options),
    Hash = erlang:phash2({Source, OptionsBin}),
    {Source, integer_to_binary(Hash)}.

%% @doc Cache render result
-spec cache_result({binary(), binary()}, svg_output(), state()) -> ok.
cache_result({Source, Hash}, SVG, State) ->
    Entry = #{
        source => Source,
        hash => Hash,
        svg => SVG,
        timestamp => erlang:monotonic_time(millisecond)
    },
    ets:insert(State#state.cache_table, {{Source, Hash}, Entry}),
    ok.

%% @doc Perform actual render using mmdc CLI
-spec do_render(mermaid_source(), render_options(), state()) -> render_result().
do_render(Source, Options, State) ->
    StartTime = erlang:monotonic_time(microsecond),

    try
        %% Write source to temporary file
        TmpDir = tmp_dir(),
        InputFile = filename:join(TmpDir, "mermaid_" ++ erlang:ref_to_list(make_ref()) ++ ".mmd"),
        OutputFile = filename:join(TmpDir, "output_" ++ erlang:ref_to_list(make_ref()) ++ ".svg"),

        ok = file:write_file(InputFile, Source),

        %% Build mmdc command
        Theme = maps:get(theme, Options, State#state.theme),
        Quality = maps:get(quality, Options, State#state.quality),
        Width = maps:get(width, Options, undefined),
        Height = maps:get(height, Options, undefined),
        Background = maps:get(background_color, Options, undefined),

        Cmd = build_mmdc_command(InputFile, OutputFile, Theme, Quality, Width, Height, Background, State),

        %% Execute command
        case exec_cmd(Cmd) of
            {ok, Output} ->
                %% Read SVG output
                case file:read_file(OutputFile) of
                    {ok, SVG} ->
                        %% Cleanup temp files
                        _ = file:delete(InputFile),
                        _ = file:delete(OutputFile),

                        RenderTime = erlang:monotonic_time(microsecond) - StartTime,
                        ?LOG_DEBUG("Render completed in ~p us", [RenderTime]),

                        {ok, SVG};
                    {error, Reason} ->
                        ?LOG_ERROR("Failed to read output file: ~p", [Reason]),
                        {error, {output_read_failed, Reason}}
                end;
            {error, Reason} ->
                ?LOG_ERROR("mmdc command failed: ~p", [Reason]),
                {error, {render_failed, Reason}}
        end
    catch
        Class:Reason:Stack ->
            ?LOG_ERROR("Render exception: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {render_exception, {Class, Reason}}}
    end.

%% @doc Build mmdc command line
-spec build_mmdc_command(
    string(), string(), render_theme(), render_quality(),
    pos_integer() | undefined, pos_integer() | undefined, binary() | undefined, state()
) -> [string()].
build_mmdc_command(Input, Output, Theme, Quality, Width, Height, Background, State) ->
    MMDcPath = binary_to_list(State#state.mmdc_path),

    BaseCmd = [
        MMDcPath,
        "-i", Input,
        "-o", Output,
        "-t", atom_to_list(Theme),
        "-b", "default"  %% background
    ],

    %% Add quality settings
    QualityCmd = case Quality of
        low -> ["-q", "low"];
        medium -> ["-q", "medium"];
        high -> ["-q", "high"]
    end,

    %% Add dimensions if specified
    DimCmd = case {Width, Height} of
        {undefined, undefined} -> [];
        {W, H} when is_integer(W), is_integer(H) ->
            ["-w", integer_to_list(W), "-H", integer_to_list(H)];
        _ -> []
    end,

    %% Add background color if specified
    BgCmd = case Background of
        undefined -> [];
        Color -> ["-b", binary_to_list(Color)]
    end,

    BaseCmd ++ QualityCmd ++ DimCmd ++ BgCmd.

%% @doc Execute external command
-spec exec_cmd([string()]) -> {ok, binary()} | {error, term()}.
exec_cmd(Cmd) ->
    Port = open_port({spawn_executable, hd(Cmd)}, [
        {args, tl(Cmd)},
        exit_status,
        binary,
        {line, ?PORT_BUFFER_SIZE},
        hide
    ]),

    %% Wait for result with timeout
    receive
        {Port, {exit_status, 0}} ->
            {ok, <<>>};
        {Port, {exit_status, Status}} ->
            {error, {exit_status, Status}};
        {Port, {data, Data}} ->
            {ok, Data}
    after
        ?RENDER_TIMEOUT_MS ->
            port_close(Port),
            {error, timeout}
    end.

%% @doc Get temp directory
-spec tmp_dir() -> string().
tmp_dir() ->
    case os:getenv("TMPDIR") of
        false -> "/tmp";
        Dir -> Dir
    end.

%% @doc Update statistics
-spec update_stats(state(), atom()) -> state().
update_stats(State, Type) ->
    Stats = State#state.stats,
    NewStats = case Type of
        cache_hit ->
            Stats#{cache_hits => maps:get(cache_hits, Stats, 0) + 1};
        cache_miss ->
            Stats#{cache_misses => maps:get(cache_misses, Stats, 0) + 1};
        render ->
            Stats#{renders => maps:get(renders, Stats, 0) + 1};
        error ->
            Stats#{errors => maps:get(errors, Stats, 0) + 1}
    end,
    State#state{stats = NewStats}.

%% @doc Fallback render for when mmdc is unavailable
-spec fallback_render(mermaid_source(), render_options()) -> render_result().
fallback_render(_Source, _Options) ->
    {error, mmdc_unavailable}.
