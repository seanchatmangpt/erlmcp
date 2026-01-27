-module(erlmcp_roots).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behavior(gen_server).

%% API exports
-export([
    start_link/1,
    add_root/1,
    remove_root/1,
    list_roots/0,
    validate_path/1,
    canonicalize_path/1,
    is_path_allowed/1
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(ROOTS_TABLE, roots_table).

-record(state, {
    allowed_roots = [] :: [binary()],
    symlink_follow = false :: boolean(),
    watchers = #{} :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec add_root(binary()) -> {ok, binary()} | {error, term()}.
add_root(RootPath) when is_binary(RootPath) ->
    gen_server:call(?MODULE, {add_root, RootPath}, 5000).

-spec remove_root(binary()) -> {ok, binary()} | {error, term()}.
remove_root(RootPath) when is_binary(RootPath) ->
    gen_server:call(?MODULE, {remove_root, RootPath}, 5000).

-spec list_roots() -> {ok, [binary()]} | {error, term()}.
list_roots() ->
    gen_server:call(?MODULE, list_roots, 5000).

-spec validate_path(binary()) -> {ok, binary()} | {error, term()}.
validate_path(Path) when is_binary(Path) ->
    SpanCtx = erlmcp_tracing:start_span(<<"roots.validate_path">>),
    try
        case is_path_allowed(Path) of
            true ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, Path};
            false ->
                erlmcp_tracing:record_error_details(SpanCtx, path_not_allowed, Path),
                {error, {path_not_allowed, Path}}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

validate_path(_) ->
    {error, invalid_path_format}.

-spec canonicalize_path(binary()) -> {ok, binary()} | {error, term()}.
canonicalize_path(Path) when is_binary(Path) ->
    SpanCtx = erlmcp_tracing:start_span(<<"roots.canonicalize_path">>),
    try
        PathStr = binary_to_list(Path),
        CanonicalPath = case file:canonicalize_path(PathStr) of
            {ok, Canonical} -> Canonical;
            {error, _} -> PathStr
        end,

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"path">> => Path,
            <<"canonical_path">> => erlang:list_to_binary(CanonicalPath)
        }),

        erlmcp_tracing:set_status(SpanCtx, ok),
        {ok, erlang:list_to_binary(CanonicalPath)}
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

canonicalize_path(_) ->
    {error, invalid_path_format}.

-spec is_path_allowed(binary()) -> boolean().
is_path_allowed(Path) when is_binary(Path) ->
    SpanCtx = erlmcp_tracing:start_span(<<"roots.is_path_allowed">>),
    try
        case canonicalize_path(Path) of
            {ok, CanonicalPath} ->
                AllowedRoots = ets:lookup_element(?ROOTS_TABLE, allowed_roots, 2),
                Result = lists:any(
                    fun(Root) ->
                        is_subpath(CanonicalPath, Root)
                    end,
                    AllowedRoots
                ),
                erlmcp_tracing:set_status(SpanCtx, ok),
                Result;
            {error, _} ->
                erlmcp_tracing:record_error_details(SpanCtx, canonicalization_failed, Path),
                false
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            false
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

is_path_allowed(_) ->
    false.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    erlmcp_tracing:start_span(<<"roots.init">>),

    ets:new(?ROOTS_TABLE, [named_table, set, public]),

    AllowedPaths = maps:get(allowed_paths, Config, []),
    SymlinkFollow = maps:get(symlink_follow, Config, false),

    %% Store initial allowed roots
    ets:insert(?ROOTS_TABLE, {allowed_roots, AllowedPaths}),
    ets:insert(?ROOTS_TABLE, {symlink_follow, SymlinkFollow}),

    %% Start filesystem watchers for each root
    Watchers = start_watchers(AllowedPaths),

    {ok, #state{
        allowed_roots = AllowedPaths,
        symlink_follow = SymlinkFollow,
        watchers = Watchers
    }}.

handle_call({add_root, RootPath}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"roots.add_root">>),
    try
        case validate_root_path(RootPath) of
            {ok, CanonicalRoot} ->
                CurrentRoots = ets:lookup_element(?ROOTS_TABLE, allowed_roots, 2),
                NewRoots = lists:append(CurrentRoots, [CanonicalRoot]),

                ets:insert(?ROOTS_TABLE, {allowed_roots, NewRoots}),

                %% Start watcher for new root
                {ok, WatcherPid} = start_single_watcher(CanonicalRoot),
                NewWatchers = maps:put(CanonicalRoot, WatcherPid, State#state.watchers),

                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"root_path">> => CanonicalRoot
                }),

                erlmcp_tracing:set_status(SpanCtx, ok),
                {reply, {ok, CanonicalRoot}, State#state{
                    allowed_roots = NewRoots,
                    watchers = NewWatchers
                }};
            {error, Reason} ->
                erlmcp_tracing:record_error_details(SpanCtx, invalid_root, Reason),
                {reply, {error, Reason}, State}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {error, {Class, CaughtReason}}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call({remove_root, RootPath}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"roots.remove_root">>),
    try
        case canonicalize_path(RootPath) of
            {ok, CanonicalRoot} ->
                CurrentRoots = ets:lookup_element(?ROOTS_TABLE, allowed_roots, 2),
                NewRoots = lists:filter(
                    fun(Root) -> Root =/= CanonicalRoot end,
                    CurrentRoots
                ),

                ets:insert(?ROOTS_TABLE, {allowed_roots, NewRoots}),

                %% Stop watcher
                case maps:get(CanonicalRoot, State#state.watchers, undefined) of
                    undefined -> ok;
                    WatcherPid ->
                        try
                            exit(WatcherPid, kill)
                        catch
                            _:_ -> ok
                        end
                end,

                NewWatchers = maps:remove(CanonicalRoot, State#state.watchers),

                erlmcp_tracing:set_status(SpanCtx, ok),
                {reply, {ok, CanonicalRoot}, State#state{
                    allowed_roots = NewRoots,
                    watchers = NewWatchers
                }};
            {error, Reason} ->
                erlmcp_tracing:record_error_details(SpanCtx, invalid_root, Reason),
                {reply, {error, Reason}, State}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {error, {Class, CaughtReason}}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call(list_roots, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"roots.list_roots">>),
    try
        Roots = ets:lookup_element(?ROOTS_TABLE, allowed_roots, 2),
        erlmcp_tracing:set_status(SpanCtx, ok),
        {reply, {ok, Roots}, State}
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {error, {Class, CaughtReason}}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Stop all watchers
    maps:foreach(
        fun(_, WatcherPid) ->
            try
                exit(WatcherPid, kill)
            catch
                _:_ -> ok
            end
        end,
        State#state.watchers
    ),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec validate_root_path(binary()) -> {ok, binary()} | {error, term()}.
validate_root_path(Path) when is_binary(Path) ->
    PathStr = binary_to_list(Path),
    case file:read_file_info(PathStr) of
        {ok, FileInfo} ->
            case maps:get(type, FileInfo, undefined) of
                directory -> canonicalize_path(Path);
                _ -> {error, not_a_directory}
            end;
        {error, Reason} ->
            {error, Reason}
    end;

validate_root_path(_) ->
    {error, invalid_path_format}.

-spec start_watchers([binary()]) -> map().
start_watchers(Roots) ->
    maps:from_list(
        lists:map(
            fun(Root) ->
                {ok, WatcherPid} = start_single_watcher(Root),
                {Root, WatcherPid}
            end,
            Roots
        )
    ).

-spec start_single_watcher(binary()) -> {ok, pid()}.
start_single_watcher(RootPath) ->
    %% Simplified watcher - in production would use fs module
    Pid = spawn(fun() -> watcher_loop(RootPath) end),
    {ok, Pid}.

-spec watcher_loop(binary()) -> no_return().
watcher_loop(_RootPath) ->
    receive
        stop -> exit(normal);
        _ -> watcher_loop(_RootPath)
    end.

-spec is_subpath(binary(), binary()) -> boolean().
is_subpath(Path, Root) ->
    case binary:match(Path, Root) of
        {0, _} -> true;  %% Root is prefix
        _ -> false
    end.
