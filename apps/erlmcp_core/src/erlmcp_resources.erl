-module(erlmcp_resources).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    stop/0,
    list_roots/0,
    add_root/2,
    remove_root/1,
    resolve_root_uri/1,
    read_resource/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type root_uri() :: binary().
-type root_name() :: binary().
-type root_entry() :: #{
    uri => root_uri(),
    name => root_name()
}.

-export_type([root_uri/0, root_name/0, root_entry/0]).

%% State record
-record(state, {
    roots = #{} :: #{root_uri() => root_entry()}
}).

-type state() :: #state{}.

%%%====================================================================
%%% API Functions
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc List all registered roots
-spec list_roots() -> {ok, [root_entry()]}.
list_roots() ->
    gen_server:call(?MODULE, list_roots).

%% @doc Add a root URI with a display name
-spec add_root(root_uri(), root_name()) -> ok.
add_root(Uri, Name) when is_binary(Uri), is_binary(Name) ->
    gen_server:call(?MODULE, {add_root, Uri, Name}).

%% @doc Remove a root URI
-spec remove_root(root_uri()) -> ok.
remove_root(Uri) when is_binary(Uri) ->
    gen_server:call(?MODULE, {remove_root, Uri}).

%% @doc Resolve a root URI to a filesystem path (for file:// URIs only)
-spec resolve_root_uri(root_uri()) -> {ok, file:name_all()} | {error, atom()}.
resolve_root_uri(Uri) when is_binary(Uri) ->
    gen_server:call(?MODULE, {resolve_root_uri, Uri}).

%% @doc Read a resource by URI (supports file:// scheme)
-spec read_resource(root_uri()) -> {ok, binary()} | {error, atom()}.
read_resource(Uri) when is_binary(Uri) ->
    gen_server:call(?MODULE, {read_resource, Uri}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    %% Initialize with default root (current working directory)
    {ok, Cwd} = file:get_cwd(),
    DefaultRoot = #{
        uri => list_to_binary(["file://", Cwd]),
        name => <<"Current Working Directory">>
    },
    InitialRoots = #{list_to_binary(["file://", Cwd]) => DefaultRoot},
    {ok, #state{roots = InitialRoots}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call(list_roots, _From, #state{roots = Roots} = State) ->
    RootList = maps:values(Roots),
    {reply, {ok, RootList}, State};

handle_call({add_root, Uri, Name}, _From, #state{roots = Roots} = State) ->
    RootEntry = #{
        uri => Uri,
        name => Name
    },
    NewRoots = maps:put(Uri, RootEntry, Roots),
    {reply, ok, State#state{roots = NewRoots}};

handle_call({remove_root, Uri}, _From, #state{roots = Roots} = State) ->
    NewRoots = maps:remove(Uri, Roots),
    {reply, ok, State#state{roots = NewRoots}};

handle_call({resolve_root_uri, Uri}, _From, State) ->
    Reply = do_resolve_root_uri(Uri),
    {reply, Reply, State};

handle_call({read_resource, Uri}, _From, State) ->
    Reply = do_read_resource(Uri),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Resolve a file:// URI to a filesystem path
-spec do_resolve_root_uri(binary()) -> {ok, file:name_all()} | {error, atom()}.
do_resolve_root_uri(<<"file://", Path/binary>>) ->
    %% Remove file:// prefix and convert to path
    case Path of
        <<"/", AbsolutePath/binary>> ->
            %% file:///absolute/path format
            FilePath = binary_to_list(AbsolutePath),
            check_file_exists(FilePath);
        <<"localhost/", RelativePath/binary>> ->
            %% file://localhost/relative/path format (treat as absolute)
            FilePath = "/" ++ binary_to_list(RelativePath),
            check_file_exists(FilePath);
        RelativePath ->
            %% file://relative/path format (shouldn't happen but handle it)
            FilePath = binary_to_list(RelativePath),
            check_file_exists(FilePath)
    end;

do_resolve_root_uri(<<Scheme, "://", _/binary>>) when is_integer(Scheme) ->
    %% Unknown scheme - could be s3://, custom://, etc.
    {error, unknown_uri_scheme};

do_resolve_root_uri(_) ->
    {error, unknown_uri_scheme}.

%% @doc Check if file exists, return path or error
-spec check_file_exists(file:name_all()) -> {ok, file:name_all()} | {error, enoent}.
check_file_exists(FilePath) ->
    case filelib:is_file(FilePath) of
        true -> {ok, FilePath};
        false -> {error, enoent}
    end.

%% @doc Read a resource by URI (supports file:// scheme)
-spec do_read_resource(binary()) -> {ok, binary()} | {error, atom()}.
do_read_resource(<<"file://", Path/binary>>) ->
    %% Extract path and read file
    FilePath = case Path of
        <<"/", AbsolutePath/binary>> -> binary_to_list(AbsolutePath);
        _ -> binary_to_list(Path)
    end,
    case file:read_file(FilePath) of
        {ok, Content} -> {ok, Content};
        {error, Reason} -> {error, Reason}
    end;

do_read_resource(_) ->
    {error, unknown_uri_scheme}.
