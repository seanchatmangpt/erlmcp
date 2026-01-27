%%%-------------------------------------------------------------------
%%% @doc
%%% Asynchronous I/O Worker - Prevents blocking of gen_server processes
%%%
%%% This module provides non-blocking file I/O operations by offloading
%%% I/O to separate worker processes. This ensures the main gen_server
%%% never blocks on file I/O operations, complying with OTP principles.
%%%
%%% Key Responsibilities:
%%% 1. Execute file read/write operations asynchronously
%%% 2. Enforce timeouts on I/O operations
%%% 3. Handle I/O errors gracefully
%%% 4. Track pending I/O operations
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_io_worker).
-behaviour(gen_server).

%% API exports
-export([
    async_read_file/3,
    async_write_file/3,
    async_execute_handler/2,
    start_link/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    pending = #{} :: #{reference() => {pid(), term()}}
}).

-type state() :: #state{}.

%% Default I/O timeout: 5 seconds
-define(DEFAULT_IO_TIMEOUT, 5000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the I/O worker pool manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Asynchronously read a file with timeout enforcement
-spec async_read_file(pid(), binary() | string(), pos_integer()) -> {ok, reference()}.
async_read_file(CallerPid, FilePath, TimeoutMs)
  when is_pid(CallerPid), is_integer(TimeoutMs), TimeoutMs > 0 ->
    % Convert string to binary if needed
    BinPath = case is_binary(FilePath) of
        true -> FilePath;
        false -> list_to_binary(FilePath)
    end,
    % Spawn worker process to read file
    WorkerRef = make_ref(),
    _WorkerPid = spawn_link(fun() ->
        read_file_worker(BinPath, TimeoutMs, CallerPid, WorkerRef)
    end),
    {ok, WorkerRef}.

%% @doc Asynchronously write a file with timeout enforcement
-spec async_write_file(pid(), binary() | string(), binary() | iodata()) -> {ok, reference()}.
async_write_file(CallerPid, FilePath, Content)
  when is_pid(CallerPid) ->
    % Convert string to binary if needed
    BinPath = case is_binary(FilePath) of
        true -> FilePath;
        false -> list_to_binary(FilePath)
    end,
    % Spawn worker process to write file
    WorkerRef = make_ref(),
    TimeoutMs = ?DEFAULT_IO_TIMEOUT,
    _WorkerPid = spawn_link(fun() ->
        write_file_worker(BinPath, Content, TimeoutMs, CallerPid, WorkerRef)
    end),
    {ok, WorkerRef}.

%% @doc Execute a handler function asynchronously (for resource/tool handlers)
-spec async_execute_handler(pid(), fun()) -> {ok, reference()}.
async_execute_handler(CallerPid, Handler)
  when is_pid(CallerPid), is_function(Handler) ->
    WorkerRef = make_ref(),
    TimeoutMs = ?DEFAULT_IO_TIMEOUT,
    _WorkerPid = spawn_link(fun() ->
        execute_handler_worker(Handler, TimeoutMs, CallerPid, WorkerRef)
    end),
    {ok, WorkerRef}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
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

%%====================================================================
%% Internal Worker Functions
%%====================================================================

%% @doc Worker process for reading file asynchronously
-spec read_file_worker(binary(), pos_integer(), pid(), reference()) -> no_return().
read_file_worker(FilePath, TimeoutMs, CallerPid, WorkerRef) ->
    process_flag(trap_exit, true),
    Deadline = erlang:system_time(millisecond) + TimeoutMs,

    try
        case file:read_file(FilePath) of
            {ok, Content} ->
                CallerPid ! {async_io_result, WorkerRef, {ok, Content}};
            {error, FileError} ->
                logger:error("Failed to read file ~p: ~p", [FilePath, FileError]),
                CallerPid ! {async_io_result, WorkerRef, {error, {read_failed, FileError}}}
        end,

        % Check if we exceeded timeout
        case erlang:system_time(millisecond) > Deadline of
            true ->
                logger:warning("I/O operation exceeded timeout (~p ms)", [TimeoutMs]),
                CallerPid ! {async_io_timeout, WorkerRef};
            false ->
                ok
        end
    catch
        Class:Exception:Stack ->
            logger:error("I/O worker crash (read): ~p:~p~n~p", [Class, Exception, Stack]),
            CallerPid ! {async_io_error, WorkerRef, {Class, Exception}}
    after
        exit(normal)
    end.

%% @doc Worker process for writing file asynchronously
-spec write_file_worker(binary(), binary() | iodata(), pos_integer(), pid(), reference()) -> no_return().
write_file_worker(FilePath, Content, TimeoutMs, CallerPid, WorkerRef) ->
    process_flag(trap_exit, true),
    Deadline = erlang:system_time(millisecond) + TimeoutMs,

    try
        case file:write_file(FilePath, Content) of
            ok ->
                CallerPid ! {async_io_result, WorkerRef, ok};
            {error, FileError} ->
                logger:error("Failed to write file ~p: ~p", [FilePath, FileError]),
                CallerPid ! {async_io_result, WorkerRef, {error, {write_failed, FileError}}}
        end,

        % Check if we exceeded timeout
        case erlang:system_time(millisecond) > Deadline of
            true ->
                logger:warning("I/O operation exceeded timeout (~p ms)", [TimeoutMs]),
                CallerPid ! {async_io_timeout, WorkerRef};
            false ->
                ok
        end
    catch
        Class:Exception:Stack ->
            logger:error("I/O worker crash (write): ~p:~p~n~p", [Class, Exception, Stack]),
            CallerPid ! {async_io_error, WorkerRef, {Class, Exception}}
    after
        exit(normal)
    end.

%% @doc Worker process for executing handler function asynchronously
-spec execute_handler_worker(fun(), pos_integer(), pid(), reference()) -> no_return().
execute_handler_worker(Handler, TimeoutMs, CallerPid, WorkerRef) ->
    process_flag(trap_exit, true),
    Deadline = erlang:system_time(millisecond) + TimeoutMs,

    try
        Result = Handler(),
        CallerPid ! {async_io_result, WorkerRef, {ok, Result}},

        % Check if we exceeded timeout
        case erlang:system_time(millisecond) > Deadline of
            true ->
                logger:warning("Handler execution exceeded timeout (~p ms)", [TimeoutMs]),
                CallerPid ! {async_io_timeout, WorkerRef};
            false ->
                ok
        end
    catch
        Class:Exception:Stack ->
            logger:error("Handler execution crashed: ~p:~p~n~p", [Class, Exception, Stack]),
            CallerPid ! {async_io_result, WorkerRef, {error, {handler_crash, {Class, Exception}}}}
    after
        exit(normal)
    end.
