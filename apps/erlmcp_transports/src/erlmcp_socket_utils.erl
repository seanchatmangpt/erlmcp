-module(erlmcp_socket_utils).

%% OTP 26-28 Modern Socket API Utilities
%% Provides wrapper functions for Erlang socket module (OTP 26+)
%% with optimizations for MCP transports.

-export([create_tcp_socket/1,
         create_tcp_socket/2,
         enable_backpressure/1,
         set_buffer_sizes/2,
         get_socket_info/1,
         socket_to_gen_tcp/1,
         is_supported/0,
         get_otp_version/0]).

%% Types
-type socket_opts() :: #{domain => inet | inet6,
                         protocol => tcp,
                         rcvbuf => pos_integer(),
                         sndbuf => pos_integer(),
                         nodelay => boolean(),
                         reuseaddr => boolean(),
                         netns => term()}.
-type socket() :: socket:socket().
-type socket_info() :: #{atom() => term()}.

%% Default values
-define(DEFAULT_RCVBUF, 8192).
-define(DEFAULT_SNDBUF, 8192).
-define(DEFAULT_NODELAY, true).
-define(DEFAULT_REUSEADDR, true).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create an optimized TCP socket using modern socket API (OTP 26+)
%% Uses canonical protocol name 'tcp' (OTP 28 requirement)
-spec create_tcp_socket(socket_opts()) -> {ok, socket()} | {error, term()}.
create_tcp_socket(Options) when is_map(Options) ->
    create_tcp_socket(inet, Options).

%% @doc Create TCP socket with specified domain (inet/inet6)
-spec create_tcp_socket(inet | inet6, socket_opts()) -> {ok, socket()} | {error, term()}.
create_tcp_socket(Domain, Options) when is_map(Options) ->
    case is_supported() of
        true ->
            %% Extract options with defaults
            RcvBuf = maps:get(rcvbuf, Options, ?DEFAULT_RCVBUF),
            SndBuf = maps:get(sndbuf, Options, ?DEFAULT_SNDBUF),
            NoDelay = maps:get(nodelay, Options, ?DEFAULT_NODELAY),
            ReuseAddr = maps:get(reuseaddr, Options, ?DEFAULT_REUSEADDR),

            %% Build socket options list (OTP 26-28 format)
            SocketOpts =
                [{rcvbuf, RcvBuf},
                 {sndbuf, SndBuf},
                 {nodelay, NoDelay},
                 {reuseaddr, ReuseAddr}],

            %% Open socket with canonical protocol name (OTP 28 requirement)
            case socket:open(Domain, stream, tcp) of
                {ok, Socket} ->
                    %% Set socket options
                    case socket:setopts(Socket, SocketOpts) of
                        ok ->
                            {ok, Socket};
                        {error, OptsReason} ->
                            %% Clean up socket on option set failure
                            socket:close(Socket),
                            {error, {setopts_failed, OptsReason}}
                    end;
                {error, OpenReason} ->
                    {error, {open_failed, OpenReason}}
            end;
        false ->
            %% Fallback for OTP < 26
            {error, unsupported_otp_version}
    end.

%% @doc Enable backpressure support on socket (OTP 26+)
%% Uses socket:notify/2 for selective receive mode
-spec enable_backpressure(socket()) -> ok | {error, term()}.
enable_backpressure(Socket) ->
    case is_supported() of
        true ->
            %% Set to passive mode
            case socket:setopts(Socket, [{active, false}]) of
                ok ->
                    %% Request notification for next receive (OTP 26 backpressure)
                    %% Note: 'receive' is a reserved word, use 'recv' instead
                    case socket:notify(Socket, {select, recv, once}) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            {error, {notify_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {setopts_failed, Reason}}
            end;
        false ->
            {error, unsupported_otp_version}
    end.

%% @doc Set buffer sizes for an existing socket
-spec set_buffer_sizes(socket(), #{rcvbuf => pos_integer(),
                                   sndbuf => pos_integer()}) ->
    ok | {error, term()}.
set_buffer_sizes(Socket, BufferSizes) when is_map(BufferSizes) ->
    case is_supported() of
        true ->
            Opts = maps:fold(fun(K, V, Acc) ->
                                [{K, V} | Acc]
                             end, [], BufferSizes),
            socket:setopts(Socket, Opts);
        false ->
            {error, unsupported_otp_version}
    end.

%% @doc Get socket information using socket:info/1 (OTP 26+)
%% Returns map with socket statistics (buffer sizes, state, etc.)
-spec get_socket_info(socket()) -> {ok, socket_info()} | {error, term()}.
get_socket_info(Socket) ->
    case is_supported() of
        true ->
            case socket:info(Socket) of
                {ok, Info} when is_list(Info) ->
                    %% Convert proplist to map for easier access
                    InfoMap = maps:from_list(Info),
                    {ok, InfoMap};
                {ok, InfoMap} when is_map(InfoMap) ->
                    {ok, InfoMap};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, unsupported_otp_version}
    end.

%% @doc Convert socket:socket() to gen_tcp socket format
%% For backward compatibility with ranch and other gen_tcp-based systems
-spec socket_to_gen_tcp(socket()) -> {ok, term()} | {error, term()}.
socket_to_gen_tcp(Socket) ->
    %% OTP 26+ socket can be used directly with gen_tcp functions
    %% that have been updated to support socket module
    %% For older gen_tcp functions, we need to extract the file descriptor
    try
        %% Get the underlying socket file descriptor
        case socket:info(Socket) of
            {ok, Info} when is_list(Info) ->
                %% Extract fd from info list
                case proplists:get_value(fd, Info) of
                    undefined ->
                        {error, no_fd};
                    Fd when is_integer(Fd) ->
                        %% Return fd for use with gen_tcp
                        {ok, Fd}
                end;
            {ok, InfoMap} when is_map(InfoMap) ->
                case maps:get(fd, InfoMap, undefined) of
                    undefined ->
                        {error, no_fd};
                    Fd when is_integer(Fd) ->
                        {ok, Fd}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error ->
            {error, {conversion_failed, Error}}
    end.

%% @doc Check if modern socket API is supported (OTP 26+)
-spec is_supported() -> boolean().
is_supported() ->
    case get_otp_version() of
        Version when Version >= 26 ->
            true;
        _ ->
            false
    end.

%% @doc Get OTP version as integer
%% Returns major version number (e.g., 28 for OTP 28.3.1)
-spec get_otp_version() -> integer().
get_otp_version() ->
    try
        VersionStr = erlang:system_info(otp_release),
        %% Handle both "28" and "28.3.1" formats
        [MajorStr | _] = string:split(VersionStr, "."),
        list_to_integer(MajorStr)
    catch
        _:_ ->
            %% Default to 0 if version detection fails
            0
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate socket options
-spec validate_options(socket_opts()) -> ok | {error, term()}.
validate_options(Options) ->
    %% Validate buffer sizes are positive integers
    RcvBuf = maps:get(rcvbuf, Options, ?DEFAULT_RCVBUF),
    SndBuf = maps:get(sndbuf, Options, ?DEFAULT_SNDBUF),

    case is_integer(RcvBuf) andalso RcvBuf > 0 of
        true ->
            case is_integer(SndBuf) andalso SndBuf > 0 of
                true ->
                    ok;
                false ->
                    {error, invalid_sndbuf}
            end;
        false ->
            {error, invalid_rcvbuf}
    end.

-compile({nowarn_unused_function, validate_options/1}).  %% Reserved for future use
