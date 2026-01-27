-module(erlmcp_localhost_binding).

%% API
-export([
    validate_bind_address/2,
    validate_bind_address/1,
    is_localhost/1,
    get_localhost_binding/0,
    get_localhost_binding/1,
    get_ipv6_localhost/0,
    normalize_address/1
]).

-include_lib("kernel/include/logger.hrl").

-define(LOCALHOST_IPV4, "127.0.0.1").
-define(LOCALHOST_IPV6, "::1").
-define(LOCALHOST_HOSTNAME, "localhost").
-define(BIND_ALL_IPV4, "0.0.0.0").
-define(BIND_ALL_IPV6, "::").

-type address() :: string() | binary() | atom().
-type validation_result() :: {ok, string()} | {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate bind address against localhost-only restriction
%% @param Address The bind address to validate
%% @param LocalhostOnly Whether to enforce localhost-only binding
%% @returns {ok, NormalizedAddress} or {error, Reason}
-spec validate_bind_address(address(), boolean()) -> validation_result().
validate_bind_address(Address, true) ->
    %% Localhost-only mode: enforce 127.0.0.1, ::1, or localhost
    validate_localhost_only(Address);
validate_bind_address(Address, false) ->
    %% No enforcement
    case normalize_address(Address) of
        {error, _} = Error -> Error;
        Normalized -> {ok, Normalized}
    end.

%% @doc Validate bind address with default localhost-only policy from config
%% @param Address The bind address to validate
%% @returns {ok, NormalizedAddress} or {error, Reason}
-spec validate_bind_address(address()) -> validation_result().
validate_bind_address(Address) ->
    LocalhostOnly = application:get_env(erlmcp, enforce_localhost_only, true),
    validate_bind_address(Address, LocalhostOnly).

%% @doc Check if address is a localhost address
%% @param Address The address to check
%% @returns true if address is localhost, false otherwise
-spec is_localhost(address()) -> boolean().
is_localhost(Address) ->
    Normalized = case normalize_address(Address) of
        {error, _} -> "";
        Addr -> Addr
    end,
    Normalized =:= ?LOCALHOST_IPV4 orelse
    Normalized =:= ?LOCALHOST_IPV6 orelse
    Normalized =:= ?LOCALHOST_HOSTNAME.

%% @doc Get configured localhost IPv4 binding address
%% @returns "127.0.0.1"
-spec get_localhost_binding() -> string().
get_localhost_binding() ->
    application:get_env(erlmcp, http_bind_address, ?LOCALHOST_IPV4).

%% @doc Get configured localhost IPv4 binding address or default
%% @param Default The default address if not configured
%% @returns Configured address or default
-spec get_localhost_binding(string()) -> string().
get_localhost_binding(Default) ->
    application:get_env(erlmcp, http_bind_address, Default).

%% @doc Get configured localhost IPv6 binding address
%% @returns "::1"
-spec get_ipv6_localhost() -> string().
get_ipv6_localhost() ->
    application:get_env(erlmcp, http_bind_ipv6, ?LOCALHOST_IPV6).

%%====================================================================
%% Internal Functions
%%====================================================================

%% Validate that address is a localhost address
-spec validate_localhost_only(address()) -> validation_result().
validate_localhost_only(Address) ->
    case normalize_address(Address) of
        {error, _} = Error ->
            Error;
        Normalized ->
            case is_localhost(Normalized) of
                true ->
                    {ok, Normalized};
                false ->
                    reject_non_localhost(Normalized)
            end
    end.

%% Reject non-localhost addresses with detailed error info
-spec reject_non_localhost(string()) -> {error, term()}.
reject_non_localhost(?BIND_ALL_IPV4) ->
    logger:error("Cannot bind to 0.0.0.0: localhost-only binding required", []),
    {error, {localhost_only_violation, ?BIND_ALL_IPV4, 'binds_to_all_interfaces'}};
reject_non_localhost(?BIND_ALL_IPV6) ->
    logger:error("Cannot bind to :: (IPv6 bind-all): localhost-only binding required", []),
    {error, {localhost_only_violation, ?BIND_ALL_IPV6, 'binds_to_all_interfaces'}};
reject_non_localhost(Address) ->
    logger:error("Cannot bind to ~s: only localhost addresses allowed", [Address]),
    {error, {localhost_only_violation, Address, 'non_localhost_address'}}.

%% Normalize address to string form
-spec normalize_address(address()) -> string() | {error, term()}.
normalize_address(Address) when is_binary(Address) ->
    try
        normalize_address(binary_to_list(Address))
    catch
        _:_ -> {error, {invalid_address, Address}}
    end;
normalize_address(Address) when is_list(Address) ->
    case validate_address_syntax(Address) of
        ok -> Address;
        error -> {error, {invalid_address_syntax, Address}}
    end;
normalize_address(Address) when is_atom(Address) ->
    try
        normalize_address(atom_to_list(Address))
    catch
        _:_ -> {error, {invalid_address, Address}}
    end;
normalize_address(Address) ->
    {error, {invalid_address_type, Address}}.

%% Validate address syntax
-spec validate_address_syntax(string()) -> ok | error.
validate_address_syntax(Address) ->
    %% Very basic validation: non-empty, reasonable length, valid characters
    case length(Address) of
        Len when Len > 0, Len =< 255 ->
            case validate_address_chars(Address) of
                ok -> ok;
                error -> error
            end;
        _ ->
            error
    end.

%% Check if address contains valid characters
-spec validate_address_chars(string()) -> ok | error.
validate_address_chars([]) ->
    ok;
validate_address_chars([H|T]) ->
    %% Allow alphanumeric, dots, colons, hyphens
    case H of
        C when C >= $0, C =< $9 -> validate_address_chars(T);
        C when C >= $a, C =< $z -> validate_address_chars(T);
        C when C >= $A, C =< $Z -> validate_address_chars(T);
        $. -> validate_address_chars(T);
        $: -> validate_address_chars(T);
        $- -> validate_address_chars(T);
        $_ -> validate_address_chars(T);
        _ -> error
    end.
