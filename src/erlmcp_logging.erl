-module(erlmcp_logging).

-include("erlmcp.hrl").

%% API exports
-export([
    init_session_levels/0,
    validate_log_level/1,
    normalize_log_level/1,
    set_global_level/1,
    get_global_level/0,
    set_session_level/2,
    get_session_level/1,
    remove_session_level/1
]).

%% Types
-type log_level() :: debug | info | warning | error | critical.
-type session_id() :: binary() | atom().

-export_type([log_level/0, session_id/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Initialize ETS table for session log levels (called at application startup)
%% @return ok
-spec init_session_levels() -> ok.
init_session_levels() ->
    try
        ets:new(erlmcp_session_log_levels, [
            named_table,
            public,
            set,
            {keypos, 1},
            {write_concurrency, true},
            {read_concurrency, true}
        ]),
        ok
    catch
        error:badarg ->
            %% Table already exists
            ok
    end.

%% @doc Validate if a log level is valid according to MCP specification
%% @param Level - atom representing log level
%% @return {ok, NormalizedLevel} | {error, invalid_level}
-spec validate_log_level(term()) -> {ok, log_level()} | {error, invalid_level}.
validate_log_level(Level) when is_atom(Level) ->
    case lists:member(Level, ?MCP_VALID_LOG_LEVELS) of
        true -> {ok, Level};
        false -> {error, invalid_level}
    end;
validate_log_level(LevelBin) when is_binary(LevelBin) ->
    try
        Atom = binary_to_existing_atom(LevelBin, utf8),
        validate_log_level(Atom)
    catch
        _:_ -> {error, invalid_level}
    end;
validate_log_level(_) ->
    {error, invalid_level}.

%% @doc Normalize a log level to standard atom format
%% @param Level - log level in any format (atom, binary, string)
%% @return {ok, NormalizedLevel} | {error, invalid_level}
-spec normalize_log_level(term()) -> {ok, log_level()} | {error, invalid_level}.
normalize_log_level(Level) when is_atom(Level) ->
    validate_log_level(Level);
normalize_log_level(LevelBin) when is_binary(LevelBin) ->
    try
        %% Try to convert binary to existing atom
        LevelAtom = binary_to_existing_atom(LevelBin, utf8),
        validate_log_level(LevelAtom)
    catch
        error:badarg ->
            %% Try as string
            try
                LevelAtom2 = list_to_atom(binary_to_list(LevelBin)),
                validate_log_level(LevelAtom2)
            catch
                _:_ -> {error, invalid_level}
            end
    end;
normalize_log_level(LevelStr) when is_list(LevelStr) ->
    try
        LevelAtom = list_to_atom(LevelStr),
        validate_log_level(LevelAtom)
    catch
        _:_ -> {error, invalid_level}
    end;
normalize_log_level(_) ->
    {error, invalid_level}.

%% @doc Set global logging level (affects all new sessions)
%% @param Level - log level to set
%% @return ok | {error, invalid_level}
-spec set_global_level(log_level()) -> ok | {error, invalid_level}.
set_global_level(Level) ->
    case validate_log_level(Level) of
        {ok, ValidLevel} ->
            %% Configure OTP logger
            configure_otp_logger(ValidLevel),
            ok;
        Error ->
            Error
    end.

%% @doc Get current global logging level
%% @return {ok, Level} | {error, not_configured}
-spec get_global_level() -> {ok, log_level()} | {error, not_configured}.
get_global_level() ->
    case application:get_env(erlmcp, global_log_level) of
        {ok, Level} -> {ok, Level};
        undefined -> {ok, ?MCP_DEFAULT_LOG_LEVEL}
    end.

%% @doc Set session-specific logging level
%% @param SessionId - unique identifier for the session
%% @param Level - log level to set for this session
%% @return ok | {error, invalid_level}
-spec set_session_level(session_id(), log_level()) -> ok | {error, invalid_level}.
set_session_level(SessionId, Level) ->
    case validate_log_level(Level) of
        {ok, ValidLevel} ->
            ets:insert(erlmcp_session_log_levels, {SessionId, ValidLevel}),
            ok;
        Error ->
            Error
    end.

%% @doc Get session-specific logging level, or global level if not set
%% @param SessionId - unique identifier for the session
%% @return {ok, Level}
-spec get_session_level(session_id()) -> {ok, log_level()}.
get_session_level(SessionId) ->
    case ets:lookup(erlmcp_session_log_levels, SessionId) of
        [{SessionId, Level}] ->
            {ok, Level};
        [] ->
            get_global_level()
    end.

%% @doc Remove session-specific logging level
%% @param SessionId - unique identifier for the session
%% @return ok
-spec remove_session_level(session_id()) -> ok.
remove_session_level(SessionId) ->
    ets:delete(erlmcp_session_log_levels, SessionId),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Configure OTP logger with the specified level
-spec configure_otp_logger(log_level()) -> ok.
configure_otp_logger(Level) ->
    %% Map MCP levels to OTP logger levels
    OtpLevel = mcp_level_to_otp(Level),

    %% Update global default
    application:set_env(erlmcp, global_log_level, Level),

    %% Try to update kernel logger if available
    try
        case logger:get_primary_config() of
            #{level := _CurrentLevel} ->
                logger:set_primary_config(level, OtpLevel),
                ok;
            _ ->
                ok
        end
    catch
        _:_ ->
            ok
    end.

%% @private
%% Map MCP log levels to OTP logger severity levels
-spec mcp_level_to_otp(log_level()) -> term().
mcp_level_to_otp(debug) -> debug;
mcp_level_to_otp(info) -> info;
mcp_level_to_otp(warning) -> warning;
mcp_level_to_otp(error) -> error;
mcp_level_to_otp(critical) -> critical.
