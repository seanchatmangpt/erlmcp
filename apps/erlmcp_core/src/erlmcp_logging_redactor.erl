%%%-------------------------------------------------------------------
%%% @doc
%%% Secret redaction module for logging safety.
%%%
%%% FM-08: Secret Redaction Layer
%%% CVSS: 7.5 (High) - Credential theft from logs
%%% RPN: 270 (Critical)
%%%
%%% This module prevents sensitive credentials from appearing in logs:
%%% - JWT tokens (Bearer, base64 patterns)
%%% - Session IDs
%%% - API keys and secrets
%%% - Passwords
%%% - OAuth tokens
%%%
%%% Redaction occurs BEFORE logging to prevent any window where
%%% credentials could be exposed.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_logging_redactor).

%% API
-export([
    redact_message/1,
    redact_message/2,
    redact_data/1,
    redact_data/2,
    is_secret_key/1,
    default_patterns/0
]).

-include("erlmcp.hrl").

%% Type definitions
-type redaction_pattern() :: {atom(), binary()}.
-type redaction_config() :: #{
    enabled => boolean(),
    patterns => [redaction_pattern()],
    replacement => binary(),
    secret_keys => [binary() | atom()]
}.

-export_type([redaction_pattern/0, redaction_config/0]).

%% Constants
-define(DEFAULT_REPLACEMENT, <<"***REDACTED***">>).
-define(SECRET_KEYS, [
    <<"password">>, password,
    <<"secret">>, secret,
    <<"api_key">>, api_key,
    <<"apikey">>, apikey,
    <<"access_token">>, access_token,
    <<"refresh_token">>, refresh_token,
    <<"session_id">>, session_id,
    <<"sessionId">>, sessionId,
    <<"jwt">>, jwt,
    <<"bearer">>, bearer,
    <<"token">>, token,
    <<"private_key">>, private_key,
    <<"privateKey">>, privateKey,
    <<"client_secret">>, client_secret,
    <<"clientSecret">>, clientSecret,
    <<"auth_token">>, auth_token,
    <<"authToken">>, authToken,
    <<"credential">>, credential,
    <<"credentials">>, credentials
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Redact sensitive data from any Erlang term using default config.
-spec redact_message(term()) -> term().
redact_message(Term) ->
    Config = get_redaction_config(),
    redact_message(Term, Config).

%% @doc Redact sensitive data from any Erlang term with custom config.
-spec redact_message(term(), redaction_config()) -> term().
redact_message(Term, #{enabled := false}) ->
    Term;
redact_message(Term, Config) when is_map(Config) ->
    redact_term(Term, Config);
redact_message(Term, _Config) ->
    redact_term(Term, get_redaction_config()).

%% @doc Redact data map specifically (for structured logging).
-spec redact_data(map() | undefined) -> map() | undefined.
redact_data(undefined) ->
    undefined;
redact_data(Data) when is_map(Data) ->
    Config = get_redaction_config(),
    redact_data(Data, Config).

%% @doc Redact data map with custom config.
-spec redact_data(map() | undefined, redaction_config()) -> map() | undefined.
redact_data(undefined, _Config) ->
    undefined;
redact_data(Data, #{enabled := false}) ->
    Data;
redact_data(Data, Config) when is_map(Data) ->
    redact_map(Data, Config).

%% @doc Check if a key is considered a secret key.
-spec is_secret_key(atom() | binary()) -> boolean().
is_secret_key(Key) when is_atom(Key) ->
    lists:member(Key, ?SECRET_KEYS);
is_secret_key(Key) when is_binary(Key) ->
    lists:member(Key, ?SECRET_KEYS) orelse
    lists:member(binary_to_existing_atom(Key, utf8), ?SECRET_KEYS).

%% @doc Get default redaction patterns.
-spec default_patterns() -> [redaction_pattern()].
default_patterns() ->
    [
        % JWT Bearer tokens
        {jwt_bearer, <<"Bearer\\s+[A-Za-z0-9_\\-\\.]+(?:\\.[A-Za-z0-9_\\-\\.]+)*">>},
        % Base64 JWT pattern (xxx.yyy.zzz)
        {jwt_base64, <<"[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+">>},
        % Session IDs (UUID format)
        {session_uuid, <<"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}">>},
        % API keys (alphanumeric, 32+ chars)
        {api_key_pattern, <<"[A-Za-z0-9]{32,}">>},
        % AWS keys
        {aws_access_key, <<"AKIA[0-9A-Z]{16}">>},
        {aws_secret_key, <<"[A-Za-z0-9/+=]{40}">>},
        % OAuth patterns
        {oauth_token, <<"ya29\\.[0-9A-Za-z\\-_]+">>}
    ].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Get redaction configuration from application env.
-spec get_redaction_config() -> redaction_config().
get_redaction_config() ->
    case application:get_env(erlmcp, logging_redaction) of
        {ok, Config} when is_map(Config) ->
            maps:merge(default_config(), Config);
        _ ->
            default_config()
    end.

%% @private Default configuration.
-spec default_config() -> redaction_config().
default_config() ->
    #{
        enabled => true,
        patterns => default_patterns(),
        replacement => ?DEFAULT_REPLACEMENT,
        secret_keys => ?SECRET_KEYS
    }.

%% @private Redact any Erlang term recursively.
-spec redact_term(term(), redaction_config()) -> term().
redact_term(Term, Config) when is_binary(Term) ->
    redact_binary(Term, Config);
redact_term(Term, Config) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true ->
            % It's a string, redact as binary
            String = unicode:characters_to_binary(Term),
            Redacted = redact_binary(String, Config),
            unicode:characters_to_list(Redacted);
        false ->
            % It's a list of terms
            redact_list(Term, Config)
    end;
redact_term(Term, Config) when is_map(Term) ->
    redact_map(Term, Config);
redact_term(Term, Config) when is_tuple(Term) ->
    redact_tuple(Term, Config);
redact_term(Term, _Config) ->
    % Atoms, numbers, pids, etc. - pass through
    Term.

%% @private Redact binary using regex patterns.
-spec redact_binary(binary(), redaction_config()) -> binary().
redact_binary(Binary, #{patterns := Patterns, replacement := Replacement}) ->
    lists:foldl(fun({_Name, Pattern}, Acc) ->
        case re:replace(Acc, Pattern, Replacement, [global, {return, binary}]) of
            Acc -> Acc;  % No match
            Redacted -> Redacted
        end
    end, Binary, Patterns).

%% @private Redact list recursively.
-spec redact_list(list(), redaction_config()) -> list().
redact_list(List, Config) ->
    [redact_term(Item, Config) || Item <- List].

%% @private Redact map, checking for secret keys.
-spec redact_map(map(), redaction_config()) -> map().
redact_map(Map, Config) ->
    #{replacement := Replacement, secret_keys := SecretKeys} = Config,
    maps:fold(fun(Key, Value, Acc) ->
        % Check if this key is a secret key
        IsSecret = lists:member(Key, SecretKeys),
        NewValue = case IsSecret of
            true ->
                Replacement;
            false ->
                redact_term(Value, Config)
        end,
        maps:put(Key, NewValue, Acc)
    end, #{}, Map).

%% @private Redact tuple recursively.
-spec redact_tuple(tuple(), redaction_config()) -> tuple().
redact_tuple(Tuple, Config) ->
    List = tuple_to_list(Tuple),
    RedactedList = redact_list(List, Config),
    list_to_tuple(RedactedList).

%%%===================================================================
%%% Tests (inline for simplicity)
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

redact_bearer_token_test() ->
    Input = <<"Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.payload.signature">>,
    Config = default_config(),
    Result = redact_binary(Input, Config),
    ?assertNotEqual(Input, Result),
    ?assertEqual(false, binary:match(Result, <<"eyJhbGci">>) =/= nomatch).

redact_session_id_test() ->
    Input = <<"session_id: 123e4567-e89b-12d3-a456-426614174000">>,
    Config = default_config(),
    Result = redact_binary(Input, Config),
    ?assertNotEqual(Input, Result),
    ?assertEqual(false, binary:match(Result, <<"123e4567">>) =/= nomatch).

redact_map_secrets_test() ->
    Input = #{
        <<"password">> => <<"secret123">>,
        <<"username">> => <<"alice">>,
        <<"api_key">> => <<"sk_live_1234567890">>
    },
    Config = default_config(),
    Result = redact_map(Input, Config),
    ?assertEqual(?DEFAULT_REPLACEMENT, maps:get(<<"password">>, Result)),
    ?assertEqual(?DEFAULT_REPLACEMENT, maps:get(<<"api_key">>, Result)),
    ?assertEqual(<<"alice">>, maps:get(<<"username">>, Result)).

redact_nested_map_test() ->
    Input = #{
        <<"user">> => #{
            <<"password">> => <<"secret123">>,
            <<"email">> => <<"alice@example.com">>
        },
        <<"auth">> => #{
            <<"token">> => <<"jwt_token_here">>
        }
    },
    Config = default_config(),
    Result = redact_map(Input, Config),
    User = maps:get(<<"user">>, Result),
    Auth = maps:get(<<"auth">>, Result),
    ?assertEqual(?DEFAULT_REPLACEMENT, maps:get(<<"password">>, User)),
    ?assertEqual(<<"alice@example.com">>, maps:get(<<"email">>, User)),
    ?assertEqual(?DEFAULT_REPLACEMENT, maps:get(<<"token">>, Auth)).

redact_list_test() ->
    Input = [
        <<"password: secret123">>,
        <<"username: alice">>,
        #{<<"api_key">> => <<"sk_1234">>}
    ],
    Config = default_config(),
    Result = redact_list(Input, Config),
    [First, Second, Third] = Result,
    ?assertEqual(<<"username: alice">>, Second),
    ?assertEqual(?DEFAULT_REPLACEMENT, maps:get(<<"api_key">>, Third)).

redact_aws_keys_test() ->
    Input = <<"AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
    Config = default_config(),
    Result = redact_binary(Input, Config),
    ?assertNotEqual(Input, Result),
    ?assertEqual(nomatch, binary:match(Result, <<"AKIAIOSFODNN7EXAMPLE">>)),
    ?assertEqual(nomatch, binary:match(Result, <<"wJalrXUtnFEMI">>)).

disabled_redaction_test() ->
    Input = <<"password: secret123">>,
    Config = #{enabled => false},
    Result = redact_message(Input, Config),
    ?assertEqual(Input, Result).

-endif.
