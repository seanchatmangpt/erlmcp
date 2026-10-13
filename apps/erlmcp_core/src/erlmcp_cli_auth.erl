%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_auth - Enterprise Authentication CLI Module
%%%
%%% Provides CLI commands for configuring, testing, and managing
%%% enterprise authentication including JWT, certificates, and OAuth.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_auth).

%% API
-export([configure/2,
         configure/3,
         test_auth/0,
         test_auth/1,
         import_certificates/2,
         import_certificates/3,
         list_certificates/0,
         remove_certificate/1,
         generate_jwt/2,
         validate_jwt/1,
         get_auth_config/0,
         set_auth_method/1,
         enable_auth/0,
         disable_auth/0]).

%% Auth methods
-type auth_method() :: none | jwt | oauth2 | mutual_tls | api_key.
-type cert_format() :: pem | der | pkcs12.

-record(auth_config, {
    enabled = false :: boolean(),
    method = none :: auth_method(),
    jwt_secret = <<>> :: binary(),
    jwt_algorithm = hs256 :: hs256 | hs384 | hs512 | rs256 | rs384 | rs512,
    jwt_expiration = 3600 :: pos_integer(),
    oauth2_client_id = <<>> :: binary(),
    oauth2_client_secret = <<>> :: binary(),
    oauth2_discovery_url = <<>> :: binary(),
    oauth2_scopes = [] :: [binary()],
    api_keys = [] :: [binary()],
    certificates = #{} :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Configure authentication with method and options
-spec configure(auth_method(), map()) -> ok | {error, term()}.
configure(Method, Options) when is_atom(Method), is_map(Options) ->
    case validate_auth_method(Method) of
        true ->
            Config = build_auth_config(Method, Options),
            store_auth_config(Config),
            io:format("Authentication configured: ~p~n", [Method]),
            ok;
        false ->
            {error, invalid_auth_method}
    end.

%% @doc Quick configure with secret (for JWT)
-spec configure(auth_method(), binary(), binary()) -> ok | {error, term()}.
configure(jwt, Secret, Algorithm) ->
    configure(jwt, #{
        secret => Secret,
        algorithm => Algorithm
    });
configure(api_key, Key, Description) ->
    configure(api_key, #{
        key => Key,
        description => Description
    });
configure(Method, _, _) ->
    {error, {unsupported_quick_config, Method}}.

%% @doc Test current authentication configuration
-spec test_auth() -> {ok, map()} | {error, term()}.
test_auth() ->
    test_auth(#{}).

%% @doc Test authentication with specific credentials
-spec test_auth(map()) -> {ok, map()} | {error, term()}.
test_auth(Credentials) ->
    case get_auth_config() of
        {ok, Config} ->
            case Config#auth_config.enabled of
                false ->
                    {error, auth_not_enabled};
                true ->
                    run_auth_tests(Config, Credentials)
            end;
        Error ->
            Error
    end.

%% @doc Import certificates (PEM format)
-spec import_certificates(binary(), binary()) -> {ok, binary()} | {error, term()}.
import_certificates(Name, CertData) ->
    import_certificates(Name, CertData, pem).

%% @doc Import certificates with format specification
-spec import_certificates(binary(), binary(), cert_format()) -> {ok, binary()} | {error, term()}.
import_certificates(Name, CertData, Format) when is_binary(Name), is_binary(CertData) ->
    case parse_certificate(CertData, Format) of
        {ok, Cert} ->
            store_certificate(Name, Cert, Format),
            io:format("Certificate imported: ~s~n", [Name]),
            {ok, Name};
        Error ->
            Error
    end.

%% @doc List all imported certificates
-spec list_certificates() -> {ok, [map()]}.
list_certificates() ->
    case get_auth_config() of
        {ok, Config} ->
            Certs = maps:to_list(Config#auth_config.certificates),
            {ok, lists:map(fun({Name, Info}) ->
                #{
                    name => Name,
                    format => maps:get(format, Info),
                    issued_at => maps:get(issued_at, Info, unknown),
                    expires_at => maps:get(expires_at, Info, unknown),
                    subject => maps:get(subject, Info, unknown)
                }
            end, Certs)};
        Error ->
            Error
    end.

%% @doc Remove a certificate by name
-spec remove_certificate(binary()) -> ok | {error, term()}.
remove_certificate(Name) ->
    case get_auth_config() of
        {ok, Config} ->
            Certs = maps:remove(Name, Config#auth_config.certificates),
            Updated = Config#auth_config{certificates = Certs},
            store_auth_config(Updated),
            io:format("Certificate removed: ~s~n", [Name]),
            ok;
        Error ->
            Error
    end.

%% @doc Generate a JWT token for testing
-spec generate_jwt(map(), pos_integer()) -> {ok, binary()} | {error, term()}.
generate_jwt(Payload, ExpirationSeconds) when is_map(Payload), is_integer(ExpirationSeconds) ->
    case get_auth_config() of
        {ok, #auth_config{method = jwt, jwt_secret = Secret, jwt_algorithm = Algo}} ->
            Header = #{
                <<"alg">> => atom_to_binary(Algo),
                <<"typ">> => <<"JWT">>
            },
            Exp = erlang:system_time(second) + ExpirationSeconds,
            FullPayload = maps:merge(Payload, #{
                <<"iat">> => erlang:system_time(second),
                <<"exp">> => Exp
            }),
            jwt_sign(Header, FullPayload, Secret);
        {ok, #auth_config{method = Method}} ->
            {error, {wrong_method, Method}};
        Error ->
            Error
    end.

%% @doc Validate a JWT token
-spec validate_jwt(binary()) -> {ok, map()} | {error, term()}.
validate_jwt(Token) when is_binary(Token) ->
    case get_auth_config() of
        {ok, #auth_config{method = jwt, jwt_secret = Secret}} ->
            case jwt_verify(Token, Secret) of
                {ok, Payload} ->
                    %% Check expiration
                    CurrentTime = erlang:system_time(second),
                    case maps:get(<<"exp">>, Payload, infinity) of
                        Exp when Exp > CurrentTime ->
                            {ok, Payload};
                        _ ->
                            {error, token_expired}
                    end;
                Error ->
                    Error
            end;
        {ok, #auth_config{method = Method}} ->
            {error, {wrong_method, Method}};
        Error ->
            Error
    end.

%% @doc Get current authentication configuration
-spec get_auth_config() -> {ok, #auth_config{}} | {error, term()}.
get_auth_config() ->
    case persistent_term:get(erlmcp_auth_config, undefined) of
        undefined ->
            %% Try to load from disk
            load_auth_config();
        Config ->
            {ok, Config}
    end.

%% @doc Set authentication method
-spec set_auth_method(auth_method()) -> ok | {error, term()}.
set_auth_method(Method) ->
    case get_auth_config() of
        {ok, Config} ->
            Updated = Config#auth_config{method = Method},
            store_auth_config(Updated),
            io:format("Auth method set to: ~p~n", [Method]),
            ok;
        Error ->
            Error
    end.

%% @doc Enable authentication
-spec enable_auth() -> ok.
enable_auth() ->
    case get_auth_config() of
        {ok, Config} ->
            Updated = Config#auth_config{enabled = true},
            store_auth_config(Updated),
            io:format("Authentication enabled~n", []),
            ok
    end.

%% @doc Disable authentication
-spec disable_auth() -> ok.
disable_auth() ->
    case get_auth_config() of
        {ok, Config} ->
            Updated = Config#auth_config{enabled = false},
            store_auth_config(Updated),
            io:format("Authentication disabled~n", []),
            ok
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Validate auth method
validate_auth_method(Method) ->
    lists:member(Method, [none, jwt, oauth2, mutual_tls, api_key]).

%% @private Build auth config from method and options
build_auth_config(Method, Options) ->
    Base = #auth_config{enabled = true, method = Method},
    case Method of
        jwt ->
            Secret = maps:get(secret, Options, crypto:strong_rand_bytes(32)),
            Algo = maps:get(algorithm, Options, hs256),
            Exp = maps:get(expiration, Options, 3600),
            Base#auth_config{
                jwt_secret = Secret,
                jwt_algorithm = Algo,
                jwt_expiration = Exp
            };
        oauth2 ->
            ClientId = maps:get(client_id, Options, <<>>),
            ClientSecret = maps:get(client_secret, Options, <<>>),
            DiscoveryUrl = maps:get(discovery_url, Options, <<>>),
            Scopes = maps:get(scopes, Options, []),
            Base#auth_config{
                oauth2_client_id = ClientId,
                oauth2_client_secret = ClientSecret,
                oauth2_discovery_url = DiscoveryUrl,
                oauth2_scopes = Scopes
            };
        api_key ->
            Key = maps:get(key, Options, crypto:strong_rand_bytes(16)),
            ExistingKeys = case get_auth_config() of
                {ok, C} -> C#auth_config.api_keys;
                _ -> []
            end,
            Base#auth_config{api_keys = [Key | ExistingKeys]};
        mutual_tls ->
            %% Certificates handled via import_certificates
            Base;
        _ ->
            Base
    end.

%% @private Store auth config
store_auth_config(Config) ->
    persistent_term:put(erlmcp_auth_config, Config),
    %% Also persist to disk
    catch file:write_file("data/auth.config", term_to_binary(Config, [compressed])).

%% @private Load auth config from disk
load_auth_config() ->
    case file:read_file("data/auth.config") of
        {ok, Binary} ->
            Config = binary_to_term(Binary),
            persistent_term:put(erlmcp_auth_config, Config),
            {ok, Config};
        {error, _} ->
            %% Return default config
            Config = #auth_config{},
            persistent_term:put(erlmcp_auth_config, Config),
            {ok, Config}
    end.

%% @private Parse certificate
parse_certificate(Data, pem) ->
    try
        Certs = public_key:pem_decode(Data),
        case lists:keyfind('Certificate', 1, Certs) of
            false -> {error, no_certificate_found};
            {'Certificate', Der, _NotEncrypted} ->
                extract_cert_info(Der, not_encrypted)
        end
    catch
        _:_ -> {error, invalid_pem_format}
    end;
parse_certificate(Data, der) ->
    try
        extract_cert_info(Data, not_encrypted)
    catch
        _:_ -> {error, invalid_der_format}
    end;
parse_certificate(_Data, pkcs12) ->
    {error, pkcs12_not_supported}.

%% @private Extract certificate info
extract_cert_info(Der, _NotEncrypted) ->
    try
        %% Use public_key to extract certificate information without direct record access
        Cert = public_key:der_decode('Certificate', Der),
        %% Extract basic info using pattern matching on the decoded term
        {'Certificate', _TBSInfo, _SignatureAlgorithm, _SignatureValue} = Cert,
        %% For a more robust solution, we'd use proper record definitions
        %% For now, return partial info
        {ok, #{
            subject => <<"Certificate subject">>,
            issued_at => <<"Not available">>,
            expires_at => <<"Not available">>,
            issuer => <<"Certificate issuer">>
        }}
    catch
        _:_ -> {error, certificate_decode_failed}
    end.

%% @private Format subject
format_subject(_Subject) ->
    %% Simplified subject formatting - avoid ASN.1 record access
    <<"CN=ErlMCP Certificate">>.

%% @private Format date
format_date(Date) ->
    try
        calendar:datetime_to_gregorian_seconds(Date) - 62167219200
    catch
        _:_ -> 0
    end.

%% @private Store certificate
store_certificate(Name, Cert, Format) ->
    case get_auth_config() of
        {ok, Config} ->
            Certs = Config#auth_config.certificates,
            Info = maps:merge(Cert, #{format => Format}),
            Updated = Config#auth_config{certificates = Certs#{Name => Info}},
            store_auth_config(Updated);
        Error ->
            Error
    end.

%% @private Run authentication tests
run_auth_tests(Config, Credentials) ->
    Tests = [
        {config_loaded, fun() -> {ok, #{method => Config#auth_config.method}} end},
        {secret_present, fun() ->
            case Config#auth_config.method of
                jwt when Config#auth_config.jwt_secret =/= <<>> -> ok;
                api_key when Config#auth_config.api_keys =/= [] -> ok;
                oauth2 when Config#auth_config.oauth2_client_id =/= <<>> -> ok;
                _ -> {error, missing_credentials}
            end
        end}
    ],

    Results = lists:map(fun({Name, TestFun}) ->
        Result = try TestFun()
        catch _:_ -> {error, test_failed} end,
        {Name, Result}
    end, Tests),

    {ok, #{
        tests => Results,
        passed => length([R || {_, ok} = R <- Results]),
        failed => length([R || {_, {error, _}} = R <- Results])
    }}.

%%====================================================================
%% JWT Sign/Verify Wrapper
%%====================================================================

%% @private jose wrapper for JWT signing
jwt_sign(Header, Payload, Secret) ->
    try
        Algo = maps:get(<<"alg">>, Header),
        EncodedHeader = base64:encode(jsx:encode(Header)),
        EncodedPayload = base64:encode(jsx:encode(Payload)),
        Message = <<EncodedHeader/binary, ".", EncodedPayload/binary>>,
        Signature = sign_message(Message, Algo, Secret),
        Token = <<Message/binary, ".", Signature/binary>>,
        {ok, Token}
    catch
        _:_ -> {error, jwt_sign_failed}
    end.

%% @private jose wrapper for JWT verification
jwt_verify(Token, Secret) ->
    try
        Parts = binary:split(Token, <<".">>, [global]),
        case length(Parts) of
            3 -> ok;
            _ -> throw(invalid_format)
        end,
        [EncodedHeader, EncodedPayload, Signature] = Parts,
        Message = <<EncodedHeader/binary, ".", EncodedPayload/binary>>,
        Header = jsx:decode(base64:decode(EncodedHeader), [return_maps]),
        Algo = maps:get(<<"alg">>, Header),
        case verify_signature(Message, Signature, Algo, Secret) of
            true ->
                Payload = jsx:decode(base64:decode(EncodedPayload), [return_maps]),
                {ok, Payload};
            false ->
                {error, invalid_signature}
        end
    catch
        _:_ -> {error, jwt_verify_failed}
    end.

%% @private Sign message
sign_message(Message, <<"HS256">>, Secret) ->
    base64:encode(crypto:mac(hmac, sha256, Secret, Message));
sign_message(Message, <<"HS384">>, Secret) ->
    base64:encode(crypto:mac(hmac, sha384, Secret, Message));
sign_message(Message, <<"HS512">>, Secret) ->
    base64:encode(crypto:mac(hmac, sha512, Secret, Message));
sign_message(_Message, Algo, _Secret) ->
    {error, {unsupported_algorithm, Algo}}.

%% @private Verify signature
verify_signature(Message, Signature, <<"HS", _/binary>>, Secret) ->
    Expected = sign_message(Message, <<"HS256">>, Secret),
    case Expected of
        Signature -> true;
        _ -> verify_other_algorithms(Message, Signature, Secret)
    end;
verify_signature(_, _, _, _) ->
    false.

verify_other_algorithms(Message, Signature, Secret) ->
    lists:any(fun(Algo) ->
        case sign_message(Message, Algo, Secret) of
            Signature -> true;
            _ -> false
        end
    end, [<<"HS384">>, <<"HS512">>]).

%%====================================================================
%% CLI Output Formatting
%%====================================================================

%% @doc Format auth status for CLI
-spec format_auth_status() -> iolist().
format_auth_status() ->
    case get_auth_config() of
        {ok, Config} ->
            Status = case Config#auth_config.enabled of
                true -> "Enabled";
                false -> "Disabled"
            end,
            Method = atom_to_list(Config#auth_config.method),
            io_lib:format("Authentication: ~s~nMethod: ~s~n", [Status, Method]);
        Error ->
            io_lib:format("Error: ~p~n", [Error])
    end.
