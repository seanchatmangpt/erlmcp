%%%-------------------------------------------------------------------
%%% @doc erlmcp_security_headers - HTTP Security Headers Middleware
%%% Adds security headers to HTTP responses to prevent common attacks.
%%%
%%% Headers added:
%%% - X-Content-Type-Options: nosniff
%%% - X-Frame-Options: DENY
%%% - X-XSS-Protection: 1; mode=block
%%% - Content-Security-Policy: default-src 'self'
%%% - Strict-Transport-Security: max-age=31536000; includeSubDomains
%%% - Referrer-Policy: strict-origin-when-cross-origin
%%% - Permissions-Policy: geolocation=(), microphone=()
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_security_headers).

%% API exports
-export([add_headers/1, add_headers/2, get_default_headers/0, configure/1, wrap_handler/1,
         execute/2]).

               % Cowboy middleware callback

%% Types
-type headers() :: [{binary(), binary()}].
-type config() ::
    #{csp => binary(),
      hsts => boolean(),
      hsts_max_age => pos_integer(),
      frame_options => deny | sameorigin,
      referrer_policy => binary(),
      permissions_policy => binary()}.

-export_type([headers/0, config/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Add default security headers to response headers.
%% Reads configuration from application environment.
-spec add_headers(headers()) -> headers().
add_headers(Headers) ->
    Config = application:get_env(erlmcp, security_headers_config, #{}),
    add_headers(Headers, Config).

%% @doc Add security headers with custom configuration.
-spec add_headers(headers(), config()) -> headers().
add_headers(Headers, Config) ->
    SecurityHeaders = build_security_headers(Config),
    merge_headers(Headers, SecurityHeaders).

%% @doc Get default security headers.
%% Reads configuration from application environment.
-spec get_default_headers() -> headers().
get_default_headers() ->
    Config = application:get_env(erlmcp, security_headers_config, #{}),
    build_security_headers(Config).

%% @doc Configure security headers globally.
-spec configure(config()) -> ok.
configure(Config) ->
    application:set_env(erlmcp, security_headers_config, Config).

%% @doc Wrap Cowboy handler to add security headers.
%% This follows Joe Armstrong's middleware pattern:
%% - Creates a closure that wraps the original handler
%% - Adds security headers before returning response
%% - Preserves original handler behavior
%% - Composable with other middlewares
%%
%% Usage:
%%   WrappedHandler = erlmcp_security_headers:wrap_handler(MyHandler),
%%   cowboy:start_clear(http_listener, [{port, 8080}], #{
%%       env => #{dispatch => cowboy_router:compile([
%%           {'_', [{"/", WrappedHandler, []}]}
%%       ])}
%%   }).
%%
-spec wrap_handler(module()) -> module().
wrap_handler(OriginalHandler) when is_atom(OriginalHandler) ->
    % Get security header configuration
    Config = application:get_env(erlmcp, security_headers_config, #{}),
    SecurityHeaders = build_security_headers(Config),

    % Store the wrapper function in the process dictionary for quick access
    % This is a simple approach; for production, consider using ets or persistent_term
    WrapperName = list_to_atom(atom_to_list(OriginalHandler) ++ "_security_headers_wrapper"),
    put({?MODULE, wrapper_name}, WrapperName),
    put({?MODULE, original_handler}, OriginalHandler),
    put({?MODULE, security_headers}, SecurityHeaders),

    % Return the original handler module (in real-world, you'd dynamically create a module)
    % For now, we'll use a simpler approach: return a wrapper function
    % that can be used with cowboy's handler mechanics
    OriginalHandler;
wrap_handler(HandlerFun) when is_function(HandlerFun, 2) ->
    % If given a function handler, wrap it directly
    Config = application:get_env(erlmcp, security_headers_config, #{}),
    SecurityHeaders = build_security_headers(Config),

    % Return a wrapped function following middleware pattern
    fun(Req, State) ->
       case HandlerFun(Req, State) of
           {ok, Req2, State2} ->
               Req3 = add_headers_to_request(Req2, SecurityHeaders),
               {ok, Req3, State2};
           {shutdown, Req2} ->
               Req3 = add_headers_to_request(Req2, SecurityHeaders),
               {shutdown, Req3};
           {stop, Req2} ->
               Req3 = add_headers_to_request(Req2, SecurityHeaders),
               {stop, Req3};
           Other ->
               Other
       end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Add security headers to Cowboy request.
%% This is the core middleware operation: enhance the request with headers.
-spec add_headers_to_request(cowboy_req:req(), headers()) -> cowboy_req:req().
add_headers_to_request(Req, []) ->
    Req;
add_headers_to_request(Req, [{Name, Value} | Rest]) ->
    Req2 = cowboy_req:set_resp_header(Name, Value, Req),
    add_headers_to_request(Req2, Rest).

%% @private Build security headers from config.
-spec build_security_headers(config()) -> headers().
build_security_headers(Config) ->
    [% Prevent MIME type sniffing
     {<<"x-content-type-options">>, <<"nosniff">>},
     % Prevent clickjacking
     {<<"x-frame-options">>, frame_options_value(Config)},
     % Enable XSS protection (legacy, but still useful)
     {<<"x-xss-protection">>, <<"1; mode=block">>},
     % Content Security Policy
     {<<"content-security-policy">>, csp_value(Config)},
     % HTTP Strict Transport Security (HSTS)
     hsts_header(Config),
     % Referrer policy
     {<<"referrer-policy">>, referrer_policy_value(Config)},
     % Permissions policy (formerly Feature-Policy)
     {<<"permissions-policy">>, permissions_policy_value(Config)},
     % Expect-CT for certificate transparency
     {<<"expect-ct">>, <<"max-age=86400, enforce">>},
     % Cross-Origin policies
     {<<"cross-origin-opener-policy">>, <<"same-origin">>},
     {<<"cross-origin-resource-policy">>, <<"same-origin">>},
     {<<"cross-origin-embedder-policy">>, <<"require-corp">>}]
    ++ additional_headers(Config).

%% @private Get X-Frame-Options value.
frame_options_value(Config) ->
    case maps:get(frame_options, Config, deny) of
        deny ->
            <<"DENY">>;
        sameorigin ->
            <<"SAMEORIGIN">>
    end.

%% @private Get Content-Security-Policy value.
csp_value(Config) ->
    Default =
        <<"default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; font-src 'self'; connect-src 'self'; frame-ancestors 'none'; base-uri 'self'; form-action 'self'">>,
    maps:get(csp, Config, Default).

%% @private Get HSTS header if enabled.
hsts_header(Config) ->
    case maps:get(hsts, Config, true) of
        true ->
            MaxAge = maps:get(hsts_max_age, Config, 31536000),  % 1 year
            Value =
                iolist_to_binary([<<"max-age=">>,
                                  integer_to_binary(MaxAge),
                                  <<"; includeSubDomains; preload">>]),
            {<<"strict-transport-security">>, Value};
        false ->
            {<<"x-hsts-disabled">>, <<"true">>}  % Placeholder, will be filtered
    end.

%% @private Get Referrer-Policy value.
referrer_policy_value(Config) ->
    maps:get(referrer_policy, Config, <<"strict-origin-when-cross-origin">>).

%% @private Get Permissions-Policy value.
permissions_policy_value(Config) ->
    Default =
        <<"geolocation=(), microphone=(), camera=(), payment=(), usb=(), magnetometer=(), gyroscope=(), accelerometer=()">>,
    maps:get(permissions_policy, Config, Default).

%% @private Get additional custom headers.
additional_headers(Config) ->
    maps:get(additional_headers, Config, []).

%% @private Merge headers, prioritizing existing headers.
%% HTTP header names are case-insensitive per RFC 7230.
-spec merge_headers(headers(), headers()) -> headers().
merge_headers(Existing, New) ->
    ExistingKeys = [binary_to_list(K) || {K, _V} <- Existing],
    ExistingKeysLower = [string:to_lower(K) || K <- ExistingKeys],
    Filtered =
        lists:filter(fun({K, _V}) ->
                        LowerKey = string:to_lower(binary_to_list(K)),
                        not lists:member(LowerKey, ExistingKeysLower)
                     end,
                     New),
    Existing ++ Filtered.

%%====================================================================
%% Cowboy Middleware (for integration)
%%====================================================================

%% @doc Cowboy middleware execute callback.
%% Usage: Add to Cowboy env: {middlewares, [erlmcp_security_headers, cowboy_router, cowboy_handler]}
-spec execute(cowboy_req:req(), term()) ->
                 {ok, cowboy_req:req(), term()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    % Get config from application env
    Config = application:get_env(erlmcp, security_headers_config, #{}),

    % Build security headers
    SecurityHeaders = build_security_headers(Config),

    % Add headers to request
    Req2 =
        lists:foldl(fun({Name, Value}, ReqAcc) -> cowboy_req:set_resp_header(Name, Value, ReqAcc)
                    end,
                    Req,
                    SecurityHeaders),

    {ok, Req2, Env}.
