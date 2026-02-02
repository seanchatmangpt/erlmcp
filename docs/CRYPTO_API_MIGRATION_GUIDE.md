# Crypto API Migration Guide: OTP 28

## Overview

This guide provides specific migration instructions for erlmcp to handle the crypto API changes in OTP 28. The most significant changes are the deprecation of `crypto:enable_fips_mode/1`, `crypto:start/0`, and `crypto:stop/0` functions.

## Changes Summary

### OTP 28 Deprecations
- `crypto:enable_fips_mode/1` → Use configuration parameter
- `crypto:start/0` → Use `application:start(crypto)`
- `crypto:stop/0` → Use `application:stop(crypto)`

### Impact Assessment
- **Severity**: Medium (breaking changes in OTP 28)
- **Compatibility**: Works with OTP 26-27, requires changes for OTP 28
- **Migration Path**: Gradual with backward compatibility

## Migration Strategy

### Phase 1: Audit Current Crypto Usage

#### 1.1 Find Crypto API Usage
```bash
# Find all crypto usage in the codebase
grep -r "crypto:" apps/erlmcp_core/src/
grep -r "crypto:" apps/erlmcp_transports/src/
grep -r "crypto:" apps/erlmcp_observability/src/
```

#### 1.2 Identify Specific Functions to Replace
```erlang
% Search for deprecated functions:
grep -n "crypto:start\|crypto:stop\|crypto:enable_fips_mode" apps/**/*.erl
```

### Phase 2: Implement Application-Based Crypto Control

#### 2.1 Create Crypto Management Module
```erlang
% apps/erlmcp_core/src/erlmcp_crypto.erl
-module(erlmcp_crypto).

-behaviour(gen_server).

%% API exports
-export([start_link/0, stop/0, is_fips_enabled/0, enable_fips/1, disable_fips/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the crypto server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the crypto server
-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

%% @doc Check if FIPS mode is enabled
-spec is_fips_enabled() -> boolean().
is_fips_enabled() ->
    gen_server:call(?MODULE, is_fips_enabled).

%% @doc Enable FIPS mode
-spec enable_fips(boolean()) -> ok | {error, term()}.
enable_fips(Enabled) ->
    gen_server:call(?MODULE, {enable_fips, Enabled}).

%% @doc Disable FIPS mode
-spec disable_fips() -> ok.
disable_fips() ->
    gen_server:call(?MODULE, disable_fips).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Start crypto application
    case application:start(crypto) of
        ok ->
            State = #{fips_enabled => get_fips_from_config()},
            {ok, State};
        {error, {already_started, _}} ->
            %% Already started
            State = #{fips_enabled => get_fips_from_config()},
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(stop, _From, State) ->
    application:stop(crypto),
    {reply, ok, State};

handle_call(is_fips_enabled, _From, State) ->
    FipsEnabled = get_fips_from_config(),
    {reply, FipsEnabled, State};

handle_call({enable_fips, Enabled}, _From, State) ->
    Result = set_fips_config(Enabled),
    NewState = State#{fips_enabled => Enabled},
    {reply, Result, NewState};

handle_call(disable_fips, _From, State) ->
    Result = set_fips_config(false),
    NewState = State#{fips_enabled => false},
    {reply, Result, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    application:stop(crypto).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Get FIPS mode from configuration
-spec get_fips_from_config() -> boolean().
get_fips_from_config() ->
    case application:get_env(crypto, fips_mode) of
        {ok, true} -> true;
        {ok, false} -> false;
        undefined -> false
    end.

%% Set FIPS configuration
-spec set_fips_config(boolean()) -> ok | {error, term()}.
set_fips_config(true) ->
    case application:set_env(crypto, fips_mode, true) of
        ok ->
            %% For OTP 28+, this should automatically enable FIPS mode
            %% For older versions, we need to call deprecated function as fallback
            case erlang:function_exported(crypto, enable_fips_mode, 1) of
                true ->
                    crypto:enable_fips_mode(true);
                false ->
                    ok
            end;
        Error ->
            Error
    end;
set_fips_config(false) ->
    case application:set_env(crypto, fips_mode, false) of
        ok ->
            ok;
        Error ->
            Error
    end.
```

#### 2.2 Update Crypto Usage in Application
```erlang
% Replace crypto:start() calls
crypto:start() ->
    erlmcp_crypto:start_link().

% Replace crypto:stop() calls
crypto:stop() ->
    erlmcp_crypto:stop().

% Replace crypto:enable_fips_mode/1 calls
crypto:enable_fips_mode(Enabled) ->
    erlmcp_crypto:enable_fips(Enabled).
```

### Phase 3: Configuration Updates

#### 3.1 Update sys.config
```erlang
% Before OTP 28 configuration:
{crypto, [
    {fips_mode, false}
]}.

% Updated configuration for OTP 28+:
{crypto, [
    {fips_mode, false}
]}.

% Additional configuration for application-based control:
{erlmcp_core, [
    {crypto_management, true},
    {crypto_timeout, 5000}
]}.
```

#### 3.2 Update relx.config or release configuration
```erlang
% Ensure crypto application is included in release
{release, {erlmcp, "1.0.0"},
    [
       erts,
        kernel,
        stdlib,
        crypto,      % Include crypto application
        sasl,
        erlmcp_core,
        erlmcp_transports,
        erlmcp_observability
    ],
    []}.
```

### Phase 4: Fallback Implementation

#### 4.1 Version-Aware Crypto Functions
```erlang
% apps/erlmcp_core/src/erlmcp_crypto_compat.erl
-module(erlmcp_crypto_compat).

%% Export deprecated functions with version-aware implementation
-export([enable_fips_mode/1, start/0, stop/0]).

%% @doc Deprecated: Use application configuration instead
-spec enable_fips_mode(boolean()) -> ok | {error, term()}.
enable_fips_mode(Enabled) ->
    %% Log deprecation warning
    logger:warning("crypto:enable_fips_mode/1 is deprecated in OTP 28. Use application configuration instead."),

    case application:get_env(crypto, fips_mode) of
        undefined ->
            %% Set application environment
            application:set_env(crypto, fips_mode, Enabled),
            ok;
        {ok, Current} when Current =:= Enabled ->
            %% Already set
            ok;
        {ok, _} ->
            %% Update existing setting
            application:set_env(crypto, fips_mode, Enabled),
            ok
    end.

%% @doc Deprecated: Use application:start(crypto) instead
-spec start() -> ok | {error, term()}.
start() ->
    logger:warning("crypto:start() is deprecated in OTP 28. Use application:start(crypto) instead."),
    application:start(crypto).

%% @doc Deprecated: Use application:stop(crypto) instead
-spec stop() -> ok.
stop() ->
    logger:warning("crypto:stop() is deprecated in OTP 28. Use application:stop(crypto) instead."),
    application:stop(crypto).
```

#### 4.2 Migration Helper Functions
```erlang
% Add to erlmcp_crypto module
-export([migrate_crypto_configuration/0]).

%% @doc Migrate old crypto configuration to new format
-spec migrate_crypto_configuration() -> ok.
migrate_crypto_configuration() ->
    %% Check for old configuration patterns
    case application:get_env(crypto, fips_mode) of
        undefined ->
            %% Try to get from old function calls
            case get_legacy_fips_config() of
                {ok, FipsEnabled} ->
                    application:set_env(crypto, fips_mode, FipsEnabled),
                    logger:info("Migrated FIPS configuration to new format"),
                    ok;
                not_found ->
                    ok
            end;
        _ ->
            %% Already using new format
            ok
    end.

%% Get legacy configuration (if any)
-spec get_legacy_fips_config() -> {ok, boolean()} | not_found.
get_legacy_fips_config() ->
    %% Check for old configuration files or environment variables
    case os:getenv("ERLMCP_CRYPTO_FIPS") of
        false ->
            not_found;
        "true" ->
            {ok, true};
        "false" ->
            {ok, false};
        _ ->
            not_found
    end.
```

### Phase 5: Testing and Validation

#### 5.1 Crypto Testing Suite
```erlang
% Add to test suite
-crypto_test(crypto_start_stop) ->
    %% Test crypto application start/stop
    erlmcp_crypto:start(),
    true = erlmcp_crypto:is_fips_enabled(),
    erlmcp_crypto:stop(),
    ok.

-crypto_test(fips_mode_enable_disable) ->
    %% Test FIPS mode changes
    erlmcp_crypto:enable_fips(true),
    true = erlmcp_crypto:is_fips_enabled(),
    erlmcp_crypto:disable_fips(),
    false = erlmcp_crypto:is_fips_enabled(),
    ok.

-crypto_test(crypto_compatibility) ->
    %% Test backward compatibility
    erlmcp_crypto_compat:start(),
    erlmcp_crypto_compat:stop(),
    ok.
```

#### 5.2 Cross-Version Testing
```bash
# Test different OTP versions
# OTP 26.x - Old crypto API works
# OTP 27.x - Old API works, warnings logged
# OTP 28.x - New API required, old functions deprecated

# Test scenarios:
1. Start/stop crypto application
2. Enable/disable FIPS mode
3. Configuration migration
4. Error handling and fallbacks
```

### Phase 6: Documentation Updates

#### 6.1 API Documentation
```erlang
%% Update documentation
%% @doc Start crypto application (deprecated in OTP 28)
%%
%% Use application:start(crypto) instead in OTP 28+.
%% This function is provided for backward compatibility.
-spec start() -> ok | {error, term()}.
start() ->
    erlmcp_crypto_compat:start().

%% @doc Enable FIPS mode (deprecated in OTP 28)
%%
%% Use application configuration instead in OTP 28+.
%% Set crypto:fips_mode = true in sys.config.
-spec enable_fips_mode(boolean()) -> ok | {error, term()}.
enable_fips_mode(Enabled) ->
    erlmcp_crypto_compat:enable_fips_mode(Enabled).
```

## Implementation Checklist

### Immediate Actions (High Priority)
- [ ] Audit crypto API usage in codebase
- [ ] Create erlmcp_crypto module for application-based control
- [ ] Implement deprecation warnings for old functions
- [ ] Update configuration files for OTP 28

### Medium-term Actions
- [ ] Implement fallback mechanisms for older OTP versions
- [ ] Add comprehensive testing
- [ ] Update documentation
- [ ] Create migration guide

### Long-term Actions
- [ ] Remove deprecated function wrappers
- [ ] Update CI/CD to test against OTP 28
- [ ] Create performance benchmarks

## Monitoring and Logging

### Crypto Monitoring
```erlang
%% Add crypto monitoring
-export([start_crypto_monitoring/0]).

start_crypto_monitoring() ->
    %% Monitor crypto application state
    erlang:send_after(60000, self(), {check_crypto_state}),
    ok.

handle_info({check_crypto_state}, State) ->
    %% Check crypto application state
    case application:get_env(crypto, fips_mode) of
        {ok, FipsEnabled} ->
            logger:info("Crypto state: FIPS ~p, Running ~p",
                       [FipsEnabled, is_crypto_running()]);
        undefined ->
            logger:info("Crypto state: No FIPS config, Running ~p",
                       [is_crypto_running()])
    end,

    erlang:send_after(60000, self(), {check_crypto_state}),
    {noreply, State}.

is_crypto_running() ->
    case application:get_env(crypto, application_controller) of
        undefined -> false;
        {ok, _} -> true
    end.
```

## Conclusion

The crypto API changes in OTP 28 are manageable with proper planning:

1. **Application-based control**: Use `application:start/stop(crypto)` instead of direct functions
2. **Configuration-based FIPS**: Set `fips_mode` in configuration instead of runtime calls
3. **Backward compatibility**: Provide wrappers with deprecation warnings
4. **Gradual migration**: Allow existing code to work while encouraging new patterns

The key is to maintain backward compatibility while encouraging the use of new patterns for future OTP versions.

## Resources

- [OTP 28 Crypto Deprecations](https://www.erlang.org/doc/deprecations.html)
- [Crypto Application Documentation](https://www.erlang.org/doc/apps/crypto/)
- [Application Management](https://www.erlang.org/doc/apps/kernel/application.html)