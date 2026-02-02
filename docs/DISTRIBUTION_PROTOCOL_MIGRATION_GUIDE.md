# Distribution Protocol Migration Guide: OTP 26-28

## Overview

This guide provides specific migration instructions for erlmcp to handle the distribution protocol changes across OTP 26-28. The most critical change is the removal of the old link protocol in OTP 26 and the deprecation of `ALIAS_SEND` control messages in OTP 28.

## Critical Changes Summary

### OTP 26: Old Link Protocol Removed
- **Impact**: Nodes will refuse to connect without implementing new link protocol
- **Action**: Remove old protocol fallbacks, ensure new protocol compliance

### OTP 28: ALIAS_SEND Deprecated
- **Impact**: Control messages deprecated, scheduled for removal in OTP 30
- **Action**: Update to use `ALTACT_SIG_SEND` control message

## Migration Strategy

### Phase 1: Audit Current Distribution Implementation

#### 1.1 Identify Distribution Dependencies
```erlang
% Find all files using distribution protocols:
grep -r "distribution\|link\|protocol" apps/erlmcp_core/src/
grep -r "ALIAS_SEND\|ALTACT_SIG" apps/
grep -r "distribution_flag\|DFLAG_" apps/
```

#### 1.2 Check Distribution Registry Implementation
```erlang
% Current implementation in erlmcp_distribution_registry.erl:
enable_distribution_protocol_optimization() ->
    %% Setup enhanced distribution protocol if available
    logger:debug("Distribution protocol optimization enabled"),
    ok.

% NEEDS UPDATE to handle protocol version detection and fallback
```

### Phase 2: Implement Protocol Version Detection

#### 2.1 Add Protocol Version Detection
```erlang
%% Add to erlmcp_distribution_registry.erl
-export([get_distribution_protocol_version/0]).

%% @doc Detect current distribution protocol version
-spec get_distribution_protocol_version() -> atom().
get_distribution_protocol_version() ->
    case erlang:system_info(distribution_version) of
        Version when Version >= 4 ->
            %% OTP 26+ with large node container support
            new_protocol;
        Version when Version >= 3 ->
            %% OTP 24-25 with enhanced distribution
            enhanced_protocol;
        _ ->
            %% Legacy protocol (OTP 23 and earlier)
            legacy_protocol
    end.

%% @doc Check if new link protocol is supported
-spec supports_new_link_protocol() -> boolean().
supports_new_link_protocol() ->
    case get_distribution_protocol_version() of
        new_protocol -> true;
        enhanced_protocol -> true;
        legacy_protocol -> false
    end.
```

#### 2.2 Update Protocol Setup Logic
```erlang
%% Update distribution protocol optimization
-spec enable_distribution_protocol_optimization() -> ok.
enable_distribution_protocol_optimization() ->
    ProtocolVersion = get_distribution_protocol_version(),
    case ProtocolVersion of
        new_protocol ->
            %% OTP 26+ - new link protocol mandatory
            logger:debug("New distribution protocol enabled (OTP 26+)"),
            setup_new_protocol_features();
        enhanced_protocol ->
            %% OTP 24-25 - enhanced protocol
            logger:debug("Enhanced distribution protocol enabled (OTP 24-25)"),
            setup_enhanced_protocol_features();
        legacy_protocol ->
            %% OTP 23 and earlier - old protocol
            logger:warning("Legacy distribution protocol detected"),
            setup_legacy_protocol_features()
    end.

%% Setup new protocol features for OTP 26+
-spec setup_new_protocol_features() -> ok.
setup_new_protocol_features() ->
    %% Configure for new link protocol
    %% No need for legacy protocol handling
    ok.

%% Setup enhanced protocol features for OTP 24-25
-spec setup_enhanced_protocol_features() -> ok.
setup_enhanced_protocol_features() ->
    %% Configure for enhanced distribution
    %% Include fallback for new protocol
    ok.

%% Setup legacy protocol features for OTP 23 and earlier
-spec setup_legacy_protocol_features() -> ok.
setup_legacy_protocol_features() ->
    %% Configure for old link protocol
    %% Only for backward compatibility with very old systems
    ok.
```

### Phase 3: Handle Control Message Migration (OTP 28)

#### 3.1 Add Control Message Version Detection
```erlang
%% Add to erlmcp_distribution_registry.erl
-export([get_control_message_support/0]).

%% @doc Check supported control message types
-spec get_control_message_support() -> map().
get_control_message_support() ->
    %% Check distribution flags
    Flags = erlang:system_info(distribution_flags),
    #{
        alias_send => lists:member(DFLAG_ALIAS, Flags),
        altact_sig_send => lists:member(DFLAG_ALTACT_SIG, Flags),
        version => get_distribution_protocol_version()
    }.
```

#### 3.2 Implement Control Message Fallback
```erlang
%% Control message sending with fallback
-spec send_control_message(atom(), term()) -> ok.
send_control_message(Type, Message) ->
    Support = get_control_message_support(),
    case Support of
        #{altact_sig_send := true} ->
            %% OTP 28+ - use ALTACT_SIG_SEND
            send_altact_sig_message(Type, Message);
        #{alias_send := true} ->
            %% OTP 27 with legacy support
            send_alias_message(Type, Message);
        _ ->
            %% Fallback for older versions
            send_legacy_control_message(Type, Message)
    end.

%% Send using ALTACT_SIG_SEND (OTP 28+)
-spec send_altact_sig_message(atom(), term()) -> ok.
send_altact_sig_message(Type, Message) ->
    %% Implementation for new control message
    logger:debug("Sending ALTACT_SIG control message: ~p", [Type]),
    ok.

%% Send using ALIAS_SEND (legacy)
-spec send_alias_message(atom(), term()) -> ok.
send_alias_message(Type, Message) ->
    %% Implementation for deprecated control message
    logger:warning("Using deprecated ALIAS_SEND control message"),
    ok.

%% Send using legacy method
-spec send_legacy_control_message(atom(), term()) -> ok.
send_legacy_control_message(Type, Message) ->
    %% Implementation for very old systems
    logger:debug("Using legacy control message: ~p", [Type]),
    ok.
```

### Phase 4: Node Connection Strategy

#### 4.1 Enhanced Node Connection Logic
```erlang
%% Update node connection handling
-spec connect_node(node(), map()) -> ok | {error, term()}.
connect_node(Node, Options) ->
    %% Check protocol compatibility
    LocalProtocol = get_distribution_protocol_version(),
    RemoteProtocol = get_remote_protocol_version(Node),

    case protocol_compatible(LocalProtocol, RemoteProtocol) of
        true ->
            %% Protocols are compatible
            setup_node_connection(Node, Options);
        false ->
            %% Protocol mismatch
            {error, protocol_incompatible}
    end.

%% Check protocol compatibility
-spec protocol_compatible(atom(), atom()) -> boolean().
protocol_compatible(new_protocol, new_protocol) -> true;
protocol_compatible(new_protocol, enhanced_protocol) -> true;
protocol_compatible(enhanced_protocol, new_protocol) -> true;
protocol_compatible(enhanced_protocol, enhanced_protocol) -> true;
protocol_compatible(legacy_protocol, legacy_protocol) -> true;
protocol_compatible(_, _) -> false.

%% Get remote protocol version
-spec get_remote_protocol_version(node()) -> atom().
get_remote_protocol_version(Node) ->
    %% Implementation to query remote node
    %% This may require RPC or other mechanism
    new_protocol. %% Placeholder
```

### Phase 5: Testing and Validation

#### 5.1 Protocol Testing Suite
```erlang
%% Add to test suite
-protocol_test(otp_26_compatibility) ->
    %% Test OTP 26+ compatibility
    ProtocolVersion = get_distribution_protocol_version(),
    case ProtocolVersion of
        new_protocol ->
            %% Test new protocol features
            test_new_protocol_features();
        _ ->
            skip
    end.

%% Test control message compatibility
-protocol_test(control_message_compatibility) ->
    Support = get_control_message_support(),
    case Support of
        #{altact_sig_send := true} ->
            test_altact_sig_messages();
        #{alias_send := true} ->
            test_alias_messages();
        _ ->
            test_legacy_control_messages()
    end.
```

#### 5.2 Cross-OTP Testing Matrix
```bash
# Test matrix for different OTP versions
# OTP 26.3.1 - New link protocol mandatory
# OTP 27.1.2 - Enhanced distribution with map support
# OTP 28.3.1 - ALTACT_SIG control messages

# Test scenarios:
1. Single node with different OTP versions
2. Multi-node with mixed OTP versions
3. Protocol negotiation between versions
4. Control message handling across versions
5. Fallback mechanisms
```

## Implementation Checklist

### Immediate Actions (High Priority)
- [ ] Audit current distribution protocol usage
- [ ] Implement protocol version detection
- [ ] Update distribution registry for OTP 26+
- [ ] Remove old link protocol fallbacks
- [ ] Add control message version detection

### Medium-term Actions
- [ ] Implement control message migration
- [ ] Update node connection logic
- [ ] Add comprehensive testing
- [ ] Update documentation

### Long-term Actions
- [ ] Plan for OTP 30 removal of ALIAS_SEND
- [ ] Remove deprecated control message support
- [ ] Optimize for latest protocol features

## Configuration Updates

### sys.config Updates
```erlang
% Add to sys.config for OTP 28+ compatibility
{kernel, [
    {distributed, [{erlmcp, ['node1@host', 'node2@host']}]},
    {sync_nodes_optional, ['node1@host', 'node2@host']},
    {net_ticktime, 60},
    % Enable new protocol features
    {distribution_protocol_version, 4},
    % Enable control message optimization
    {enable_altact_sig, true}
]}.
```

### rebar.config Updates
```erlang
% Ensure OTP version compatibility
{otp_vsn, "26.3.1"}. % Minimum supported version
{minimum_otp_vsn, "26.3.1"}. % Required minimum
```

## Monitoring and Logging

### Protocol Monitoring
```erlang
%% Add protocol monitoring
-export([start_protocol_monitoring/0]).

start_protocol_monitoring() ->
    %% Monitor protocol usage
    erlang:send_after(30000, self(), {check_protocol_health}),
    ok.

%% Handle protocol health checks
handle_info({check_protocol_health}, State) ->
    ProtocolVersion = get_distribution_protocol_version(),
    ControlSupport = get_control_message_support(),

    logger:info("Protocol Health Check: ~p, Control: ~p",
                [ProtocolVersion, ControlSupport]),

    erlang:send_after(30000, self(), {check_protocol_health}),
    {noreply, State}.
```

## Conclusion

The distribution protocol changes across OTP 26-28 require careful attention but are manageable with proper planning:

1. **OTP 26**: Focus on new link protocol compliance
2. **OTP 27**: Enhanced distribution with better error handling
3. **OTP 28**: Control message migration to ALTACT_SIG_SEND

The key is to implement protocol version detection and graceful fallback mechanisms while planning for eventual removal of deprecated features.

## Resources

- [Erlang Distribution Documentation](https://www.erlang.org/doc/apps/erts/distribution.html)
- [OTP Distribution Protocol Changes](https://www.erlang.org/doc/deprecations.html)
- [Control Message Documentation](https://www.erlang.org/doc/apps/kernel/dist_protocol.html)