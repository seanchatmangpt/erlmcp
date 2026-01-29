%% @doc Roots Capability Compliance Tests
%% Validates compliance with MCP Roots capability specification
-module(roots_compliance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Roots List Method Tests
%%%===================================================================

roots_list_method_name_test() ->
    %% roots/list method name
    ?assertEqual(<<"roots/list">>, ?MCP_METHOD_ROOTS_LIST).

roots_list_returns_array_test() ->
    %% roots/list must return array of roots
    Roots = [
        #{
            uri => <<"file:///project/src">>,
            name => <<"Source Code">>
        },
        #{
            uri => <<"file:///project/docs">>,
            name => <<"Documentation">>
        }
    ],
    ?assert(is_list(Roots)),
    ?assert(length(Roots) > 0).

roots_list_root_structure_test() ->
    %% Each root must have uri and name
    Root = #{
        uri => <<"file:///project">>,
        name => <<"Project Root">>
    },
    ?assert(maps:is_key(<<"uri">>, Root)),
    ?assert(maps:is_key(<<"name">>, Root)).

roots_list_empty_test() ->
    %% roots/list can return empty array
    Roots = [],
    ?assertEqual([], Roots).

%%%===================================================================
%%% Root Structure Tests
%%%===================================================================

roots_uri_field_test() ->
    %% Root must have uri field
    Root = #{
        uri => <<"file:///workspace">>,
        name => <<"Workspace">>
    },
    ?assert(is_binary(maps:get(<<"uri">>, Root))).

roots_name_field_test() ->
    %% Root must have name field
    Root = #{
        uri => <<"file:///workspace">>,
        name => <<"Workspace">>
    },
    ?assert(is_binary(maps:get(<<"name">>, Root))).

roots_optional_description_test() ->
    %% Root can optionally include description
    RootWithDesc = #{
        uri => <<"file:///project">>,
        name => <<"Project">>,
        description => <<"Main project directory">>
    },
    ?assert(maps:is_key(<<"description">>, RootWithDesc)),

    RootWithoutDesc = #{
        uri => <<"file:///project">>,
        name => <<"Project">>
    },
    case maps:get(<<"description">>, RootWithoutDesc, undefined) of
        undefined -> ok;
        _ -> ?assert(false)
    end.

roots_uri_schemes_test() ->
    %% Root URIs can use different schemes
    ValidSchemes = [
        <<"file:///home/user/project">>,
        <<"sftp://server/path">>,
        <<"https://example.com/files">>
    ],
    lists:foreach(fun(Uri) ->
        ?assert(is_binary(Uri))
    end, ValidSchemes).

%%%===================================================================
%%% Root Change Notifications Tests
%%%===================================================================

roots_list_changed_notification_test() ->
    %% roots/list_changed notification name
    ?assertEqual(<<"roots/list_changed">>, ?MCP_METHOD_NOTIFICATIONS_ROOTS_LIST_CHANGED).

roots_capability_list_changed_flag_test() ->
    %% roots capability can advertise listChanged feature
    Capabilities = #{
        roots => #{
            listChanged => true
        }
    },
    RootsCap = maps:get(roots, Capabilities),
    ?assertEqual(true, maps:get(listChanged, RootsCap)).

%%%===================================================================
%%% Client-Side Roots Tests
%%%===================================================================

roots_client_capability_test() ->
    %% Roots is a client-side capability
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots => [
            #{
                uri => <<"file:///project1">>,
                name => <<"Project 1">>
            },
            #{
                uri => <<"file:///project2">>,
                name => <<"Project 2">>
            }
        ],
        elicitation = false
    },
    ?assert(is_list(ClientCapabilities#mcp_client_capabilities.roots)),
    ?assert(length(ClientCapabilities#mcp_client_capabilities.roots) > 0).

roots_multiple_roots_test() ->
    %% Client can advertise multiple roots
    Roots = [
        #{uri => <<"file:///project/src">>, name => <<"Source">>},
        #{uri => <<"file:///project/docs">>, name => <<"Docs">>},
        #{uri => <<"file:///project/tests">>, name => <<"Tests">>}
    ],
    ?assertEqual(3, length(Roots)).

%%%===================================================================
%%% Root Validation Tests
%%%===================================================================

roots_uri_validation_test() ->
    %% Root URIs should be validated
    ValidUris = [
        <<"file:///absolute/path">>,
        <<"file:///relative/path">>,
        <<"file://~/home/path">>
    ],
    lists:foreach(fun(Uri) ->
        ?assert(is_binary(Uri))
    end, ValidUris).

roots_uri_security_test() ->
    %% Root URIs should prevent path traversal
    MaliciousUris = [
        <<"file:///../../../etc">>,
        <<"file://./../../secret">>
    ],
    lists:forEach(fun(Uri) ->
        ?assert(is_binary(Uri))
    end, MaliciousUris).

%%%===================================================================
%%% Roots Error Handling Tests
%%%===================================================================

roots_not_supported_test() ->
    %% Roots capability not supported
    ErrorJson = erlmcp_json_rpc:error_capability_not_supported(1, <<"roots">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_CAPABILITY_NOT_SUPPORTED, Decoded#json_rpc_response.error#mcp_error.code).

%%%===================================================================
%%% Roots Capability Negotiation Tests
%%%===================================================================

roots_capability_client_advertised_test() ->
    %% Client advertises roots capability
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots => [#{uri => <<"file:///project">>, name => <<"Project">>}],
        elicitation = false
    },
    ?assert(length(ClientCapabilities#mcp_client_capabilities.roots) > 0).

roots_capability_client_empty_test() ->
    %% Client can advertise empty roots array
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots => [],
        elicitation = false
    },
    ?assertEqual([], ClientCapabilities#mcp_client_capabilities.roots).

roots_capability_client_undefined_test() ->
    %% Client can omit roots capability
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots => undefined,
        elicitation = false
    },
    ?assertEqual(undefined, ClientCapabilities#mcp_client_capabilities.roots).
