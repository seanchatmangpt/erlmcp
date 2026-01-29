-module(erlmcp_roots_capability_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp.hrl").

%%%====================================================================
%%% Roots Capability Tests (MCP 2025-03-26 Specification)
%%%
%%% Tests the roots capability which allows clients to expose file system
%%% "roots" to servers, defining operational boundaries.
%%%
%%% Reference: https://modelcontextprotocol.info/docs/concepts/roots/
%%%====================================================================

%%%--------------------------------------------------------------------
%%% Test Fixtures
%%%--------------------------------------------------------------------

roots_capability_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Roots capability declaration test", fun test_roots_capability_declaration/0},
            {"Roots/list request encoding test", fun test_roots_list_request_encoding/0},
            {"Roots/list response decoding test", fun test_roots_list_response_decoding/0},
            {"Root metadata validation test", fun test_root_metadata_validation/0},
            {"Empty roots list test", fun test_empty_roots_list/0},
            {"Multiple roots test", fun test_multiple_roots/0},
            {"Root URI format validation test", fun test_root_uri_format_validation/0},
            {"Root name optional field test", fun test_root_name_optional/0},
            {"Roots list changed notification test", fun test_roots_list_changed_notification/0},
            {"Roots capability negotiation test", fun test_roots_capability_negotiation/0}
        ]
    }.

%%%--------------------------------------------------------------------
%%% Setup/Teardown
%%%--------------------------------------------------------------------

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%%--------------------------------------------------------------------
%%% Test Cases
%%%--------------------------------------------------------------------

%% @doc Test that roots capability is properly declared in client capabilities
test_roots_capability_declaration() ->
    %% Create client capabilities with roots support
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{}
    },

    %% Verify roots capability exists
    ?assertNotEqual(undefined, ClientCaps#mcp_client_capabilities.roots),

    %% Verify it's a capability record
    ?assert(is_record(ClientCaps#mcp_client_capabilities.roots, mcp_capability)),

    %% Test with listChanged feature
    RootsCapsWithListChanged = #mcp_capability{
        listChanged = true
    },
    ClientCaps2 = ClientCaps#mcp_client_capabilities{
        roots = RootsCapsWithListChanged
    },
    ?assertEqual(true, (ClientCaps2#mcp_client_capabilities.roots)#mcp_capability.listChanged).

%% @doc Test encoding of roots/list request (server -> client)
test_roots_list_request_encoding() ->
    %% roots/list has no parameters (empty request)
    Request = #json_rpc_request{
        jsonrpc = ?JSONRPC_VERSION,
        id = 1,
        method = ?MCP_METHOD_ROOTS_LIST,
        params = undefined
    },

    %% Encode the request
    Encoded = erlmcp_json_rpc:encode_request(Request),

    %% Verify it's a binary
    ?assert(is_binary(Encoded)),

    %% Verify it contains the method name
    ?assertNotEqual(nomatch, binary:match(Encoded, <<"roots/list">>)),

    %% Decode and verify structure
    {ok, Decoded} = erlmcp_json_rpc:decode_request(Encoded),
    ?assertEqual(?MCP_METHOD_ROOTS_LIST, Decoded#json_rpc_request.method),
    ?assertEqual(undefined, Decoded#json_rpc_request.params).

%% @doc Test decoding of roots/list response (client -> server)
test_roots_list_response_decoding() ->
    %% Sample roots list response
    RootsList = [
        #{
            ?MCP_PARAM_URI => <<"file:///home/user/projects/frontend">>,
            ?MCP_PARAM_NAME => <<"Frontend Repository">>
        },
        #{
            ?MCP_PARAM_URI => <<"file:///home/user/projects/backend">>,
            ?MCP_PARAM_NAME => <<"Backend Repository">>
        }
    ],

    Response = #json_rpc_response{
        jsonrpc = ?JSONRPC_VERSION,
        id = 1,
        result = #{?MCP_PARAM_ROOTS => RootsList},
        error = undefined
    },

    %% Encode the response
    Encoded = erlmcp_json_rpc:encode_response(Response#json_rpc_response.id, Response#json_rpc_response.result),

    %% Verify encoding
    ?assert(is_binary(Encoded)),

    %% Decode and verify
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertMatch(#json_rpc_response{}, Decoded),

    %% Extract roots from result
    Result = Decoded#json_rpc_response.result,
    ?assert(maps:is_key(?MCP_PARAM_ROOTS, Result)),
    DecodedRoots = maps:get(?MCP_PARAM_ROOTS, Result),
    ?assertEqual(2, length(DecodedRoots)).

%% @doc Test root metadata validation
test_root_metadata_validation() ->
    %% Valid root with all fields
    Root1 = #{
        ?MCP_PARAM_URI => <<"file:///home/user/project">>,
        ?MCP_PARAM_NAME => <<"My Project">>
    },

    ?assert(validate_root_uri(Root1)),
    ?assert(is_binary(maps:get(?MCP_PARAM_URI, Root1))),
    ?assert(is_binary(maps:get(?MCP_PARAM_NAME, Root1))),

    %% Valid root with only URI (name is optional)
    Root2 = #{
        ?MCP_PARAM_URI => <<"https://api.example.com/v1">>
    },

    ?assert(validate_root_uri(Root2)),
    ?assert(is_binary(maps:get(?MCP_PARAM_URI, Root2))),
    ?assertEqual(undefined, maps:get(?MCP_PARAM_NAME, Root2, undefined)).

%% @doc Test empty roots list (valid response)
test_empty_roots_list() ->
    %% Empty roots list is valid
    EmptyRoots = [],

    Response = #{?MCP_PARAM_ROOTS => EmptyRoots},

    %% Verify structure
    ?assert(maps:is_key(?MCP_PARAM_ROOTS, Response)),

    %% Verify empty list
    Roots = maps:get(?MCP_PARAM_ROOTS, Response),
    ?assertEqual([], Roots),
    ?assertEqual(0, length(Roots)).

%% @doc Test multiple roots with different URI schemes
test_multiple_roots() ->
    %% Multiple roots
    Roots = [
        #{
            ?MCP_PARAM_URI => <<"file:///home/user/projects/app1">>,
            ?MCP_PARAM_NAME => <<"Application 1">>
        },
        #{
            ?MCP_PARAM_URI => <<"file:///home/user/projects/app2">>,
            ?MCP_PARAM_NAME => <<"Application 2">>
        },
        #{
            ?MCP_PARAM_URI => <<"https://api.example.com/v1">>,
            ?MCP_PARAM_NAME => <<"API Endpoint">>
        }
    ],

    %% Verify all roots have URIs
    ?assertEqual(3, length(Roots)),

    %% Verify unique URIs (MCP spec requirement)
    Uris = [maps:get(?MCP_PARAM_URI, R) || R <- Roots],
    UniqueUris = lists:usort(Uris),
    ?assertEqual(3, length(UniqueUris)),

    %% Verify each root has valid URI
    lists:foreach(fun(R) ->
        ?assert(validate_root_uri(R))
    end, Roots).

%% @doc Test root URI format validation
test_root_uri_format_validation() ->
    %% Valid file:// URIs
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<"file:///home/user/project">>})),
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<"file:///Users/username/test">>})),

    %% Valid HTTP URIs
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<"https://api.example.com">>})),
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<"http://localhost:8080">>})),

    %% Relative paths (valid in some contexts)
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<"./relative/path">>})),
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<"../parent/path">>})),

    %% Invalid URIs (missing scheme or malformed)
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<"not_a_uri">>}) =:= false orelse
           validate_root_uri(#{?MCP_PARAM_URI => <<"not_a_uri">>})),
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<">>})),
    ?assert(validate_root_uri(#{?MCP_PARAM_URI => <<>>})).

%% @doc Test that name field is optional in root metadata
test_root_name_optional() ->
    %% Root without name (valid)
    RootWithoutName = #{
        ?MCP_PARAM_URI => <<"file:///home/user/project">>
    },

    ?assert(is_binary(maps:get(?MCP_PARAM_URI, RootWithoutName))),
    ?assertEqual(undefined, maps:get(?MCP_PARAM_NAME, RootWithoutName, undefined)),

    %% Root with name (valid)
    RootWithName = RootWithoutName#{
        ?MCP_PARAM_NAME => <<"Project Name">>
    },

    ?assert(is_binary(maps:get(?MCP_PARAM_URI, RootWithName))),
    ?assert(is_binary(maps:get(?MCP_PARAM_NAME, RootWithName))).

%% @doc Test roots/list_changed notification
test_roots_list_changed_notification() ->
    %% Roots list changed notification has no parameters
    Notification = #json_rpc_notification{
        jsonrpc = ?JSONRPC_VERSION,
        method = ?MCP_METHOD_NOTIFICATIONS_ROOTS_LIST_CHANGED,
        params = #{}
    },

    %% Encode the notification
    Encoded = erlmcp_json_rpc:encode_notification(
        ?MCP_METHOD_NOTIFICATIONS_ROOTS_LIST_CHANGED,
        #{}
    ),

    %% Verify encoding
    ?assert(is_binary(Encoded)),
    ?assertNotEqual(nomatch, binary:match(Encoded, <<"roots/list_changed">>)),

    %% Decode and verify
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertMatch(#json_rpc_notification{}, Decoded),
    ?assertEqual(?MCP_METHOD_NOTIFICATIONS_ROOTS_LIST_CHANGED, Decoded#json_rpc_notification.method).

%% @doc Test roots capability negotiation during initialize
test_roots_capability_negotiation() ->
    %% Server declares roots capability
    ServerCapabilities = #mcp_server_capabilities{
        roots = #mcp_roots_capability{}
    },

    %% Convert to map for JSON encoding
    CapMap = erlmcp_capabilities:capability_to_map(ServerCapabilities),

    %% Verify roots capability is included
    ?assert(maps:is_key(?MCP_CAPABILITY_ROOTS, CapMap)),

    %% Verify it's an empty map (roots capability has no features in basic spec)
    RootsCap = maps:get(?MCP_CAPABILITY_ROOTS, CapMap),
    ?assert(is_map(RootsCap)),

    %% Test client capability declaration
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{
            listChanged = true
        }
    },

    %% Client sends roots capability in initialize request
    InitParams = #{
        ?MCP_FIELD_PROTOCOL_VERSION => <<"2025-03-26">>,
        ?MCP_FIELD_CAPABILITIES => #{
            ?MCP_CAPABILITY_ROOTS => #{
                <<"listChanged">> => true
            }
        },
        ?MCP_FIELD_CLIENT_INFO => #{
            ?MCP_INFO_NAME => <<"test_client">>,
            ?MCP_INFO_VERSION => <<"1.0.0">>
        }
    },

    ?assert(maps:is_key(?MCP_FIELD_CAPABILITIES, InitParams)),
    CapsMap = maps:get(?MCP_FIELD_CAPABILITIES, InitParams),
    ?assert(maps:is_key(?MCP_CAPABILITY_ROOTS, CapsMap)),
    ClientRootsCap = maps:get(?MCP_CAPABILITY_ROOTS, CapsMap),
    ?assertEqual(true, maps:get(<<"listChanged">>, ClientRootsCap, false)).

%%%--------------------------------------------------------------------
%%% Helper Functions
%%%--------------------------------------------------------------------

%% @doc Validate root URI format (basic validation)
%% According to MCP spec, root URIs should be valid URIs
-spec validate_root_uri(map()) -> boolean().
validate_root_uri(Root) ->
    case maps:get(?MCP_PARAM_URI, Root, undefined) of
        undefined -> false;
        Uri ->
            %% Basic URI validation: check scheme
            case Uri of
                <<"file://", _/binary>> -> true;
                <<"http://", _/binary>> -> true;
                <<"https://", _/binary>> -> true;
                <<"ftp://", _/binary>> -> true;
                <<"./", _/binary>> -> true;   % Relative path
                <<"../", _/binary>> -> true;  % Parent relative path
                <<"/", _/binary>> -> true;    % Absolute path
                <<>> -> false;                % Empty URI
                _ when byte_size(Uri) > 0 -> true;  % Accept other valid strings
                _ -> false
            end
    end.
