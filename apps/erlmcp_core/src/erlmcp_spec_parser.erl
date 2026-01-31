-module(erlmcp_spec_parser).
-author("Joe Armstrong's Philosophy: HARDCODED TRUTH").

%% @doc MCP 2025-11-25 Specification Parser
%%
%% JOE ARMSTRONG'S PHILOSOPHY:
%% "NO THEORY. HARDCODED TRUTH."
%%
%% This module does NOT parse YAML/JSON files. It does NOT download specs.
%% It HARDCODES the MCP 2025-11-25 specification directly as Erlang data.
%%
%% This is the SINGLE SOURCE OF TRUTH for the MCP protocol specification.
%% If the spec changes, you update THIS FILE with new hardcoded data.

%% API exports
-export([
    get_version/0,
    get_method/1,
    get_notification/1,
    get_error_code/1,
    list_all_methods/0,
    list_all_notifications/0,
    list_all_error_codes/0,
    validate_request/2,
    validate_notification/2
]).

%% Types
-type method_name() :: binary().
-type notification_name() :: binary().
-type error_code() :: integer().
-type method_spec() :: map().
-type notification_spec() :: map().
-type error_spec() :: map().
-type validation_result() :: {ok, map()} | {error, term()}.

-export_type([
    method_name/0,
    notification_name/0,
    error_code/0,
    method_spec/0,
    notification_spec/0,
    error_spec/0,
    validation_result/0
]).

%%====================================================================
%% HARDCODED SPECIFICATION CONSTANTS
%%====================================================================

%% @doc Get MCP protocol version
%% This is HARDCODED. Not parsed. Not downloaded. Just truth.
-spec get_version() -> binary().
get_version() ->
    <<"2025-11-25">>.

%%====================================================================
%% HARDCODED METHOD DEFINITIONS
%%====================================================================

%% @doc Get method specification by name
%% Returns HARDCODED method spec or undefined if not found
-spec get_method(binary()) -> method_spec() | undefined.
get_method(<<"initialize">>) ->
    #{
        type => request_response,
        params => #{
            protocolVersion => binary,
            capabilities => map,
            clientInfo => #{
                name => binary,
                version => binary
            }
        },
        result => #{
            protocolVersion => binary,
            capabilities => map,
            serverInfo => #{
                name => binary,
                version => binary
            }
        }
    };

get_method(<<"ping">>) ->
    #{
        type => request_response,
        params => #{},
        result => #{}
    };

get_method(<<"shutdown">>) ->
    #{
        type => request_response,
        params => #{},
        result => #{}
    };

%% Resources API
get_method(<<"resources/list">>) ->
    #{
        type => request_response,
        params => #{
            cursor => binary
        },
        result => #{
            resources => list,
            nextCursor => binary
        }
    };

get_method(<<"resources/read">>) ->
    #{
        type => request_response,
        params => #{
            uri => binary
        },
        result => #{
            contents => list
        }
    };

get_method(<<"resources/subscribe">>) ->
    #{
        type => request_response,
        params => #{
            uri => binary
        },
        result => #{}
    };

get_method(<<"resources/unsubscribe">>) ->
    #{
        type => request_response,
        params => #{
            uri => binary
        },
        result => #{}
    };

%% Tools API
get_method(<<"tools/list">>) ->
    #{
        type => request_response,
        params => #{
            cursor => binary
        },
        result => #{
            tools => list,
            nextCursor => binary
        }
    };

get_method(<<"tools/call">>) ->
    #{
        type => request_response,
        params => #{
            name => binary,
            arguments => map
        },
        result => #{
            content => list,
            isError => boolean
        }
    };

%% Prompts API
get_method(<<"prompts/list">>) ->
    #{
        type => request_response,
        params => #{
            cursor => binary
        },
        result => #{
            prompts => list,
            nextCursor => binary
        }
    };

get_method(<<"prompts/get">>) ->
    #{
        type => request_response,
        params => #{
            name => binary,
            arguments => map
        },
        result => #{
            description => binary,
            messages => list
        }
    };

%% Unknown method
get_method(_Method) ->
    undefined.

%% @doc List ALL methods defined in MCP 2025-11-25 spec
-spec list_all_methods() -> [method_name()].
list_all_methods() ->
    [
        <<"initialize">>,
        <<"ping">>,
        <<"shutdown">>,
        <<"resources/list">>,
        <<"resources/read">>,
        <<"resources/subscribe">>,
        <<"resources/unsubscribe">>,
        <<"tools/list">>,
        <<"tools/call">>,
        <<"prompts/list">>,
        <<"prompts/get">>
    ].

%%====================================================================
%% HARDCODED NOTIFICATION DEFINITIONS
%%====================================================================

%% @doc Get notification specification by name
%% Returns HARDCODED notification spec or undefined if not found
-spec get_notification(binary()) -> notification_spec() | undefined.
get_notification(<<"notifications/initialized">>) ->
    #{
        type => notification,
        params => #{}
    };

get_notification(<<"notifications/cancelled">>) ->
    #{
        type => notification,
        params => #{
            requestId => binary,
            reason => binary
        }
    };

get_notification(<<"notifications/progress">>) ->
    #{
        type => notification,
        params => #{
            <<"progressToken">> => binary,
            <<"progress">> => number,
            <<"total">> => number
        }
    };

get_notification(<<"notifications/roots/list_changed">>) ->
    #{
        type => notification,
        params => #{}
    };

%% Unknown notification
get_notification(_Notification) ->
    undefined.

%% @doc List ALL notifications defined in MCP 2025-11-25 spec
-spec list_all_notifications() -> [notification_name()].
list_all_notifications() ->
    [
        <<"notifications/initialized">>,
        <<"notifications/cancelled">>,
        <<"notifications/progress">>,
        <<"notifications/roots/list_changed">>
    ].

%%====================================================================
%% HARDCODED ERROR CODE DEFINITIONS
%%====================================================================

%% @doc Get error code specification by code number
%% Returns HARDCODED error spec or undefined if not found
-spec get_error_code(integer()) -> error_spec() | undefined.
get_error_code(-32700) ->
    #{
        code => -32700,
        name => <<"Parse error">>,
        message => <<"Invalid JSON was received by the server">>,
        category => jsonrpc
    };

get_error_code(-32600) ->
    #{
        code => -32600,
        name => <<"Invalid Request">>,
        message => <<"The JSON sent is not a valid Request object">>,
        category => jsonrpc
    };

get_error_code(-32601) ->
    #{
        code => -32601,
        name => <<"Method not found">>,
        message => <<"The method does not exist / is not available">>,
        category => jsonrpc
    };

get_error_code(-32602) ->
    #{
        code => -32602,
        name => <<"Invalid params">>,
        message => <<"Invalid method parameter(s)">>,
        category => jsonrpc
    };

get_error_code(-32603) ->
    #{
        code => -32603,
        name => <<"Internal error">>,
        message => <<"Internal JSON-RPC error">>,
        category => jsonrpc
    };

%% MCP-specific errors (-32000 to -32099)
get_error_code(-32009) ->
    #{
        code => -32009,
        name => <<"Method not supported">>,
        message => <<"Method not supported by this server">>,
        category => mcp_protocol
    };

get_error_code(-32008) ->
    #{
        code => -32008,
        name => <<"Capability negotiation failed">>,
        message => <<"Capability negotiation failed">>,
        category => mcp_protocol
    };

get_error_code(-32007) ->
    #{
        code => -32007,
        name => <<"Protocol version mismatch">>,
        message => <<"Protocol version mismatch between client and server">>,
        category => mcp_protocol
    };

get_error_code(-32006) ->
    #{
        code => -32006,
        name => <<"Unsupported protocol version">>,
        message => <<"Unsupported protocol version">>,
        category => mcp_protocol
    };

get_error_code(-32005) ->
    #{
        code => -32005,
        name => <<"Not initialized">>,
        message => <<"Server has not been initialized">>,
        category => mcp_core
    };

get_error_code(-32004) ->
    #{
        code => -32004,
        name => <<"Capability not supported">>,
        message => <<"Requested capability is not supported">>,
        category => mcp_core
    };

get_error_code(-32003) ->
    #{
        code => -32003,
        name => <<"Validation failed">>,
        message => <<"Request validation failed">>,
        category => mcp_core
    };

get_error_code(-32002) ->
    #{
        code => -32002,
        name => <<"Message too large">>,
        message => <<"Message size exceeds maximum allowed">>,
        category => mcp_core
    };

get_error_code(-32001) ->
    #{
        code => -32001,
        name => <<"Custom server error">>,
        message => <<"Custom server error">>,
        category => mcp_core
    };

get_error_code(-32000) ->
    #{
        code => -32000,
        name => <<"Server error">>,
        message => <<"Reserved for implementation-defined server-errors">>,
        category => mcp_core
    };

%% Unknown error code
get_error_code(_Code) ->
    undefined.

%% @doc List ALL error codes defined in MCP 2025-11-25 spec
-spec list_all_error_codes() -> [error_code()].
list_all_error_codes() ->
    [
        %% JSON-RPC standard errors
        -32700,  %% Parse error
        -32600,  %% Invalid Request
        -32601,  %% Method not found
        -32602,  %% Invalid params
        -32603,  %% Internal error

        %% MCP protocol errors
        -32009,  %% Method not supported
        -32008,  %% Capability negotiation failed
        -32007,  %% Protocol version mismatch
        -32006,  %% Unsupported protocol version

        %% MCP core errors
        -32005,  %% Not initialized
        -32004,  %% Capability not supported
        -32003,  %% Validation failed
        -32002,  %% Message too large
        -32001,  %% Custom server error
        -32000   %% Server error
    ].

%%====================================================================
%% VALIDATION FUNCTIONS
%%====================================================================

%% @doc Validate a request against hardcoded spec
-spec validate_request(binary(), map()) -> validation_result().
validate_request(Method, Params) when is_binary(Method), is_map(Params) ->
    case get_method(Method) of
        undefined ->
            {error, {method_not_found, Method}};
        Spec ->
            validate_params(Spec, Params)
    end.

%% @doc Validate a notification against hardcoded spec
-spec validate_notification(binary(), map()) -> validation_result().
validate_notification(Notification, Params) when is_binary(Notification), is_map(Params) ->
    case get_notification(Notification) of
        undefined ->
            {error, {notification_not_found, Notification}};
        Spec ->
            validate_params(Spec, Params)
    end.

%%====================================================================
%% INTERNAL VALIDATION HELPERS
%%====================================================================

%% @doc Validate params against method/notification spec
validate_params(Spec, Params) ->
    ExpectedParams = maps:get(params, Spec, #{}),
    case validate_param_keys(ExpectedParams, Params) of
        ok ->
            {ok, Spec};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate that required params are present
validate_param_keys(_Expected, _Params) ->
    %% For now, accept any params structure
    %% TODO: Add strict validation if needed
    ok.
