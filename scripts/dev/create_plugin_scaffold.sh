#!/bin/bash
# Plugin Scaffold Generator
# Usage: create_plugin_scaffold.sh <name> <type>

set -e

PLUGIN_NAME="$1"
PLUGIN_TYPE="$2"

if [ -z "$PLUGIN_NAME" ] || [ -z "$PLUGIN_TYPE" ]; then
    echo "Usage: $0 <name> <type>"
    echo "Types: validator, formatter, exporter, command, middleware"
    exit 1
fi

MODULE_NAME="erlmcp_plugin_${PLUGIN_NAME}"

case "$PLUGIN_TYPE" in
    validator)
        cat <<EOF
%%%-------------------------------------------------------------------
%%% @doc
%%% ${MODULE_NAME} - Custom validator plugin
%%%
%%% Add your custom validation logic here.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(${MODULE_NAME}).
-behaviour(erlmcp_plugin_validator).

%% Plugin behavior callbacks
-export([
    init/1,
    validate/2,
    get_schema/0,
    metadata/0,
    terminate/2
]).

-record(state, {
    %% Add your state fields here
}).

%%====================================================================
%% Plugin Behavior Callbacks
%%====================================================================

%% @doc Plugin metadata
metadata() ->
    #{
        name => <<"${PLUGIN_NAME}">>,
        version => <<"1.0.0">>,
        type => validator,
        description => <<"Custom validator plugin">>,
        author => <<"Your Name">>
    }.

%% @doc Initialize plugin
init(Opts) ->
    %% Initialize your plugin state
    {ok, #state{}}.

%% @doc Validate data
validate(Data, State) ->
    %% Add your validation logic here
    %% Return {ok, ValidationResult, NewState} or {error, Reason}
    Result = #{
        status => passed,
        details => #{}
    },
    {ok, Result, State}.

%% @doc Get validation schema
get_schema() ->
    %% Return JSON Schema for your validator
    #{
        type => object,
        properties => #{}
    }.

%% @doc Terminate plugin
terminate(_Reason, _State) ->
    ok.
EOF
        ;;

    formatter)
        cat <<EOF
%%%-------------------------------------------------------------------
%%% @doc
%%% ${MODULE_NAME} - Custom formatter plugin
%%%
%%% Add your custom formatting logic here.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(${MODULE_NAME}).
-behaviour(erlmcp_plugin_formatter).

%% Plugin behavior callbacks
-export([
    init/1,
    format/2,
    supports_format/0,
    metadata/0,
    terminate/2
]).

-record(state, {
    %% Add your state fields here
}).

%%====================================================================
%% Plugin Behavior Callbacks
%%====================================================================

%% @doc Plugin metadata
metadata() ->
    #{
        name => <<"${PLUGIN_NAME}">>,
        version => <<"1.0.0">>,
        type => formatter,
        description => <<"Custom formatter plugin">>,
        author => <<"Your Name">>
    }.

%% @doc Initialize plugin
init(Opts) ->
    {ok, #state{}}.

%% @doc Format data
format(Data, State) when is_map(Data) ->
    %% Add your formatting logic here
    FormattedData = format_data(Data),
    {ok, FormattedData, State}.

%% @doc Supported format
supports_format() ->
    %% Return the format name (e.g., csv, xml, custom)
    custom.

%% @doc Terminate plugin
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

format_data(Data) ->
    %% Implement your formatting logic
    jsx:encode(Data).
EOF
        ;;

    exporter)
        cat <<EOF
%%%-------------------------------------------------------------------
%%% @doc
%%% ${MODULE_NAME} - Custom exporter plugin
%%%
%%% Add your custom export logic here.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(${MODULE_NAME}).
-behaviour(erlmcp_plugin_exporter).

%% Plugin behavior callbacks
-export([
    init/1,
    export/2,
    get_config_schema/0,
    metadata/0,
    terminate/2
]).

-record(state, {
    %% Add your state fields here
    endpoint,
    credentials
}).

%%====================================================================
%% Plugin Behavior Callbacks
%%====================================================================

%% @doc Plugin metadata
metadata() ->
    #{
        name => <<"${PLUGIN_NAME}">>,
        version => <<"1.0.0">>,
        type => exporter,
        description => <<"Custom exporter plugin">>,
        author => <<"Your Name">>
    }.

%% @doc Initialize plugin
init(Opts) ->
    Endpoint = maps:get(endpoint, Opts, <<"http://localhost">>),
    Credentials = maps:get(credentials, Opts, #{}),
    {ok, #state{endpoint = Endpoint, credentials = Credentials}}.

%% @doc Export data
export(Data, State) ->
    %% Add your export logic here
    %% Example: Send to external API, write to file, etc.
    Result = #{
        status => success,
        exported_to => State#state.endpoint
    },
    {ok, Result, State}.

%% @doc Get configuration schema
get_config_schema() ->
    #{
        type => object,
        properties => #{
            endpoint => #{type => string},
            credentials => #{type => object}
        },
        required => [endpoint]
    }.

%% @doc Terminate plugin
terminate(_Reason, _State) ->
    ok.
EOF
        ;;

    command)
        cat <<EOF
%%%-------------------------------------------------------------------
%%% @doc
%%% ${MODULE_NAME} - Custom command plugin
%%%
%%% Add your custom command logic here.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(${MODULE_NAME}).
-behaviour(erlmcp_plugin_command).

%% Plugin behavior callbacks
-export([
    init/1,
    execute/2,
    help/0,
    metadata/0,
    terminate/2
]).

-record(state, {
    %% Add your state fields here
}).

%%====================================================================
%% Plugin Behavior Callbacks
%%====================================================================

%% @doc Plugin metadata
metadata() ->
    #{
        name => <<"${PLUGIN_NAME}">>,
        version => <<"1.0.0">>,
        type => command,
        description => <<"Custom command plugin">>,
        author => <<"Your Name">>
    }.

%% @doc Initialize plugin
init(Opts) ->
    {ok, #state{}}.

%% @doc Execute command
execute(Args, State) ->
    %% Add your command logic here
    %% Args is a list of command arguments
    Result = #{
        status => success,
        output => <<"Command executed">>
    },
    {ok, Result, State}.

%% @doc Help text
help() ->
    <<"${PLUGIN_NAME} <args> - Custom command">>.

%% @doc Terminate plugin
terminate(_Reason, _State) ->
    ok.
EOF
        ;;

    middleware)
        cat <<EOF
%%%-------------------------------------------------------------------
%%% @doc
%%% ${MODULE_NAME} - Custom middleware plugin
%%%
%%% Add your custom middleware logic here.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(${MODULE_NAME}).
-behaviour(erlmcp_plugin_middleware).

%% Plugin behavior callbacks
-export([
    init/1,
    pre_execute/2,
    post_execute/2,
    metadata/0,
    terminate/2
]).

-record(state, {
    %% Add your state fields here
}).

%%====================================================================
%% Plugin Behavior Callbacks
%%====================================================================

%% @doc Plugin metadata
metadata() ->
    #{
        name => <<"${PLUGIN_NAME}">>,
        version => <<"1.0.0">>,
        type => middleware,
        description => <<"Custom middleware plugin">>,
        author => <<"Your Name">>
    }.

%% @doc Initialize plugin
init(Opts) ->
    {ok, #state{}}.

%% @doc Pre-execute hook (runs before command)
pre_execute(Request, State) ->
    %% Add pre-processing logic here
    %% Example: logging, validation, transformation
    {ok, Request, State}.

%% @doc Post-execute hook (runs after command)
post_execute(Response, State) ->
    %% Add post-processing logic here
    %% Example: logging, enrichment, transformation
    {ok, Response, State}.

%% @doc Terminate plugin
terminate(_Reason, _State) ->
    ok.
EOF
        ;;

    *)
        echo "Error: Invalid plugin type: $PLUGIN_TYPE"
        echo "Valid types: validator, formatter, exporter, command, middleware"
        exit 1
        ;;
esac
