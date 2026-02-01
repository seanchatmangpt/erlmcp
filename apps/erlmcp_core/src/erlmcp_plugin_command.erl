%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Command Behavior - Custom CLI commands
%%%
%%% Commands add new functionality to the CLI:
%%% - Custom validation workflows
%%% - Integration with external tools
%%% - Organization-specific operations
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(erlmcp_plugin_analyze_command).
%%% -behaviour(erlmcp_plugin_command).
%%%
%%% -export([init/1, execute/2, help/0, metadata/0]).
%%%
%%% metadata() ->
%%%     #{name => <<"analyze">>,
%%%       version => <<"1.0.0">>,
%%%       type => command,
%%%       description => <<"Analyze MCP server">>}.
%%%
%%% init(Opts) ->
%%%     {ok, #{}}.
%%%
%%% execute(Args, State) ->
%%%     %% Execute command logic
%%%     {ok, #{result => <<"analysis complete">>}, State}.
%%%
%%% help() ->
%%%     <<"analyze <url> - Analyze MCP server">>.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_command).

%% Re-export plugin behavior
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    erlmcp_plugin:behaviour_info(callbacks) ++ [{execute, 2}, {help, 0}];
behaviour_info(_) ->
    undefined.
