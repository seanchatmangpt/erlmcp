%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Validator Behavior - Custom spec validators
%%%
%%% Validators extend the compliance checking system with custom
%%% validation logic. Use cases:
%%% - Domain-specific MCP spec extensions
%%% - Custom protocol validators
%%% - Organization-specific compliance rules
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(my_custom_validator).
%%% -behaviour(erlmcp_plugin_validator).
%%%
%%% -export([init/1, validate/2, get_schema/0, metadata/0]).
%%%
%%% metadata() ->
%%%     #{name => <<"my_validator">>,
%%%       version => <<"1.0.0">>,
%%%       type => validator,
%%%       description => <<"Custom validator">>}.
%%%
%%% init(Opts) ->
%%%     {ok, #{}}.
%%%
%%% validate(Data, State) ->
%%%     %% Custom validation logic
%%%     {ok, #{status => passed, details => #{}}, State}.
%%%
%%% get_schema() ->
%%%     #{type => object}.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_validator).

%% Re-export plugin behavior
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    erlmcp_plugin:behaviour_info(callbacks) ++ [{validate, 2}, {get_schema, 0}];
behaviour_info(_) ->
    undefined.
