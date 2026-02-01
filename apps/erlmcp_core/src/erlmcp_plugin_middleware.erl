%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Middleware Behavior - Request/response interceptors
%%%
%%% Middleware intercepts CLI requests and responses:
%%% - Logging and auditing
%%% - Request transformation
%%% - Response enrichment
%%% - Rate limiting
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(erlmcp_plugin_audit_middleware).
%%% -behaviour(erlmcp_plugin_middleware).
%%%
%%% -export([init/1, pre_execute/2, post_execute/2, metadata/0]).
%%%
%%% metadata() ->
%%%     #{name => <<"audit_middleware">>,
%%%       version => <<"1.0.0">>,
%%%       type => middleware,
%%%       description => <<"Audit logging middleware">>}.
%%%
%%% init(Opts) ->
%%%     {ok, #{}}.
%%%
%%% pre_execute(Request, State) ->
%%%     %% Log request
%%%     {ok, Request, State}.
%%%
%%% post_execute(Response, State) ->
%%%     %% Log response
%%%     {ok, Response, State}.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_middleware).

%% Re-export plugin behavior
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    erlmcp_plugin:behaviour_info(callbacks) ++ [{pre_execute, 2}, {post_execute, 2}];
behaviour_info(_) ->
    undefined.
