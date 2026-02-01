%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Worker - Individual plugin instance process
%%%
%%% Each plugin runs in its own gen_server process for isolation.
%%% Crash in one plugin doesn't affect others or the system.
%%%
%%% == Process Model ==
%%% - One worker per plugin instance
%%% - Supervised by erlmcp_plugin_worker_sup
%%% - Registered in erlmcp_plugin_registry
%%% - Isolated execution environment
%%%
%%% == State ==
%%% - module: Plugin module name
%%% - plugin_state: Plugin's internal state
%%% - metadata: Plugin metadata
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_worker).

-behaviour(gen_server).

%% API
-export([start_link/2, call_function/3, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {module :: module(), plugin_state :: term(), metadata :: map(), opts :: map()}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(module(), map()) -> {ok, pid()} | {error, term()}.
start_link(Module, Opts) ->
    gen_server:start_link(?MODULE, {Module, Opts}, []).

%% @doc Call a plugin function
-spec call_function(pid(), atom(), [term()]) -> term().
call_function(Pid, Function, Args) ->
    gen_server:call(Pid, {call_function, Function, Args}, 10000).

%% @doc Stop plugin worker
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Module, Opts}) ->
    try
        %% Get plugin metadata
        Metadata = Module:metadata(),

        %% Initialize plugin
        case Module:init(Opts) of
            {ok, PluginState} ->
                State =
                    #state{module = Module,
                           plugin_state = PluginState,
                           metadata = Metadata,
                           opts = Opts},
                {ok, State};
            {error, Reason} ->
                {stop, {init_failed, Reason}}
        end
    catch
        _:Error:Stack ->
            {stop, {init_error, Error, Stack}}
    end.

handle_call({call_function, Function, Args}, _From, State) ->
    try
        Module = State#state.module,
        PluginState = State#state.plugin_state,

        %% Verify function exists
        case erlang:function_exported(Module, Function, length(Args)) of
            true ->
                %% Call plugin function
                Result =
                    case Args of
                        [] ->
                            Module:Function();
                        [Arg1]
                            when Function =:= pre_execute;
                                 Function =:= post_execute;
                                 Function =:= format;
                                 Function =:= export;
                                 Function =:= execute ->
                            %% Functions that take (Data, State)
                            case Module:Function(Arg1, PluginState) of
                                {ok, Output, NewPluginState} ->
                                    NewState = State#state{plugin_state = NewPluginState},
                                    {ok, Output, NewState};
                                {ok, Output} ->
                                    {ok, Output, State};
                                {error, Reason} ->
                                    {error, Reason, State}
                            end;
                        [Arg1] when Function =:= validate ->
                            %% validate/2 returns {ok, Result, State}
                            case Module:Function(Arg1, PluginState) of
                                {ok, Output, NewPluginState} ->
                                    NewState = State#state{plugin_state = NewPluginState},
                                    {ok, Output, NewState};
                                {error, Reason} ->
                                    {error, Reason, State}
                            end;
                        [Arg1] ->
                            %% Single arg, no state
                            Module:Function(Arg1);
                        _ ->
                            %% Multiple args, apply
                            erlang:apply(Module, Function, Args)
                    end,

                %% Handle result
                case Result of
                    {ok, ResultOutput, ResultState}
                        when is_tuple(ResultState); is_list(ResultState) ->
                        {reply, {ok, ResultOutput}, ResultState};
                    {ok, ResultOutput} ->
                        {reply, {ok, ResultOutput}, State};
                    {error, ErrorReason, _} ->
                        {reply, {error, ErrorReason}, State};
                    {error, ErrorReason} ->
                        {reply, {error, ErrorReason}, State};
                    Other ->
                        {reply, Other, State}
                end;
            false ->
                {reply, {error, function_not_exported}, State}
        end
    catch
        _:Error:Stack ->
            {reply, {error, {function_call_failed, Error, Stack}}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    %% Call plugin's terminate if it exists
    Module = State#state.module,
    PluginState = State#state.plugin_state,
    case erlang:function_exported(Module, terminate, 2) of
        true ->
            try
                Module:terminate(Reason, PluginState)
            catch
                _:_Error ->
                    ok
            end;
        false ->
            ok
    end,
    ok.
