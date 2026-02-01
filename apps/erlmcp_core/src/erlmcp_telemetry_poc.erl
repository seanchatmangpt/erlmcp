-module(erlmcp_telemetry_poc).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/1, attach_handler/3, detach_handler/2, emit_event/3,
         get_handlers/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {handlers = #{} :: #{event_name() => [handler()]}}).

-type event_name() :: atom().
-type event_metadata() :: #{atom() => term()}.
-type handler() ::
    #{id => term(),
      module => module(),
      function => atom()}.

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec attach_handler(pid(), event_name(), handler()) -> ok | {error, term()}.
attach_handler(Pid, EventName, Handler) ->
    gen_server:call(Pid, {attach_handler, EventName, Handler}).

-spec detach_handler(pid(), event_name()) -> ok.
detach_handler(Pid, EventName) ->
    gen_server:call(Pid, {detach_handler, EventName}).

-spec emit_event(pid(), event_name(), event_metadata()) -> ok.
emit_event(Pid, EventName, Metadata) ->
    gen_server:cast(Pid, {emit_event, EventName, Metadata}).

-spec get_handlers(pid(), event_name()) -> {ok, [handler()]}.
get_handlers(Pid, EventName) ->
    gen_server:call(Pid, {get_handlers, EventName}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_Opts) ->
    {ok, #state{}}.

handle_call({attach_handler, EventName, Handler}, _From, State = #state{handlers = Handlers}) ->
    CurrentHandlers = maps:get(EventName, Handlers, []),
    NewHandlers = maps:put(EventName, [Handler | CurrentHandlers], Handlers),
    {reply, ok, State#state{handlers = NewHandlers}};
handle_call({detach_handler, EventName}, _From, State = #state{handlers = Handlers}) ->
    NewHandlers = maps:remove(EventName, Handlers),
    {reply, ok, State#state{handlers = NewHandlers}};
handle_call({get_handlers, EventName}, _From, State = #state{handlers = Handlers}) ->
    EventHandlers = maps:get(EventName, Handlers, []),
    {reply, {ok, EventHandlers}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({emit_event, EventName, Metadata}, State = #state{handlers = Handlers}) ->
    EventHandlers = maps:get(EventName, Handlers, []),
    lists:foreach(fun(Handler) ->
                     Module = maps:get(module, Handler),
                     Function = maps:get(function, Handler),
                     try
                         erlang:apply(Module, Function, [EventName, Metadata])
                     catch
                         _:_ ->
                             ok
                     end
                  end,
                  EventHandlers),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
