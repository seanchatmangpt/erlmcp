-module(erlmcp_mcp_proxy_relay).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([forward_request/1]).
-export([add_upstream/2]).
-export([remove_upstream/1]).
-export([list_upstreams/0]).
-export([get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).
-define(MAX_RETRIES, 3).

-record(state, {
    upstreams = #{},
    stats = #{forwarded => 0, errors => 0, last_error => undefined}
}).

-type upstream() :: #{
    name => atom(),
    url => binary(),
    timeout => pos_integer(),
    enabled => boolean()
}.

-type stats() :: #{
    forwarded => non_neg_integer(),
    errors => non_neg_integer(),
    last_error => undefined | {term(), erlang:timestamp()}
}.

-export_type([upstream/0, stats/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec forward_request(map()) -> {ok, map()} | {error, term()}.
forward_request(Request) when is_map(Request) ->
    gen_server:call(?SERVER, {forward_request, Request}, ?DEFAULT_TIMEOUT * 2).

-spec add_upstream(atom(), binary()) -> ok | {error, term()}.
add_upstream(Name, Url) when is_atom(Name), is_binary(Url) ->
    gen_server:call(?SERVER, {add_upstream, Name, Url}).

-spec remove_upstream(atom()) -> ok | {error, term()}.
remove_upstream(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {remove_upstream, Name}).

-spec list_upstreams() -> [upstream()].
list_upstreams() ->
    gen_server:call(?SERVER, list_upstreams).

-spec get_stats() -> stats().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({add_upstream, Name, Url}, _From, State) ->
    Upstream = #{
        name => Name,
        url => Url,
        timeout => ?DEFAULT_TIMEOUT,
        enabled => true
    },
    NewUpstreams = maps:put(Name, Upstream, State#state.upstreams),
    {reply, ok, State#state{upstreams = NewUpstreams}};

handle_call({remove_upstream, Name}, _From, State) ->
    NewUpstreams = maps:remove(Name, State#state.upstreams),
    {reply, ok, State#state{upstreams = NewUpstreams}};

handle_call(list_upstreams, _From, State) ->
    Upstreams = maps:values(State#state.upstreams),
    {reply, Upstreams, State};

handle_call(get_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call({forward_request, Request}, _From, State) ->
    case maps:size(State#state.upstreams) of
        0 ->
            NewStats = increment_error(State#state.stats, no_upstreams),
            {reply, {error, no_upstreams}, State#state{stats = NewStats}};
        _ ->
            case do_forward(Request, State#state.upstreams) of
                {ok, Response} ->
                    NewStats = increment_forwarded(State#state.stats),
                    {reply, {ok, Response}, State#state{stats = NewStats}};
                {error, Reason} ->
                    NewStats = increment_error(State#state.stats, Reason),
                    {reply, {error, Reason}, State#state{stats = NewStats}}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

do_forward(Request, Upstreams) ->
    % Select first enabled upstream
    Enabled = [U || U = #{enabled := true} <- maps:values(Upstreams)],
    case Enabled of
        [] -> {error, no_available_upstream};
        [Upstream | _] -> forward_to_upstream(Request, Upstream)
    end.

forward_to_upstream(Request, #{url := Url, timeout := _Timeout}) ->
    % Simulate forwarding request (in real implementation, use httpc/gun)
    try
        Response = #{
            jsonrpc => maps:get(jsonrpc, Request, <<"2.0">>),
            id => maps:get(id, Request, null),
            result => #{forwarded_to => Url}
        },
        {ok, Response}
    catch
        _:Error -> {error, {forward_failed, Error}}
    end.

increment_forwarded(Stats) ->
    Stats#{forwarded => maps:get(forwarded, Stats, 0) + 1}.

increment_error(Stats, Reason) ->
    Stats#{
        errors => maps:get(errors, Stats, 0) + 1,
        last_error => {Reason, erlang:timestamp()}
    }.
