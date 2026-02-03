%% @private
%% Promotion Manager - Handles automatic site promotion during disasters
-module(erlmcp_promotion_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_promotion/2,
    request_promotion/1,
    get_promotion_status/0
]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(promotion_request, {
    site_id :: binary(),
    timestamp :: integer(),
    reason :: term(),
    status :: requested | approved | rejected | in_progress | completed | failed,
    votes :: [binary()],
    objections :: [binary()],
    priority :: high | normal | low
}).

-record(promotion_state, {
    current_primary :: binary() | undefined,
    requests :: queue(),
    active_promotion :: #promotion_request{} | undefined,
    promotion_history :: [#promotion_request{}],
    voting_threshold :: 0.5,  % 50% majority
    max_concurrent_promotions :: 1,
    promotion_timeout :: integer()  % milliseconds
}).

-define(SERVER, ?MODULE).
-define(PROMOTION_TIMEOUT, 60000).  % 1 minute
-define(VOTING_TIMEOUT, 30000).   % 30 seconds
-define(MAX_HISTORY, 100).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_promotion(binary(), term()) -> ok | {error, term()}.
start_promotion(SiteId, Reason) ->
    gen_server:call(?SERVER, {start_promotion, SiteId, Reason}).

-spec request_promotion(binary()) -> ok | {error, term()}.
request_promotion(SiteId) ->
    gen_server:call(?SERVER, {request_promotion, SiteId}).

-spec get_promotion_status() -> {ok, map()} | {error, term()}.
get_promotion_status() ->
    gen_server:call(?SERVER, get_promotion_status).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #promotion_state{}}.
init([]) ->
    %% Initialize promotion state
    State = #promotion_state{
        current_primary = undefined,
        requests = queue:new(),
        active_promotion = undefined,
        promotion_history = [],
        voting_threshold = 0.5,
        max_concurrent_promotions = 1,
        promotion_timeout = ?PROMOTION_TIMEOUT
    },

    %% Register with global monitoring
    erlmcp_global_monitor:register_promotion_manager(self()),

    {ok, State}.

-spec handle_call(term(), {pid(), any()}, #promotion_state{}) -> {reply, term(), #promotion_state{}}.
handle_call({start_promotion, SiteId, Reason}, _From, State) ->
    %% Validate promotion request
    case validate_promotion_request(SiteId, State) of
        {valid, Details} ->
            %% Check if we can start promotion
            case State#promotion_state.active_promotion of
                undefined ->
                    %% No active promotion - start new one
                    PromotionRequest = #promotion_request{
                        site_id = SiteId,
                        timestamp = erlang:system_time(millisecond),
                        reason = Reason,
                        status = requested,
                        votes = [],
                        objections = [],
                        priority = Details#promotion_request.priority
                    },

                    %% Update state
                    NewState = State#promotion_state{
                        requests = queue:in(PromotionRequest, State#promotion_state.requests),
                        active_promotion = PromotionRequest
                    },

                    %% Start voting process
                    start_voting_process(PromotionRequest),

                    {reply, ok, NewState};
                _ ->
                    %% Active promotion exists - queue request
                    PromotionRequest = #promotion_request{
                        site_id = SiteId,
                        timestamp = erlang:system_time(millisecond),
                        reason = Reason,
                        status = requested,
                        votes = [],
                        objections = [],
                        priority = Details#promotion_request.priority
                    },

                    NewState = State#promotion_state{
                        requests = queue:in(PromotionRequest, State#promotion_state.requests)
                    },

                    {reply, ok, NewState}
            end;
        {invalid, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({request_promotion, SiteId}, _From, State) ->
    %% Request promotion from site
    case validate_promotion_request(SiteId, State) of
        {valid, Details} ->
            %% Check if site can be promoted
            case check_site_promotion_readiness(SiteId) of
                true ->
                    %% Create promotion request
                    PromotionRequest = #promotion_request{
                        site_id = SiteId,
                        timestamp = erlang:system_time(millisecond),
                        reason = manual_request,
                        status = requested,
                        votes = [],
                        objections = [],
                        priority = Details#promotion_request.priority
                    },

                    %% Queue request
                    NewState = State#promotion_state{
                        requests = queue:in(PromotionRequest, State#promotion_state.requests)
                    },

                    {reply, ok, NewState};
                false ->
                    {reply, {error, site_not_ready}, State}
            end;
        {invalid, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_promotion_status, _From, State) ->
    Status = #{
        current_primary => State#promotion_state.current_primary,
        active_promotion => case State#promotion_state.active_promotion of
            undefined -> undefined;
            Promo -> format_promotion_request(Promo)
        end,
        pending_requests => queue:len(State#promotion_state.requests),
        promotion_history => length(State#promotion_state.promotion_history),
        voting_threshold => State#promotion_state.voting_threshold
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #promotion_state{}) -> {noreply, #promotion_state{}}.
handle_cast({vote, SiteId, Vote, PromotionId}, State) ->
    %% Handle voting response
    case State#promotion_state.active_promotion of
        #promotion_request{id = PromotionId} = Promo ->
            %% Update votes
            case Vote of
                approve ->
                    Votes = [SiteId | Promo#promotion_request.votes],
                    Objections = Promo#promotion_request.objections;
                object ->
                    Votes = Promo#promotion_request.votes,
                    Objections = [SiteId | Promo#promotion_request.objections];
                abstain ->
                    Votes = Promo#promotion_request.votes,
                    Objections = Promo#promotion_request.objections
            end,

            UpdatedPromo = Promo#promotion_request{
                votes = Votes,
                objections = Objections
            },

            %% Check if voting is complete
            TotalVotes = length(Votes) + length(Objections) + 1,  +1 for requester
            case check_voting_complete(UpdatedPromo, TotalVotes, State#promotion_state.voting_threshold) of
                true ->
                    %% Voting complete - approve or reject
                    case length(Votes) / TotalVotes >= State#promotion_state.voting_threshold of
                        true ->
                            %% Approved - start promotion
                            start_site_promotion(UpdatedPromo, State);
                        false ->
                            %% Rejected - log and clean up
                            log_promotion_rejection(UpdatedPromo),
                            NewState = cleanup_promotion(State),
                            {noreply, NewState}
                    end;
                false ->
                    %% Voting still in progress
                    NewState = State#promotion_state{active_promotion = UpdatedPromo},
                    {noreply, NewState}
            end;
        undefined ->
            %% No active promotion - ignore vote
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #promotion_state{}) -> {noreply, #promotion_state{}}.
handle_info({promotion_timeout, PromotionId}, State) ->
    %% Handle promotion timeout
    case State#promotion_state.active_promotion of
        #promotion_request{id = PromotionId} ->
            Promotion = State#promotion_state.active_promotion,
            log_promotion_timeout(Promotion),
            NewState = cleanup_promotion(State),
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info({promotion_failed, PromotionId, Reason}, State) ->
    %% Handle promotion failure
    case State#promotion_state.active_promotion of
        #promotion_request{id = PromotionId} ->
            Promotion = State#promotion_state.active_promotion,
            log_promotion_failure(Promotion, Reason),
            NewState = cleanup_promotion(State),
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #promotion_state{}) -> ok.
terminate(Reason, State) ->
    logger:warning("Promotion manager terminating: ~p", [Reason]),
    erlmcp_global_monitor:deregister_promotion_manager().

-spec code_change(term(), #promotion_state{}, term()) -> {ok, #promotion_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

validate_promotion_request(SiteId, State) ->
    %% Validate promotion request
    case erlmcp_global_directory:find_site_manager(SiteId) of
        {ok, _} ->
            %% Site exists
            case State#promotion_state.current_primary of
                SiteId ->
                    {invalid, already_primary};
                _ ->
                    %% Check if site is active
                    case erlmcp_global_monitor:get_site_status(SiteId) of
                        {ok, Status} when Status#site_status.status =:= healthy ->
                            %% Site is healthy
                            {valid, #promotion_request{site_id = SiteId, priority = high}};
                        {ok, Status} when Status#site_status.status =:= degraded ->
                            %% Site is degraded - lower priority
                            {valid, #promotion_request{site_id = SiteId, priority = normal}};
                        {error, _} ->
                            {invalid, site_unavailable}
                    end
            end;
        {error, not_found} ->
            {invalid, site_not_found}
    end.

start_voting_process(PromotionRequest) ->
    %% Start voting process
    Sites = erlmcp_global_directory:get_all_site_ids(),
    ActiveSites = lists:filter(fun(SiteId) ->
        SiteId =/= PromotionRequest#promotion_request.site_id
    end, Sites),

    %% Send vote requests to all sites
    lists:foreach(fun(SiteId) ->
        request_vote(SiteId, PromotionRequest#promotion_request.site_id)
    end, ActiveSites),

    %% Set voting timeout
    erlang:send_after(?VOTING_TIMEOUT, self(),
                     {vote_timeout, PromotionRequest#promotion_request.site_id}).

request_vote(SiteId, RequestingSiteId) ->
    %% Request vote from site
    SitePid = case erlmcp_global_directory:find_site_manager(SiteId) of
        {ok, Pid} -> Pid;
        {error, _} -> error(not_found)
    end,

    gen_server:cast(SitePid, {request_vote, RequestingSiteId}).

check_site_promotion_readiness(SiteId) ->
    %% Check if site is ready for promotion
    case erlmcp_site_manager:get_status(SiteId) of
        {ok, Status} ->
            %% Check readiness score
            Readiness = maps:get(readiness, Status, not_ready),
            Readiness =:= ready orelse Readiness =:= ready_with_warnings;
        {error, _} ->
            false
    end.

check_voting_complete(PromotionRequest, TotalVotes, Threshold) ->
    %% Check if voting is complete
    VoteCount = length(PromotionRequest#promotion_request.votes) +
                length(PromotionRequest#promotion_request.objections) + 1,  +1 for requester

    VoteCount >= TotalVotes orelse
    VoteCount > erlang:round(TotalVotes * Threshold).

start_site_promotion(PromotionRequest, State) ->
    %% Start site promotion
    SiteId = PromotionRequest#promotion_request.site_id,
    Reason = PromotionRequest#promotion_request.reason,

    %% Log promotion start
    logger:info("Starting promotion of site ~p to primary. Reason: ~p", [SiteId, Reason]),

    %% Update promotion status
    UpdatedPromo = PromotionRequest#promotion_request{
        status = in_progress,
        timestamp = erlang:system_time(millisecond)
    },

    %% Send promotion request to site
    gen_server:cast(erlmcp_site_manager:make_ref(SiteId),
                    {promote_to_primary, Reason, UpdatedPromo}),

    %% Set timeout
    erlang:send_after(State#promotion_state.promotion_timeout, self(),
                     {promotion_timeout, PromotionRequest#promotion_request.site_id}).

log_promotion_rejection(PromotionRequest) ->
    %% Log promotion rejection
    logger:warning("Promotion of site ~p rejected. Votes: ~p, Objections: ~p",
                  [PromotionRequest#promotion_request.site_id,
                   PromotionRequest#promotion_request.votes,
                   PromotionRequest#promotion_request.objections]).

cleanup_promotion(State) ->
    %% Clean up after promotion completion or failure
    NewState = case State#promotion_state.requests of
        Q when queue:len(Q) > 0 ->
            %% Process next request
            {Promo, NewQ} = queue:out(Q),
            case Promo of
                {value, NewRequest} ->
                    start_voting_process(NewRequest),
                    State#promotion_state{
                        requests = NewQ,
                        active_promotion = NewRequest
                    };
                empty ->
                    State#promotion_state{
                        requests = NewQ,
                        active_promotion = undefined
                    }
            end;
        _ ->
            State#promotion_state{
                active_promotion = undefined
            }
    end,

    %% Add to history if there was an active promotion
    case State#promotion_state.active_promotion of
        undefined ->
            NewState;
        Promo ->
            History = lists:sublist([Promo | State#promotion_state.promotion_history],
                                  ?MAX_HISTORY),
            NewState#promotion_state{
                promotion_history = History
            }
    end.

format_promotion_request(PromotionRequest) ->
    %% Format promotion request for response
    #{
        site_id => PromotionRequest#promotion_request.site_id,
        reason => PromotionRequest#promotion_request.reason,
        status => PromotionRequest#promotion_request.status,
        votes => PromotionRequest#promotion_request.votes,
        objections => PromotionRequest#promotion_request.objections,
        priority => PromotionRequest#promotion_request.priority,
        timestamp => PromotionRequest#promotion_request.timestamp
    }.

log_promotion_timeout(PromotionRequest) ->
    logger:warning("Promotion of site ~p timed out", [PromotionRequest#promotion_request.site_id]).

log_promotion_failure(PromotionRequest, Reason) ->
    logger:error("Promotion of site ~p failed: ~p", [PromotionRequest#promotion_request.site_id, Reason]).