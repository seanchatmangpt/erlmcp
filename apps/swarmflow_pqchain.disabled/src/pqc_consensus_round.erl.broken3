%%% @doc BFT Consensus Round State Machine
%%%
%%% Each consensus round is a supervised gen_statem process implementing
%%% Byzantine Fault Tolerant consensus for post-quantum blockchain.
%%%
%%% States: propose -> prevote -> precommit -> commit -> finalized
%%%
%%% Consensus Rules:
%%% - Need 2/3+ voting power for quorum
%%% - Lock on block if 2/3+ prevotes received
%%% - Commit if 2/3+ precommits received
%%% - Timeout and move to next round if no progress
%%%
%%% Process Registration:
%%% - gproc: {n, l, {consensus_round, Height, Round}}
%%%
%%% @end
-module(pqc_consensus_round).

-behaviour(gen_statem).

-include("pqchain.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/3,
    propose/2,
    vote/2,
    get_state/1,
    get_round_state/1,
    is_proposer/2,
    stop/1
]).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4,
    format_status/1
]).

%% State functions
-export([
    propose/3,
    prevote/3,
    precommit/3,
    commit/3,
    finalized/3
]).

%%====================================================================
%% Types
%%====================================================================

-type height() :: non_neg_integer().
-type round_number() :: non_neg_integer().
-type validator_address() :: binary().
-type vote_type() :: prevote | precommit.
-type phase() :: propose | prevote | precommit | commit | finalized.

-record(state_data, {
    height :: height(),
    round :: round_number(),
    validator_set :: #validator_set{},
    proposer :: validator_address(),

    %% Current round data
    proposal :: #pqc_block{} | undefined,
    proposal_received_at :: non_neg_integer() | undefined,

    %% Vote tracking
    prevotes :: #{validator_address() => #consensus_vote{}},
    precommits :: #{validator_address() => #consensus_vote{}},
    prevote_power :: non_neg_integer(),
    precommit_power :: non_neg_integer(),

    %% Locking mechanism (prevent equivocation)
    locked_block :: #pqc_block{} | undefined,
    locked_round :: round_number() | undefined,
    valid_block :: #pqc_block{} | undefined,
    valid_round :: round_number() | undefined,

    %% Quorum certificate
    qc :: #quorum_certificate{} | undefined,

    %% Timing
    started_at :: non_neg_integer(),
    propose_timeout_ms :: non_neg_integer(),
    prevote_timeout_ms :: non_neg_integer(),
    precommit_timeout_ms :: non_neg_integer(),

    %% Metadata
    round_id :: binary()
}).

-type state_data() :: #state_data{}.
-type state_name() :: propose | prevote | precommit | commit | finalized.

%% Default timeouts (milliseconds)
-define(DEFAULT_PROPOSE_TIMEOUT, 3000).
-define(DEFAULT_PREVOTE_TIMEOUT, 1000).
-define(DEFAULT_PRECOMMIT_TIMEOUT, 1000).
-define(HIBERNATE_AFTER_MS, 30000).

%% Quorum threshold (2/3+ voting power)
-define(QUORUM_FRACTION, 0.67).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a consensus round process.
%%
%% The process is registered with gproc as {n, l, {consensus_round, Height, Round}}.
%% This allows other processes to find the round by height and round number.
%%
%% @end
-spec start_link(height(), round_number(), #validator_set{}) ->
    {ok, pid()} | {error, term()}.
start_link(Height, Round, #validator_set{} = ValidatorSet) when is_integer(Height),
                                              is_integer(Round) ->
    gen_statem:start_link(
        ?MODULE,
        {Height, Round, ValidatorSet},
        [{hibernate_after, ?HIBERNATE_AFTER_MS}]
    ).

%% @doc Submit a block proposal (only proposer can call).
%%
%% Returns:
%% - ok: Proposal accepted
%% - {error, not_proposer}: Caller is not the proposer
%% - {error, wrong_phase}: Not in propose phase
%% - {error, proposal_exists}: Proposal already received
%%
%% @end
-spec propose(pid(), #pqc_block{}) -> ok | {error, term()}.
propose(Pid, #pqc_block{} = Block) when is_pid(Pid) ->
    gen_statem:call(Pid, {propose_block, Block}, 5000).

%% @doc Submit a vote (prevote or precommit).
%%
%% Returns:
%% - ok: Vote accepted
%% - {error, invalid_vote}: Vote validation failed
%% - {error, duplicate_vote}: Already received vote from this validator
%% - {error, wrong_phase}: Vote type doesn't match current phase
%%
%% @end
-spec vote(pid(), #consensus_vote{}) -> ok | {error, term()}.
vote(Pid, #consensus_vote{} = Vote) when is_pid(Pid) ->
    gen_statem:call(Pid, {submit_vote, Vote}, 5000).

%% @doc Get current phase of the round.
-spec get_state(pid()) -> phase().
get_state(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, get_current_state, 5000).

%% @doc Get complete round state (for debugging/monitoring).
-spec get_round_state(pid()) -> {ok, map()}.
get_round_state(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, get_round_state, 5000).

%% @doc Check if a validator is the proposer for this round.
-spec is_proposer(pid(), validator_address()) -> boolean().
is_proposer(Pid, ValidatorAddr) when is_pid(Pid), is_binary(ValidatorAddr) ->
    gen_statem:call(Pid, {is_proposer, ValidatorAddr}, 5000).

%% @doc Stop the consensus round process.
-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    gen_statem:stop(Pid).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec callback_mode() -> [state_functions | state_enter, ...].
callback_mode() ->
    [state_functions, state_enter].

-spec init({height(), round_number(), #validator_set{}}) ->
    {ok, propose, state_data()}.
init({Height, Round, ValidatorSet}) ->
    process_flag(trap_exit, true),

    %% Generate unique ID for this round
    RoundId = generate_ulid(),

    %% Select proposer for this round (round-robin for now)
    Proposer = select_proposer(Round, ValidatorSet),

    %% Register with gproc
    try
        ok = gproc:reg({n, l, {consensus_round, Height, Round}})
    catch
        error:badarg ->
            %% Already registered (shouldn't happen)
            ?LOG_WARNING("Consensus round ~p/~p already registered", [Height, Round])
    end,

    StartTime = erlang:system_time(millisecond),

    Data = #state_data{
        height = Height,
        round = Round,
        validator_set = ValidatorSet,
        proposer = Proposer,
        proposal = undefined,
        proposal_received_at = undefined,
        prevotes = #{},
        precommits = #{},
        prevote_power = 0,
        precommit_power = 0,
        locked_block = undefined,
        locked_round = undefined,
        valid_block = undefined,
        valid_round = undefined,
        qc = undefined,
        started_at = StartTime,
        propose_timeout_ms = ?DEFAULT_PROPOSE_TIMEOUT,
        prevote_timeout_ms = ?DEFAULT_PREVOTE_TIMEOUT,
        precommit_timeout_ms = ?DEFAULT_PRECOMMIT_TIMEOUT,
        round_id = RoundId
    },

    ?LOG_INFO("Consensus round ~s: height=~p round=~p proposer=~s",
              [RoundId, Height, Round, format_address(Proposer)]),

    {ok, propose, Data}.

-spec terminate(term(), state_name(), state_data()) -> ok.
terminate(Reason, State, Data) ->
    ?LOG_INFO("Consensus round ~s terminating in state ~p: ~p",
              [Data#state_data.round_id, State, Reason]),

    %% Unregister from gproc
    try
        gproc:unreg({n, l, {consensus_round, Data#state_data.height, Data#state_data.round}})
    catch
        error:badarg ->
            ok
    end,

    ok.

-spec code_change(term(), state_name(), state_data(), term()) ->
    {ok, state_name(), state_data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

-spec format_status(map()) -> map().
format_status(#{state := State, data := Data}) ->
    #{
        state => State,
        round_id => Data#state_data.round_id,
        height => Data#state_data.height,
        round => Data#state_data.round,
        proposer => format_address(Data#state_data.proposer),
        has_proposal => Data#state_data.proposal =/= undefined,
        prevote_count => map_size(Data#state_data.prevotes),
        precommit_count => map_size(Data#state_data.precommits),
        prevote_power => Data#state_data.prevote_power,
        precommit_power => Data#state_data.precommit_power,
        has_locked_block => Data#state_data.locked_block =/= undefined,
        has_qc => Data#state_data.qc =/= undefined
    }.

%%====================================================================
%% State Functions
%%====================================================================

%% @doc PROPOSE state - Waiting for block proposal or timeout.
%%
%% State entry:
%% - Start propose timeout
%% - If we are the proposer, we should create and broadcast a proposal
%%
%% Events:
%% - {propose_block, Block}: Receive proposal from proposer
%% - state_timeout: No proposal received, move to prevote with nil
%%
%% @end
propose(enter, _OldState, Data) ->
    ?LOG_DEBUG("Round ~s: PROPOSE phase (timeout: ~pms)",
               [Data#state_data.round_id, Data#state_data.propose_timeout_ms]),

    %% Set timeout for proposal phase
    Timeout = Data#state_data.propose_timeout_ms,
    Actions = [{state_timeout, Timeout, propose_timeout}],

    {keep_state_and_data, Actions};

propose(state_timeout, propose_timeout, Data) ->
    ?LOG_WARNING("Round ~s: Propose timeout - no proposal received",
                 [Data#state_data.round_id]),

    %% Move to prevote with nil vote (no valid proposal)
    {next_state, prevote, Data};

propose({call, From}, {propose_block, Block}, Data) ->
    case validate_proposal(Block, Data) of
        ok ->
            ?LOG_INFO("Round ~s: Proposal received from ~s",
                      [Data#state_data.round_id,
                       format_address(Data#state_data.proposer)]),

            ReceivedAt = erlang:system_time(millisecond),
            NewData = Data#state_data{
                proposal = Block,
                proposal_received_at = ReceivedAt
            },

            %% Automatically transition to prevote phase
            {next_state, prevote, NewData, [{reply, From, ok}]};

        {error, Reason} = Error ->
            ?LOG_WARNING("Round ~s: Invalid proposal: ~p",
                         [Data#state_data.round_id, Reason]),
            {keep_state_and_data, [{reply, From, Error}]}
    end;

propose({call, From}, {submit_vote, _Vote}, _Data) ->
    %% Votes not accepted in propose phase
    {keep_state_and_data, [{reply, From, {error, wrong_phase}}]};

propose({call, From}, get_current_state, _Data) ->
    {keep_state_and_data, [{reply, From, propose}]};

propose({call, From}, {is_proposer, Addr}, Data) ->
    Result = Addr =:= Data#state_data.proposer,
    {keep_state_and_data, [{reply, From, Result}]};

propose({call, From}, get_round_state, Data) ->
    State = build_round_state(propose, Data),
    {keep_state_and_data, [{reply, From, {ok, State}}]};

propose(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, Data).

%% @doc PREVOTE state - Collect prevotes from validators.
%%
%% State entry:
%% - Start prevote timeout
%% - Broadcast our prevote
%%
%% Events:
%% - {submit_vote, Vote}: Receive prevote from validator
%% - state_timeout: Prevote timeout, move to precommit
%%
%% Transition:
%% - If 2/3+ prevotes for a block: Lock on block, move to precommit
%% - If 2/3+ prevotes for nil: Move to precommit with nil
%% - If timeout: Move to precommit
%%
%% @end
prevote(enter, _OldState, Data) ->
    ?LOG_DEBUG("Round ~s: PREVOTE phase (timeout: ~pms)",
               [Data#state_data.round_id, Data#state_data.prevote_timeout_ms]),

    %% Set timeout for prevote phase
    Timeout = Data#state_data.prevote_timeout_ms,
    Actions = [{state_timeout, Timeout, prevote_timeout}],

    {keep_state_and_data, Actions};

prevote(state_timeout, prevote_timeout, Data) ->
    ?LOG_INFO("Round ~s: Prevote timeout - moving to precommit (power: ~p/~p)",
              [Data#state_data.round_id,
               Data#state_data.prevote_power,
               Data#state_data.validator_set#validator_set.total_voting_power]),

    %% Move to precommit phase
    {next_state, precommit, Data};

prevote({call, From}, {submit_vote, Vote}, Data)
    when Vote#consensus_vote.type =:= prevote ->
    case validate_vote(Vote, Data) of
        ok ->
            case handle_prevote(Vote, Data) of
                {ok, NewData, quorum_reached} ->
                    ?LOG_INFO("Round ~s: Prevote quorum reached (power: ~p/~p)",
                              [Data#state_data.round_id,
                               NewData#state_data.prevote_power,
                               Data#state_data.validator_set#validator_set.total_voting_power]),

                    %% Lock on block and transition to precommit
                    LockedData = lock_block(NewData),
                    {next_state, precommit, LockedData, [{reply, From, ok}]};

                {ok, NewData, continue} ->
                    {keep_state, NewData, [{reply, From, ok}]}
            end;

        {error, Reason} = Error ->
            ?LOG_WARNING("Round ~s: Invalid prevote: ~p",
                         [Data#state_data.round_id, Reason]),
            {keep_state_and_data, [{reply, From, Error}]}
    end;

prevote({call, From}, {submit_vote, _Vote}, _Data) ->
    %% Wrong vote type
    {keep_state_and_data, [{reply, From, {error, wrong_phase}}]};

prevote({call, From}, get_current_state, _Data) ->
    {keep_state_and_data, [{reply, From, prevote}]};

prevote({call, From}, {is_proposer, Addr}, Data) ->
    Result = Addr =:= Data#state_data.proposer,
    {keep_state_and_data, [{reply, From, Result}]};

prevote({call, From}, get_round_state, Data) ->
    State = build_round_state(prevote, Data),
    {keep_state_and_data, [{reply, From, {ok, State}}]};

prevote(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, Data).

%% @doc PRECOMMIT state - Collect precommits from validators.
%%
%% State entry:
%% - Start precommit timeout
%% - Broadcast our precommit
%%
%% Events:
%% - {submit_vote, Vote}: Receive precommit from validator
%% - state_timeout: Precommit timeout, start new round
%%
%% Transition:
%% - If 2/3+ precommits for a block: Create QC, move to commit
%% - If 2/3+ precommits for nil: Start new round
%% - If timeout: Start new round
%%
%% @end
precommit(enter, _OldState, Data) ->
    ?LOG_DEBUG("Round ~s: PRECOMMIT phase (timeout: ~pms)",
               [Data#state_data.round_id, Data#state_data.precommit_timeout_ms]),

    %% Set timeout for precommit phase
    Timeout = Data#state_data.precommit_timeout_ms,
    Actions = [{state_timeout, Timeout, precommit_timeout}],

    {keep_state_and_data, Actions};

precommit(state_timeout, precommit_timeout, Data) ->
    ?LOG_WARNING("Round ~s: Precommit timeout - consensus failed",
                 [Data#state_data.round_id]),

    %% Consensus failed for this round
    %% In production, would signal need for new round
    {keep_state_and_data, []};

precommit({call, From}, {submit_vote, Vote}, Data)
    when Vote#consensus_vote.type =:= precommit ->
    case validate_vote(Vote, Data) of
        ok ->
            case handle_precommit(Vote, Data) of
                {ok, NewData, quorum_reached} ->
                    ?LOG_INFO("Round ~s: Precommit quorum reached - creating QC",
                              [Data#state_data.round_id]),

                    %% Create quorum certificate
                    QC = build_quorum_certificate(NewData),
                    FinalData = NewData#state_data{qc = QC},

                    %% Transition to commit phase
                    {next_state, commit, FinalData, [{reply, From, ok}]};

                {ok, NewData, continue} ->
                    {keep_state, NewData, [{reply, From, ok}]}
            end;

        {error, Reason} = Error ->
            ?LOG_WARNING("Round ~s: Invalid precommit: ~p",
                         [Data#state_data.round_id, Reason]),
            {keep_state_and_data, [{reply, From, Error}]}
    end;

precommit({call, From}, {submit_vote, _Vote}, _Data) ->
    %% Wrong vote type
    {keep_state_and_data, [{reply, From, {error, wrong_phase}}]};

precommit({call, From}, get_current_state, _Data) ->
    {keep_state_and_data, [{reply, From, precommit}]};

precommit({call, From}, {is_proposer, Addr}, Data) ->
    Result = Addr =:= Data#state_data.proposer,
    {keep_state_and_data, [{reply, From, Result}]};

precommit({call, From}, get_round_state, Data) ->
    State = build_round_state(precommit, Data),
    {keep_state_and_data, [{reply, From, {ok, State}}]};

precommit(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, Data).

%% @doc COMMIT state - Finalize the block.
%%
%% State entry:
%% - Block has reached consensus
%% - QC is available
%% - Ready to finalize and apply block
%%
%% In production, this would:
%% - Apply block to state
%% - Broadcast commit
%% - Transition to finalized
%%
%% @end
commit(enter, _OldState, Data) ->
    ?LOG_INFO("Round ~s: COMMIT phase - block ready to finalize",
              [Data#state_data.round_id]),

    %% Automatically transition to finalized
    %% In production, would wait for block application
    {next_state, finalized, Data};

commit({call, From}, get_current_state, _Data) ->
    {keep_state_and_data, [{reply, From, commit}]};

commit({call, From}, {is_proposer, Addr}, Data) ->
    Result = Addr =:= Data#state_data.proposer,
    {keep_state_and_data, [{reply, From, Result}]};

commit({call, From}, get_round_state, Data) ->
    State = build_round_state(commit, Data),
    {keep_state_and_data, [{reply, From, {ok, State}}]};

commit(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, Data).

%% @doc FINALIZED state - Terminal state, consensus complete.
%%
%% State entry:
%% - Block has been finalized
%% - Consensus round is complete
%% - Process can be terminated
%%
%% @end
finalized(enter, _OldState, Data) ->
    ?LOG_INFO("Round ~s: FINALIZED - consensus complete (height=~p)",
              [Data#state_data.round_id, Data#state_data.height]),

    Duration = erlang:system_time(millisecond) - Data#state_data.started_at,
    ?LOG_INFO("Round ~s: Consensus duration: ~pms", [Data#state_data.round_id, Duration]),

    {keep_state_and_data, []};

finalized({call, From}, get_current_state, _Data) ->
    {keep_state_and_data, [{reply, From, finalized}]};

finalized({call, From}, {is_proposer, Addr}, Data) ->
    Result = Addr =:= Data#state_data.proposer,
    {keep_state_and_data, [{reply, From, Result}]};

finalized({call, From}, get_round_state, Data) ->
    State = build_round_state(finalized, Data),
    {keep_state_and_data, [{reply, From, {ok, State}}]};

finalized({call, From}, {submit_vote, _Vote}, _Data) ->
    %% Round is finalized, no more votes accepted
    {keep_state_and_data, [{reply, From, {error, round_finalized}}]};

finalized({call, From}, {propose_block, _Block}, _Data) ->
    %% Round is finalized, no more proposals accepted
    {keep_state_and_data, [{reply, From, {error, round_finalized}}]};

finalized(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, Data).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Handle common events across all states.
handle_common(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% @private
%% @doc Select proposer for round (round-robin for now).
-spec select_proposer(round_number(), #validator_set{}) -> validator_address().
select_proposer(Round, ValidatorSet) ->
    Validators = ValidatorSet#validator_set.validators,
    Index = Round rem length(Validators),
    Validator = lists:nth(Index + 1, Validators),
    Validator#pqc_validator.address.

%% @private
%% @doc Validate a block proposal.
-spec validate_proposal(#pqc_block{}, state_data()) -> ok | {error, term()}.
validate_proposal(Block, Data) ->
    %% Check height matches
    Header = Block#pqc_block.header,
    case Header#pqc_block_header.height =:= Data#state_data.height of
        true ->
            %% Check proposer matches
            case Header#pqc_block_header.validator =:= Data#state_data.proposer of
                true ->
                    %% In production, would validate:
                    %% - Signature
                    %% - Transactions
                    %% - State root
                    ok;
                false ->
                    {error, wrong_proposer}
            end;
        false ->
            {error, wrong_height}
    end.

%% @private
%% @doc Validate a vote.
-spec validate_vote(#consensus_vote{}, state_data()) -> ok | {error, term()}.
validate_vote(Vote, Data) ->
    %% Check height matches
    case Vote#consensus_vote.height =:= Data#state_data.height of
        false ->
            {error, wrong_height};
        true ->
            %% Check round matches
            case Vote#consensus_vote.round =:= Data#state_data.round of
                false ->
                    {error, wrong_round};
                true ->
                    %% Check validator exists in set
                    ValidatorAddr = Vote#consensus_vote.validator,
                    case find_validator(ValidatorAddr, Data#state_data.validator_set) of
                        {ok, _Validator} ->
                            %% In production, would validate signature
                            ok;
                        error ->
                            {error, unknown_validator}
                    end
            end
    end.

%% @private
%% @doc Handle a prevote.
-spec handle_prevote(#consensus_vote{}, state_data()) ->
    {ok, state_data(), quorum_reached | continue}.
handle_prevote(Vote, Data) ->
    ValidatorAddr = Vote#consensus_vote.validator,

    %% Check for duplicate vote
    case maps:is_key(ValidatorAddr, Data#state_data.prevotes) of
        true ->
            %% Already have vote from this validator
            {ok, Data, continue};
        false ->
            %% Add vote
            NewPrevotes = maps:put(ValidatorAddr, Vote, Data#state_data.prevotes),

            %% Calculate new voting power
            {ok, Validator} = find_validator(ValidatorAddr, Data#state_data.validator_set),
            NewPower = Data#state_data.prevote_power + Validator#pqc_validator.voting_power,

            NewData = Data#state_data{
                prevotes = NewPrevotes,
                prevote_power = NewPower
            },

            %% Check if quorum reached
            case has_quorum(NewPower, Data#state_data.validator_set) of
                true ->
                    {ok, NewData, quorum_reached};
                false ->
                    {ok, NewData, continue}
            end
    end.

%% @private
%% @doc Handle a precommit.
-spec handle_precommit(#consensus_vote{}, state_data()) ->
    {ok, state_data(), quorum_reached | continue}.
handle_precommit(Vote, Data) ->
    ValidatorAddr = Vote#consensus_vote.validator,

    %% Check for duplicate vote
    case maps:is_key(ValidatorAddr, Data#state_data.precommits) of
        true ->
            %% Already have vote from this validator
            {ok, Data, continue};
        false ->
            %% Add vote
            NewPrecommits = maps:put(ValidatorAddr, Vote, Data#state_data.precommits),

            %% Calculate new voting power
            {ok, Validator} = find_validator(ValidatorAddr, Data#state_data.validator_set),
            NewPower = Data#state_data.precommit_power + Validator#pqc_validator.voting_power,

            NewData = Data#state_data{
                precommits = NewPrecommits,
                precommit_power = NewPower
            },

            %% Check if quorum reached
            case has_quorum(NewPower, Data#state_data.validator_set) of
                true ->
                    {ok, NewData, quorum_reached};
                false ->
                    {ok, NewData, continue}
            end
    end.

%% @private
%% @doc Lock on the proposed block.
-spec lock_block(state_data()) -> state_data().
lock_block(Data) ->
    Data#state_data{
        locked_block = Data#state_data.proposal,
        locked_round = Data#state_data.round,
        valid_block = Data#state_data.proposal,
        valid_round = Data#state_data.round
    }.

%% @private
%% @doc Check if voting power reaches quorum threshold.
-spec has_quorum(non_neg_integer(), #validator_set{}) -> boolean().
has_quorum(VotingPower, ValidatorSet) ->
    TotalPower = ValidatorSet#validator_set.total_voting_power,
    Threshold = ValidatorSet#validator_set.quorum_threshold,
    VotingPower >= trunc(TotalPower * Threshold).

%% @private
%% @doc Find a validator in the validator set.
-spec find_validator(validator_address(), #validator_set{}) ->
    {ok, #pqc_validator{}} | error.
find_validator(Addr, ValidatorSet) ->
    Validators = ValidatorSet#validator_set.validators,
    case lists:keyfind(Addr, #pqc_validator.address, Validators) of
        false -> error;
        Validator -> {ok, Validator}
    end.

%% @private
%% @doc Build quorum certificate from precommits.
-spec build_quorum_certificate(state_data()) -> #quorum_certificate{}.
build_quorum_certificate(Data) ->
    Precommits = maps:values(Data#state_data.precommits),
    Validators = [V#consensus_vote.validator || V <- Precommits],
    Signatures = [V#consensus_vote.signature || V <- Precommits],

    BlockHash = case Data#state_data.proposal of
        undefined -> <<0:256>>;
        Block -> crypto:hash(sha256, term_to_binary(Block))
    end,

    #quorum_certificate{
        height = Data#state_data.height,
        round = Data#state_data.round,
        block_hash = BlockHash,
        vote_type = precommit,
        validators = Validators,
        signatures = Signatures,
        aggregated_signature = undefined,
        voting_power = Data#state_data.precommit_power,
        timestamp = erlang:system_time(millisecond)
    }.

%% @private
%% @doc Build round state map for get_round_state.
-spec build_round_state(phase(), state_data()) -> map().
build_round_state(Phase, Data) ->
    #{
        phase => Phase,
        round_id => Data#state_data.round_id,
        height => Data#state_data.height,
        round => Data#state_data.round,
        proposer => Data#state_data.proposer,
        has_proposal => Data#state_data.proposal =/= undefined,
        prevote_count => map_size(Data#state_data.prevotes),
        precommit_count => map_size(Data#state_data.precommits),
        prevote_power => Data#state_data.prevote_power,
        precommit_power => Data#state_data.precommit_power,
        total_power => Data#state_data.validator_set#validator_set.total_voting_power,
        quorum_threshold => Data#state_data.validator_set#validator_set.quorum_threshold,
        has_locked_block => Data#state_data.locked_block =/= undefined,
        locked_round => Data#state_data.locked_round,
        has_qc => Data#state_data.qc =/= undefined,
        started_at => Data#state_data.started_at,
        duration_ms => erlang:system_time(millisecond) - Data#state_data.started_at
    }.

%% @private
%% @doc Generate a ULID (Universally Unique Lexicographically Sortable ID).
-spec generate_ulid() -> binary().
generate_ulid() ->
    Bytes = crypto:strong_rand_bytes(16),
    <<A:32, B:16, C:16, D:16, E:48>> = Bytes,
    list_to_binary(
        io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                      [A, B, C, D, E])
    ).

%% @private
%% @doc Format address for logging (truncated for readability).
-spec format_address(binary()) -> binary().
format_address(Addr) when byte_size(Addr) >= 8 ->
    <<Prefix:4/binary, _Rest/binary>> = Addr,
    <<Prefix/binary, "...">>;
format_address(Addr) ->
    Addr.
