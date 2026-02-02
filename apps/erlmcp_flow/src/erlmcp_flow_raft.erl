%%%-------------------------------------------------------------------
%%% @doc Minimal Raft Consensus - Leader Election Only
%%% 80/20 Implementation: ~100 LOC, pure module (no gen_server)
%%% Scope: Leader election with quorum, no log replication
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_raft).

-export([
    start_election/1,
    heartbeat/2,
    is_leader/1
]).

-include_lib("kernel/include/logger.hrl").

-define(ELECTION_TIMEOUT_MIN, 150).
-define(ELECTION_TIMEOUT_MAX, 300).
-define(HEARTBEAT_INTERVAL, 100).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start election among nodes, return leader or none
%% Election uses random timeout (150-300ms) and requires quorum (N/2+1)
-spec start_election([node()]) -> {ok, Leader :: node()} | {error, no_quorum}.
start_election([]) ->
    {error, no_quorum};
start_election(Nodes) ->
    Term = erlang:system_time(millisecond),
    Self = node(),

    ?LOG_INFO("Starting election: self=~p, nodes=~p, term=~p", [Self, Nodes, Term]),

    % Request votes from all peers (including self)
    AllNodes = lists:usort([Self | Nodes]),
    Votes = request_votes(AllNodes, Term),

    Quorum = quorum_size(length(AllNodes)),
    VoteCount = length(Votes),

    ?LOG_DEBUG("Election results: votes=~p, quorum=~p", [VoteCount, Quorum]),

    case VoteCount >= Quorum of
        true ->
            ?LOG_INFO("Election won: self=~p, votes=~p/~p", [Self, VoteCount, length(AllNodes)]),
            {ok, Self};
        false ->
            ?LOG_WARNING("Election failed: votes=~p < quorum=~p", [VoteCount, Quorum]),
            {error, no_quorum}
    end.

%% @doc Send heartbeat from leader to nodes, check leader health
%% Returns ok if leader is alive, leader_dead if unreachable
-spec heartbeat(Leader :: node(), Nodes :: [node()]) -> ok | {error, leader_dead}.
heartbeat(Leader, Nodes) when is_atom(Leader), is_list(Nodes) ->
    case lists:member(Leader, Nodes) orelse Leader =:= node() of
        true ->
            % Check if leader responds to ping
            case net_adm:ping(Leader) of
                pong ->
                    ?LOG_DEBUG("Heartbeat ok: leader=~p", [Leader]),
                    ok;
                pang ->
                    ?LOG_WARNING("Heartbeat failed: leader=~p unreachable", [Leader]),
                    {error, leader_dead}
            end;
        false ->
            ?LOG_ERROR("Heartbeat invalid: leader=~p not in nodes=~p", [Leader, Nodes]),
            {error, leader_dead}
    end.

%% @doc Check if given process/node is the current leader
-spec is_leader(node() | pid()) -> boolean().
is_leader(Leader) when is_atom(Leader) ->
    % Simple check: if it's our node, we're the leader
    Leader =:= node();
is_leader(Pid) when is_pid(Pid) ->
    % Check if pid is on the local node
    node(Pid) =:= node().

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Calculate quorum size (N/2 + 1)
-spec quorum_size(pos_integer()) -> pos_integer().
quorum_size(N) ->
    (N div 2) + 1.

%% @doc Request votes from all nodes with timeout
-spec request_votes([node()], Term :: integer()) -> [node()].
request_votes(Nodes, Term) ->
    Self = self(),
    Timeout = random_election_timeout(),

    % Spawn vote requesters
    Refs = lists:map(fun(Node) ->
        Ref = make_ref(),
        spawn(fun() ->
            Vote = request_vote_from_node(Node, Term),
            Self ! {vote_response, Ref, Node, Vote}
        end),
        {Ref, Node}
    end, Nodes),

    % Collect votes with timeout
    collect_votes(Refs, Timeout, []).

%% @doc Request vote from a single node
-spec request_vote_from_node(node(), integer()) -> boolean().
request_vote_from_node(Node, _Term) ->
    try
        case Node =:= node() of
            true ->
                % Always vote for self
                true;
            false ->
                % In real implementation, would use gen_server:call
                % For minimal MVP, use simple ping check
                net_adm:ping(Node) =:= pong
        end
    catch
        _:_ ->
            false
    end.

%% @doc Collect vote responses with timeout
-spec collect_votes([{reference(), node()}], timeout(), [node()]) -> [node()].
collect_votes([], _Timeout, Acc) ->
    Acc;
collect_votes(Refs, Timeout, Acc) ->
    receive
        {vote_response, Ref, Node, true} ->
            case lists:keytake(Ref, 1, Refs) of
                {value, _, Remaining} ->
                    collect_votes(Remaining, Timeout, [Node | Acc]);
                false ->
                    collect_votes(Refs, Timeout, Acc)
            end;
        {vote_response, Ref, _Node, false} ->
            case lists:keytake(Ref, 1, Refs) of
                {value, _, Remaining} ->
                    collect_votes(Remaining, Timeout, Acc);
                false ->
                    collect_votes(Refs, Timeout, Acc)
            end
    after Timeout ->
        % Timeout: return votes collected so far
        Acc
    end.

%% @doc Random election timeout between 150-300ms
-spec random_election_timeout() -> pos_integer().
random_election_timeout() ->
    ?ELECTION_TIMEOUT_MIN + rand:uniform(?ELECTION_TIMEOUT_MAX - ?ELECTION_TIMEOUT_MIN).
