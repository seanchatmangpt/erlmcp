%%%-------------------------------------------------------------------
%%% @doc PQChain Mempool
%%%
%%% Transaction mempool for pending blockchain transactions.
%%% Manages transaction queue, priority ordering, and gas tracking.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_mempool).
-behaviour(gen_server).

-include("pqchain.hrl").

%% API
-export([start_link/0]).
-export([submit/1, submit/2]).
-export([get_pending/0, get_pending/1]).
-export([remove/1, remove/2]).
-export([size/0, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TABLE, pqc_mempool).

-record(state, {
    table :: ets:tid(),
    max_size :: pos_integer(),
    max_gas :: pos_integer()
}).

-type tx() :: #pqc_transaction{}.
-type tx_hash() :: binary().
-type gas_price() :: non_neg_integer().

-export_type([tx/0, tx_hash/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the mempool server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Submit a transaction to the mempool
%% Options:
%%   - gas_price => non_neg_integer() - Gas price for priority
%%   - nonce => non_neg_integer() - Transaction nonce
-spec submit(tx()) -> ok | {error, term()}.
submit(Tx) ->
    submit(Tx, #{}).

-spec submit(tx(), map()) -> ok | {error, term()}.
submit(Tx, Opts) ->
    gen_server:call(?SERVER, {submit, Tx, Opts}).

%% @doc Get all pending transactions
-spec get_pending() -> [tx()].
get_pending() ->
    get_pending(all).

%% @doc Get pending transactions with filters
%% Filters:
%%   - all - Get all pending transactions
%%   - {gas_price, MinPrice} - Get transactions above gas price
%%   - {limit, N} - Get N transactions
-spec get_pending(all | {gas_price, gas_price()} | {limit, pos_integer()}) -> [tx()].
get_pending(Filter) ->
    gen_server:call(?SERVER, {get_pending, Filter}).

%% @doc Remove a transaction from the mempool
-spec remove(tx_hash()) -> ok.
remove(TxHash) ->
    remove(TxHash, []).

-spec remove(tx_hash(), [tx_hash()]) -> ok.
remove(TxHash, TxHashes) ->
    gen_server:call(?SERVER, {remove, [TxHash | TxHashes]}).

%% @doc Get current mempool size
-spec size() -> non_neg_integer().
size() ->
    gen_server:call(?SERVER, size).

%% @doc Clear all transactions from the mempool
-spec clear() -> ok.
clear() ->
    gen_server:call(?SERVER, clear).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    Table = ets:new(?DEFAULT_TABLE, [
        set,
        protected,
        {keypos, #pqc_transaction.hash},
        {read_concurrency, true}
    ]),
    State = #state{
        table = Table,
        max_size = application:get_env(swarmflow_pqchain, mempool_max_size, 10000),
        max_gas = application:get_env(swarmflow_pqchain, mempool_max_gas, 1000000)
    },
    {ok, State}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({submit, Tx, Opts}, _From, State = #state{table = Table, max_size = MaxSize}) ->
    case ets:info(Table, size) >= MaxSize of
        true ->
            {reply, {error, mempool_full}, State};
        false ->
            TxHash = Tx#pqc_transaction.hash,
            GasPrice = maps:get(gas_price, Opts, 0),
            TxWithGas = Tx#pqc_transaction{gas_price = GasPrice},
            ets:insert(Table, TxWithGas),
            {reply, ok, State}
    end;

handle_call({get_pending, Filter}, _From, State = #state{table = Table}) ->
    Txns = ets:tab2list(Table),
    Filtered = apply_filter(Filter, Txns),
    Sorted = lists:sort(
        fun(A, B) ->
            A#pqc_transaction.gas_price >= B#pqc_transaction.gas_price
        end,
        Filtered
    ),
    {reply, Sorted, State};

handle_call({remove, TxHashes}, _From, State = #state{table = Table}) ->
    lists:foreach(fun(H) -> ets:delete(Table, H) end, TxHashes),
    {reply, ok, State};

handle_call(size, _From, State = #state{table = Table}) ->
    Size = ets:info(Table, size),
    {reply, Size, State};

handle_call(clear, _From, State = #state{table = Table}) ->
    ets:delete_all_objects(Table),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{table = Table}) ->
    %% Persist pending transactions to disk for recovery
    try
        Txns = ets:tab2list(Table),
        logger:info("Persisting ~p pending transactions", [length(Txns)]),
        %% TODO: Write to persistent storage
        ok
    catch
        _:_ ->
            ok
    end,
    ets:delete(Table),
    ok.

%% @private
-spec code_change(term() | {down, term()}, #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec apply_filter(all | {gas_price, gas_price()} | {limit, pos_integer()}, [tx()]) -> [tx()].
apply_filter(all, Txns) ->
    Txns;
apply_filter({gas_price, MinPrice}, Txns) ->
    lists:filter(fun(T) -> T#pqc_transaction.gas_price >= MinPrice end, Txns);
apply_filter({limit, N}, Txns) ->
    lists:sublist(Txns, N);
apply_filter(_, Txns) ->
    Txns.
