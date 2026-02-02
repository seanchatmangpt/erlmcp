-module(erlmcp_unwrap_utils).
-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1]).
-export([unwrap_tuple/2, unwrap_binary/2]).
-export([unwrap_tuple_nested/2, unwrap_binary_nested/2]).
-export([extract_rpc_response/1, extract_rpc_error/1]).
-export([extract_nested/3, safe_extract/3]).
-export([get_in/2, safe_get_in/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type definitions
-type unwrap_result() :: {ok, term()} | {error, {invalid_structure, term()}}.
-type nested_path() :: [non_neg_integer() | binary() | atom()].

-record(state, {
    cache :: ets:tid()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the unwrap utilities server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start the unwrap utilities server with options
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Safely unwrap a tuple element by index (1-based)
%% Returns {ok, Element} or {error, {invalid_structure, Term}}
%%
%% Example:
%%   > erlmcp_unwrap_utils:unwrap_tuple({a, b, c}, 2).
%%   {ok, b}
%%   > erlmcp_unwrap_utils:unwrap_tuple(not_a_tuple, 1).
%%   {error, {invalid_structure, not_a_tuple}}
-spec unwrap_tuple(term(), non_neg_integer()) -> unwrap_result().
unwrap_tuple(Term, Index) when is_integer(Index), Index >= 1 ->
    try
        case tuple_size(Term) of
            Size when Index =< Size ->
                {ok, element(Index, Term)};
            _ ->
                {error, {invalid_structure, {index_out_of_bounds, Index, tuple_size(Term)}}}
        end
    catch
        error:badarg ->
            {error, {invalid_structure, {not_a_tuple, Term}}}
    end.

%% @doc Safely unwrap a binary byte by position (0-based)
%% Returns {ok, Byte} or {error, {invalid_structure, Binary}}
%%
%% Example:
%%   > erlmcp_unwrap_utils:unwrap_binary(<<"abc">>, 1).
%%   {ok, 98}
%%   > erlmcp_unwrap_utils:unwrap_binary(not_a_binary, 0).
%%   {error, {invalid_structure, not_a_binary}}
-spec unwrap_binary(term(), non_neg_integer()) -> unwrap_result().
unwrap_binary(Binary, Position) when is_integer(Position), Position >= 0 ->
    try
        ByteSize = byte_size(Binary),
        case Position < ByteSize of
            true ->
                <<_:Position/binary, Byte:8, _/binary>> = Binary,
                {ok, Byte};
            false ->
                {error, {invalid_structure, {position_out_of_bounds, Position, ByteSize}}}
        end
    catch
        error:badarg ->
            {error, {invalid_structure, {not_a_binary, Binary}}}
    end.

%% @doc Safely unwrap nested tuple structures by path
%% Path is a list of tuple indices (1-based)
%%
%% Example:
%%   > erlmcp_unwrap_utils:unwrap_tuple_nested({{a, b}, {c, d}}, [2, 1]).
%%   {ok, c}
-spec unwrap_tuple_nested(term(), [non_neg_integer()]) -> unwrap_result().
unwrap_tuple_nested(Term, Path) when is_list(Path) ->
    unwrap_tuple_nested(Term, Path, []).

unwrap_tuple_nested(Term, [], _Acc) ->
    {ok, Term};
unwrap_tuple_nested(Term, [Index | Rest], Acc) ->
    case unwrap_tuple(Term, Index) of
        {ok, Element} ->
            unwrap_tuple_nested(Element, Rest, [Index | Acc]);
        {error, Reason} ->
            {error, {invalid_structure, {lists:reverse([Index | Acc]), Term, Reason}}}
    end.

%% @doc Safely unwrap nested binary structures by path
%% Path is a list of byte positions (0-based)
%%
%% Example:
%%   > erlmcp_unwrap_utils:unwrap_binary_nested(<<"abcd">>, [0, 2]).
%%   {ok, [97, 99]}
-spec unwrap_binary_nested(term(), [non_neg_integer()]) -> unwrap_result().
unwrap_binary_nested(Binary, Path) when is_list(Path) ->
    unwrap_binary_nested(Binary, Path, []).

unwrap_binary_nested(_Binary, [], Acc) when is_list(Acc) ->
    {ok, lists:reverse(Acc)};
unwrap_binary_nested(Binary, [Position | Rest], Acc) ->
    case unwrap_binary(Binary, Position) of
        {ok, Byte} ->
            unwrap_binary_nested(Binary, Rest, [Byte | Acc]);
        {error, Reason} ->
            {error, {invalid_structure, {lists:reverse([Position | Acc]), Binary, Reason}}}
    end.

%% @doc Extract result from JSON-RPC response
%% Returns {ok, Result} or {error, invalid_response}
%%
%% Example:
%%   > Response = #{result => #{data => <<"value">>}, id => 1}.
%%   > erlmcp_unwrap_utils:extract_rpc_response(Response).
%%   {ok, #{data => <<"value">>}}
-spec extract_rpc_response(term()) -> {ok, term()} | {error, invalid_response}.
extract_rpc_response({ok, Response}) when is_map(Response) ->
    case maps:get(<<"result">>, Response, undefined) of
        undefined ->
            case maps:get(result, Response, undefined) of
                undefined ->
                    {error, invalid_response};
                Result ->
                    {ok, Result}
            end;
        Result ->
            {ok, Result}
    end;
extract_rpc_response(Response) when is_map(Response) ->
    extract_rpc_response({ok, Response});
extract_rpc_response(_) ->
    {error, invalid_response}.

%% @doc Extract error from JSON-RPC response
%% Returns {ok, ErrorMap} or {error, invalid_response}
-spec extract_rpc_error(term()) -> {ok, map()} | {error, invalid_response}.
extract_rpc_error({ok, Response}) when is_map(Response) ->
    case maps:get(<<"error">>, Response, undefined) of
        undefined ->
            case maps:get(error, Response, undefined) of
                undefined ->
                    {error, invalid_response};
                ErrorMap ->
                    {ok, ErrorMap}
            end;
        ErrorMap ->
            {ok, ErrorMap}
    end;
extract_rpc_error(Response) when is_map(Response) ->
    extract_rpc_error({ok, Response});
extract_rpc_error(_) ->
    {error, invalid_response}.

%% @doc Extract nested value from complex structure using path
%% Path can contain tuple indices (integers), map keys (atoms/binaries)
%%
%% Example:
%%   > Data = #{outer => {inner, <<"value">>}}.
%%   > erlmcp_unwrap_utils:extract_nested(Data, [outer, 2]).
%%   {ok, <<"value">>}
-spec extract_nested(term(), nested_path(), term()) -> {ok, term()} | {error, term()}.
extract_nested(Term, Path, Default) ->
    extract_nested(Term, Path, Default, []).

extract_nested(Term, [], _Default, _Acc) ->
    {ok, Term};
extract_nested(Term, [Key | Rest], Default, Acc) when is_atom(Key); is_binary(Key) ->
    case extract_map_key(Term, Key) of
        {ok, Value} ->
            extract_nested(Value, Rest, Default, [Key | Acc]);
        {error, _} ->
            {ok, Default}
    end;
extract_nested(Term, [Index | Rest], Default, Acc) when is_integer(Index) ->
    case unwrap_tuple(Term, Index) of
        {ok, Element} ->
            extract_nested(Element, Rest, Default, [Index | Acc]);
        {error, _} ->
            {ok, Default}
    end.

%% @doc Safe extraction with detailed error tracking
-spec safe_extract(term(), nested_path(), fun()) -> {ok, term()} | {error, term()}.
safe_extract(Term, Path, OnErrorFun) when is_function(OnErrorFun, 1) ->
    safe_extract(Term, Path, [], OnErrorFun).

safe_extract(Term, [], _Acc, _OnErrorFun) ->
    {ok, Term};
safe_extract(Term, [Key | Rest], Acc, OnErrorFun) when is_atom(Key); is_binary(Key) ->
    case extract_map_key(Term, Key) of
        {ok, Value} ->
            safe_extract(Value, Rest, [Key | Acc], OnErrorFun);
        {error, Reason} ->
            OnErrorFun({invalid_path, [Key | lists:reverse(Acc)], Term, Reason})
    end;
safe_extract(Term, [Index | Rest], Acc, OnErrorFun) when is_integer(Index) ->
    case unwrap_tuple(Term, Index) of
        {ok, Element} ->
            safe_extract(Element, Rest, [Index | Acc], OnErrorFun);
        {error, Reason} ->
            OnErrorFun({invalid_path, [Index | lists:reverse(Acc)], Term, Reason})
    end.

%% @doc Get nested value from structure (similar to Clojure's get-in)
%% Returns {ok, Value} or {error, not_found}
%%
%% Example:
%%   > Data = #{user => #{profile => #{name => <<"Alice">>}}}.
%%   > erlmcp_unwrap_utils:get_in(Data, [user, profile, name]).
%%   {ok, <<"Alice">>}
-spec get_in(term(), nested_path()) -> {ok, term()} | {error, not_found}.
get_in(Term, Path) ->
    get_in(Term, Path, []).

get_in(Term, [], _Acc) ->
    {ok, Term};
get_in(Term, [Key | Rest], Acc) when is_atom(Key); is_binary(Key) ->
    case extract_map_key(Term, Key) of
        {ok, Value} ->
            get_in(Value, Rest, [Key | Acc]);
        {error, Reason} ->
            {error, {not_found, lists:reverse([Key | Acc]), Reason}}
    end;
get_in(Term, [Index | Rest], Acc) when is_integer(Index) ->
    case unwrap_tuple(Term, Index) of
        {ok, Element} ->
            get_in(Element, Rest, [Index | Acc]);
        {error, Reason} ->
            {error, {not_found, lists:reverse([Index | Acc]), Reason}}
    end.

%% @doc Safe get-in with default value
-spec safe_get_in(term(), nested_path()) -> term().
safe_get_in(Term, Path) ->
    case get_in(Term, Path) of
        {ok, Value} ->
            Value;
        {error, _} ->
            undefined
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(_Options) ->
    Cache = ets:new(?MODULE, [set, private, {read_concurrency, true}]),
    {ok, #state{cache = Cache}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{cache = Cache}) ->
    ets:delete(Cache),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Extract key from map (supports both atom and binary keys)
-spec extract_map_key(term(), atom() | binary()) -> {ok, term()} | {error, term()}.
extract_map_key(Map, Key) when is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined ->
            % Try with atom/binary conversion
            ConvertedKey = convert_key(Key),
            case maps:get(ConvertedKey, Map, undefined) of
                undefined ->
                    {error, {key_not_found, Key}};
                Value ->
                    {ok, Value}
            end;
        Value ->
            {ok, Value}
    end;
extract_map_key(NotMap, _Key) ->
    {error, {not_a_map, NotMap}}.

%% @doc Convert key between atom and binary
-spec convert_key(atom() | binary()) -> atom() | binary().
convert_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
convert_key(Key) when is_binary(Key) ->
    try binary_to_existing_atom(Key, utf8) of
        Atom -> Atom
    catch
        error:badarg -> Key
    end.
