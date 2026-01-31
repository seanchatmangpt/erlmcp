%%%-------------------------------------------------------------------
%%% @doc
%%% Error handling and formatting utilities for erlmcp.
%%% Provides functions for creating standardized error responses.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_errors).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([format_error/1]).
-export([is_refusal_error/1]).
-export([refusal_reason/1]).
-export([error_code/1]).
-export([error_message/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the error manager server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Formats an error into a standard error response map.
-spec format_error({atom(), term()}) -> map().
format_error({ErrorCode, ErrorMsg}) when is_atom(ErrorCode), is_binary(ErrorMsg) ->
    #{
        <<"code">> => error_code(ErrorCode),
        <<"message">> => ErrorMsg,
        <<"data">> => #{}
    };
format_error({ErrorCode, ErrorMsg, Data}) when is_atom(ErrorCode), is_binary(ErrorMsg), is_map(Data) ->
    #{
        <<"code">> => error_code(ErrorCode),
        <<"message">> => ErrorMsg,
        <<"data">> => Data
    }.

%% @doc Checks if an error code is a refusal error (1001-1089).
-spec is_refusal_error(integer()) -> boolean().
is_refusal_error(Code) when is_integer(Code), Code >= 1001, Code =< 1089 ->
    true;
is_refusal_error(_) ->
    false.

%% @doc Returns the refusal reason for a refusal error code.
-spec refusal_reason(integer()) -> binary() | undefined.
refusal_reason(1001) -> <<"Content violates policy">>;
refusal_reason(1002) -> <<"Violates safety guidelines">>;
refusal_reason(1003) -> <<"Rate limit exceeded">>;
refusal_reason(1004) -> <<"Resource constraints">>;
refusal_reason(1005) -> <<"Permission denied">>;
refusal_reason(1006) -> <<"Invalid input">>;
refusal_reason(1007) -> <<"Unsupported operation">>;
refusal_reason(1008) -> <<"Temporarily unavailable">>;
refusal_reason(1009) -> <<"Dependency failed">>;
refusal_reason(1010) -> <<"Custom refusal">>;
refusal_reason(Code) when is_integer(Code), Code >= 1001, Code =< 1089 -> <<"Tool refused">>;
refusal_reason(_) -> undefined.

%% @doc Converts an error atom to its numeric error code.
-spec error_code(atom()) -> integer().
error_code(parse_error) -> -32700;
error_code(invalid_request) -> -32600;
error_code(method_not_found) -> -32601;
error_code(invalid_params) -> -32602;
error_code(internal_error) -> -32603;
error_code(resource_not_found) -> -32001;
error_code(tool_not_found) -> -32002;
error_code(not_initialized) -> -32005;
error_code(validation_failed) -> -32007;
error_code(rate_limited) -> -32010;
error_code(unauthorized) -> -32051;
error_code(_) -> -32603.

%% @doc Returns a human-readable error message for an error code.
-spec error_message(atom()) -> binary().
error_message(parse_error) -> <<"Parse error">>;
error_message(invalid_request) -> <<"Invalid request">>;
error_message(method_not_found) -> <<"Method not found">>;
error_message(invalid_params) -> <<"Invalid params">>;
error_message(internal_error) -> <<"Internal error">>;
error_message(resource_not_found) -> <<"Resource not found">>;
error_message(tool_not_found) -> <<"Tool not found">>;
error_message(not_initialized) -> <<"Not initialized">>;
error_message(validation_failed) -> <<"Validation failed">>;
error_message(rate_limited) -> <<"Rate limited">>;
error_message(unauthorized) -> <<"Unauthorized">>;
error_message(_) -> <<"Unknown error">>.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, #{}}.

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
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
