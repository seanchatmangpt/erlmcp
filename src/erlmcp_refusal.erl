%%%-------------------------------------------------------------------
%% @doc Refusal Taxonomy - Utilities for Consistent Error Handling (v1.4.0)
%%
%% Provides functions for creating, formatting, and managing refusal
%% responses across the erlmcp system with consistent codes, HTTP status,
%% remediation hints, and severity levels.
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_refusal).

-include("erlmcp_refusal.hrl").
-include_lib("kernel/include/logger.hrl").

%% API - Lookup Functions
-export([
    get_http_status/1,
    get_message/1,
    get_hint/1,
    get_severity/1,
    get_metadata/1,
    lookup_refusal/1
]).

%% API - Creation Functions
-export([
    create_refusal/1,
    create_refusal/2,
    create_refusal/3,
    format_refusal/1,
    format_refusal_json/1
]).

%% API - Validation Functions
-export([
    is_valid_code/1,
    is_critical/1,
    is_auth_failure/1,
    is_security_issue/1,
    is_rate_limit/1,
    is_queue_limit/1
]).

%% API - Logging Functions
-export([
    log_refusal/1,
    log_refusal/2,
    log_refusal_with_context/3
]).

%% Type exports (refusal_code and refusal exported from erlmcp_refusal.hrl)
-export_type([
    refusal_category/0
]).

-type refusal_category() :: queue | auth | validation | security | resource | rate_limit | protocol | server | circuit_breaker.

%%====================================================================
%% Lookup Functions
%%====================================================================

%% @doc Get HTTP status code for refusal code
-spec get_http_status(refusal_code()) -> pos_integer() | error.
get_http_status(Code) ->
    case lookup_refusal(Code) of
        {ok, _HTTPStatus, _Message, _Hint, _Severity} = Result ->
            {ok, element(2, Result)};
        error ->
            error
    end.

%% @doc Get error message for refusal code
-spec get_message(refusal_code()) -> binary() | error.
get_message(Code) ->
    case lookup_refusal(Code) of
        {ok, _HTTPStatus, Message, _Hint, _Severity} ->
            {ok, Message};
        error ->
            error
    end.

%% @doc Get remediation hint for refusal code
-spec get_hint(refusal_code()) -> binary() | error.
get_hint(Code) ->
    case lookup_refusal(Code) of
        {ok, _HTTPStatus, _Message, Hint, _Severity} ->
            {ok, Hint};
        error ->
            error
    end.

%% @doc Get severity level for refusal code
-spec get_severity(refusal_code()) -> warn | error | critical | not_found.
get_severity(Code) ->
    case lookup_refusal(Code) of
        {ok, _HTTPStatus, _Message, _Hint, Severity} ->
            {ok, Severity};
        error ->
            not_found
    end.

%% @doc Get full metadata for refusal code
-spec get_metadata(refusal_code()) -> {ok, map()} | error.
get_metadata(Code) ->
    case lookup_refusal(Code) of
        {ok, HTTPStatus, Message, Hint, Severity} ->
            {ok, #{
                code => Code,
                http_status => HTTPStatus,
                message => Message,
                hint => Hint,
                severity => Severity,
                category => categorize_code(Code)
            }};
        error ->
            error
    end.

%% @doc Look up refusal metadata from table
-spec lookup_refusal(refusal_code()) ->
    {ok, pos_integer(), binary(), binary(), atom()} | error.
lookup_refusal(Code) when is_integer(Code), Code >= 1001, Code =< 1095 ->
    case lists:keyfind(Code, 1, ?REFUSAL_METADATA) of
        {Code, HTTPStatus, Message, Hint, Severity} ->
            {ok, HTTPStatus, Message, Hint, Severity};
        false ->
            error
    end;
lookup_refusal(_) ->
    error.

%%====================================================================
%% Creation Functions
%%====================================================================

%% @doc Create refusal with code only
-spec create_refusal(refusal_code()) -> {ok, refusal()} | error.
create_refusal(Code) ->
    create_refusal(Code, undefined, undefined).

%% @doc Create refusal with code and details
-spec create_refusal(refusal_code(), map() | undefined) -> {ok, refusal()} | error.
create_refusal(Code, Details) ->
    create_refusal(Code, Details, undefined).

%% @doc Create refusal with code, details, and custom message
-spec create_refusal(refusal_code(), map() | undefined, binary() | undefined) ->
    {ok, refusal()} | error.
create_refusal(Code, Details, CustomMsg) ->
    case lookup_refusal(Code) of
        {ok, HTTPStatus, DefaultMsg, Hint, Severity} ->
            Message = case CustomMsg of
                undefined -> DefaultMsg;
                CustomMsg -> CustomMsg
            end,
            Refusal = #refusal{
                code = Code,
                http_status = HTTPStatus,
                message = Message,
                hint = Hint,
                severity = Severity,
                details = Details,
                timestamp = erlang:system_time(millisecond)
            },
            {ok, Refusal};
        error ->
            error
    end.

%% @doc Format refusal as human-readable string
-spec format_refusal(refusal()) -> binary().
format_refusal(#refusal{code = Code, message = Msg, hint = Hint, severity = Severity}) ->
    Sev = atom_to_binary(Severity, utf8),
    iolist_to_binary([
        <<"[">>, Sev, <<"] ">>,
        <<"Code ">>, integer_to_binary(Code), <<": ">>,
        Msg, <<"\n">>,
        <<"Tip: ">>, Hint
    ]).

%% @doc Format refusal as JSON for HTTP responses
-spec format_refusal_json(refusal()) -> map().
format_refusal_json(#refusal{
    code = Code,
    http_status = HTTPStatus,
    message = Message,
    hint = Hint,
    severity = Severity,
    details = Details,
    timestamp = Timestamp
}) ->
    BaseMap = #{
        error_code => Code,
        http_status => HTTPStatus,
        message => Message,
        remediation_hint => Hint,
        severity => Severity,
        timestamp_ms => Timestamp
    },
    case Details of
        undefined -> BaseMap;
        Details -> maps:put(details, Details, BaseMap)
    end.

%%====================================================================
%% Validation Functions
%%====================================================================

%% @doc Check if code is valid
-spec is_valid_code(term()) -> boolean().
is_valid_code(Code) when is_integer(Code), Code >= 1001, Code =< 1095 ->
    case lists:keyfind(Code, 1, ?REFUSAL_METADATA) of
        {Code, _, _, _, _} -> true;
        false -> false
    end;
is_valid_code(_) ->
    false.

%% @doc Check if code is critical severity
-spec is_critical(refusal_code()) -> boolean().
is_critical(Code) ->
    case get_severity(Code) of
        {ok, critical} -> true;
        _ -> false
    end.

%% @doc Check if code is authentication failure
-spec is_auth_failure(refusal_code()) -> boolean().
is_auth_failure(Code) when Code >= 1011, Code =< 1016 ->
    true;
is_auth_failure(_) ->
    false.

%% @doc Check if code is security issue (path traversal, symlink, etc.)
-spec is_security_issue(refusal_code()) -> boolean().
is_security_issue(Code) when Code >= 1036, Code =< 1040 ->
    true;
is_security_issue(1015) -> true;  % Missing auth
is_security_issue(1016) -> true;  % Invalid session
is_security_issue(_) ->
    false.

%% @doc Check if code is rate limiting
-spec is_rate_limit(refusal_code()) -> boolean().
is_rate_limit(Code) when Code >= 1056, Code =< 1060 ->
    true;
is_rate_limit(_) ->
    false.

%% @doc Check if code is queue limit
-spec is_queue_limit(refusal_code()) -> boolean().
is_queue_limit(Code) when Code >= 1001, Code =< 1005 ->
    true;
is_queue_limit(_) ->
    false.

%%====================================================================
%% Logging Functions
%%====================================================================

%% @doc Log refusal to logger
-spec log_refusal(refusal()) -> ok.
log_refusal(#refusal{code = Code, severity = Severity, message = Msg}) ->
    LogFun = case Severity of
        critical -> error;
        error -> error;
        warn -> warning
    end,
    logger:LogFun("Refusal [~w]: ~s", [Code, Msg]).

%% @doc Log refusal with custom context
-spec log_refusal(refusal(), string()) -> ok.
log_refusal(#refusal{code = Code, severity = Severity, message = Msg}, Context) ->
    LogFun = case Severity of
        critical -> error;
        error -> error;
        warn -> warning
    end,
    logger:LogFun("Refusal [~w] ~s: ~s", [Code, Context, Msg]).

%% @doc Log refusal with full context and details
-spec log_refusal_with_context(refusal(), string(), map()) -> ok.
log_refusal_with_context(Refusal, Context, ContextDetails) ->
    #refusal{code = Code, severity = Severity, message = Msg} = Refusal,
    LogFun = case Severity of
        critical -> error;
        error -> error;
        warn -> warning
    end,
    logger:LogFun("Refusal [~w] ~s: ~s~n~p", [Code, Context, Msg, ContextDetails]).

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @doc Categorize refusal code
-spec categorize_code(refusal_code()) -> refusal_category().
categorize_code(Code) when Code >= 1001, Code =< 1005 -> queue;
categorize_code(Code) when Code >= 1011, Code =< 1016 -> auth;
categorize_code(Code) when Code >= 1021, Code =< 1029 -> validation;
categorize_code(Code) when Code >= 1036, Code =< 1040 -> security;
categorize_code(Code) when Code >= 1046, Code =< 1052 -> resource;
categorize_code(Code) when Code >= 1056, Code =< 1060 -> rate_limit;
categorize_code(Code) when Code >= 1066, Code =< 1070 -> protocol;
categorize_code(Code) when Code >= 1076, Code =< 1080 -> server;
categorize_code(Code) when Code >= 1086, Code =< 1089 -> circuit_breaker;
categorize_code(_) -> error.
