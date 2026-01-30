-module(erlmcp_completion).

-include("erlmcp.hrl").

%% API exports
-export([
    register_completion/2,
    unregister_completion/2,
    list_completions/1,
    get_completion/2,
    complete/3,
    validate_completion_request/1,
    validate_completion_result/1,
    encode_completion/1,
    encode_completion_result/1,
    decode_completion_request/1
]).

%% Types
-type completion_name() :: binary().
-type completion_registry() :: #{completion_name() => completion_handler()}.

-export_type([completion_name/0, completion_registry/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Register a completion handler for a specific prompt or resource template.
%% The handler function receives a #mcp_completion_request{} and returns a #mcp_completion_result{}.
-spec register_completion(completion_registry(), completion_handler()) -> completion_registry().
register_completion(Registry, Handler) when is_function(Handler, 1) ->
    %% For now, we'll register by extracting the ref name from the handler's metadata
    %% In practice, this would be called with an explicit name
    Registry.

%% @doc Register a completion handler with an explicit name.
-spec register_completion(completion_registry(), completion_name(), completion_handler()) ->
    completion_registry().
register_completion(Registry, Name, Handler) when is_binary(Name), is_function(Handler, 1) ->
    maps:put(Name, Handler, Registry).

%% @doc Unregister a completion handler.
-spec unregister_completion(completion_registry(), completion_name()) -> completion_registry().
unregister_completion(Registry, Name) when is_binary(Name) ->
    maps:remove(Name, Registry).

%% @doc List all registered completion handlers.
-spec list_completions(completion_registry()) -> [completion_name()].
list_completions(Registry) ->
    maps:keys(Registry).

%% @doc Get a specific completion handler by name.
-spec get_completion(completion_registry(), completion_name()) ->
    {ok, completion_handler()} | {error, not_found}.
get_completion(Registry, Name) when is_binary(Name) ->
    case maps:find(Name, Registry) of
        {ok, Handler} -> {ok, Handler};
        error -> {error, not_found}
    end.

%% @doc Perform a completion request.
%% This is the main entry point for executing completion logic.
-spec complete(completion_registry(), completion_name(), #mcp_completion_request{}) ->
    {ok, #mcp_completion_result{}} | {error, term()}.
complete(Registry, Name, Request) when is_binary(Name), is_record(Request, mcp_completion_request) ->
    %% Validate request
    case validate_completion_request(Request) of
        ok ->
            %% Get handler
            case get_completion(Registry, Name) of
                {ok, Handler} ->
                    try
                        Result = Handler(Request),
                        case validate_completion_result(Result) of
                            ok -> {ok, Result};
                            {error, _} = Error -> Error
                        end
                    catch
                        error:Reason:Stacktrace ->
                            logger:error("Completion handler failed: ~p~nStacktrace: ~p",
                                        [Reason, Stacktrace]),
                            {error, {completion_failed, Reason}};
                        throw:Reason ->
                            {error, {completion_failed, Reason}};
                        exit:Reason ->
                            {error, {completion_failed, Reason}}
                    end;
                {error, not_found} ->
                    {error, {completion_not_found, Name}}
            end;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Validation Functions
%%====================================================================

%% @doc Validate a completion request structure.
-spec validate_completion_request(#mcp_completion_request{}) -> ok | {error, term()}.
validate_completion_request(#mcp_completion_request{
    ref = Ref,
    argument = Argument,
    context = Context
}) ->
    case validate_completion_ref(Ref) of
        ok ->
            case validate_completion_argument(Argument) of
                ok ->
                    validate_context(Context);
                Error -> Error
            end;
        Error -> Error
    end;
validate_completion_request(_) ->
    {error, invalid_completion_request}.

%% @doc Validate completion reference.
-spec validate_completion_ref(#mcp_completion_ref{}) -> ok | {error, term()}.
validate_completion_ref(#mcp_completion_ref{type = Type, name = Name})
  when (Type =:= ref_prompt orelse Type =:= ref_resource_template),
       is_binary(Name), byte_size(Name) > 0 ->
    ok;
validate_completion_ref(_) ->
    {error, invalid_completion_ref}.

%% @doc Validate completion argument.
-spec validate_completion_argument(#mcp_completion_argument{}) -> ok | {error, term()}.
validate_completion_argument(#mcp_completion_argument{name = Name, value = Value})
  when is_binary(Name), byte_size(Name) > 0, is_binary(Value) ->
    ok;
validate_completion_argument(_) ->
    {error, invalid_completion_argument}.

%% @doc Validate context map.
-spec validate_context(map()) -> ok | {error, term()}.
validate_context(Context) when is_map(Context) ->
    %% Ensure all keys and values are binary
    try
        maps:fold(
            fun(K, V, _Acc) when is_binary(K), is_binary(V) -> ok;
               (_, _, _) -> throw(invalid_context)
            end,
            ok,
            Context
        )
    catch
        throw:invalid_context -> {error, invalid_context}
    end;
validate_context(_) ->
    {error, invalid_context}.

%% @doc Validate a completion result structure.
-spec validate_completion_result(#mcp_completion_result{}) -> ok | {error, term()}.
validate_completion_result(#mcp_completion_result{
    completions = Completions,
    hasMore = HasMore,
    metadata = Metadata
}) when is_list(Completions), is_boolean(HasMore), is_map(Metadata) ->
    %% Validate each completion
    case validate_completions(Completions) of
        ok -> ok;
        Error -> Error
    end;
validate_completion_result(_) ->
    {error, invalid_completion_result}.

%% @doc Validate list of completions.
-spec validate_completions([#mcp_completion{}]) -> ok | {error, term()}.
validate_completions([]) ->
    ok;
validate_completions([#mcp_completion{value = Value, score = Score} = _C | Rest])
  when is_binary(Value), byte_size(Value) > 0 ->
    %% Validate score if present
    case Score of
        undefined -> validate_completions(Rest);
        S when is_float(S), S >= 0.0, S =< 1.0 -> validate_completions(Rest);
        _ -> {error, {invalid_score, Score}}
    end;
validate_completions([Invalid | _]) ->
    {error, {invalid_completion, Invalid}};
validate_completions(_) ->
    {error, invalid_completions_list}.

%%====================================================================
%% Encoding/Decoding Functions
%%====================================================================

%% @doc Encode a single completion to a map for JSON serialization.
-spec encode_completion(#mcp_completion{}) -> map().
encode_completion(#mcp_completion{
    value = Value,
    label = Label,
    description = Description,
    score = Score
}) ->
    Base = #{<<"value">> => Value},
    WithLabel = case Label of
        undefined -> Base;
        L -> Base#{<<"label">> => L}
    end,
    WithDesc = case Description of
        undefined -> WithLabel;
        D -> WithLabel#{<<"description">> => D}
    end,
    case Score of
        undefined -> WithDesc;
        S -> WithDesc#{<<"score">> => S}
    end.

%% @doc Encode a completion result to a map for JSON serialization.
-spec encode_completion_result(#mcp_completion_result{}) -> map().
encode_completion_result(#mcp_completion_result{
    completions = Completions,
    hasMore = HasMore,
    metadata = Metadata
}) ->
    EncodedCompletions = [encode_completion(C) || C <- Completions],
    Base = #{
        <<"completions">> => EncodedCompletions,
        <<"hasMore">> => HasMore
    },
    case maps:size(Metadata) of
        0 -> Base;
        _ -> Base#{<<"metadata">> => Metadata}
    end.

%% @doc Decode a completion request from a JSON map.
-spec decode_completion_request(map()) ->
    {ok, #mcp_completion_request{}} | {error, term()}.
decode_completion_request(#{
    <<"ref">> := RefMap,
    <<"argument">> := ArgMap
} = Params) ->
    case decode_completion_ref(RefMap) of
        {ok, Ref} ->
            case decode_completion_argument(ArgMap) of
                {ok, Argument} ->
                    Context = maps:get(<<"context">>, Params, #{}),
                    Request = #mcp_completion_request{
                        ref = Ref,
                        argument = Argument,
                        context = Context
                    },
                    {ok, Request};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end;
decode_completion_request(_) ->
    {error, {missing_required_fields, [<<"ref">>, <<"argument">>]}}.

%% @doc Decode completion reference from JSON map.
-spec decode_completion_ref(map()) -> {ok, #mcp_completion_ref{}} | {error, term()}.
decode_completion_ref(#{<<"type">> := TypeBin, <<"name">> := Name})
  when is_binary(TypeBin), is_binary(Name) ->
    Type = case TypeBin of
        <<"ref/prompt">> -> ref_prompt;
        <<"ref/resource_template">> -> ref_resource_template;
        _ -> undefined
    end,
    case Type of
        undefined -> {error, {invalid_ref_type, TypeBin}};
        _ -> {ok, #mcp_completion_ref{type = Type, name = Name}}
    end;
decode_completion_ref(_) ->
    {error, {missing_ref_fields, [<<"type">>, <<"name">>]}}.

%% @doc Decode completion argument from JSON map.
-spec decode_completion_argument(map()) ->
    {ok, #mcp_completion_argument{}} | {error, term()}.
decode_completion_argument(#{<<"name">> := Name, <<"value">> := Value})
  when is_binary(Name), is_binary(Value) ->
    {ok, #mcp_completion_argument{name = Name, value = Value}};
decode_completion_argument(_) ->
    {error, {missing_argument_fields, [<<"name">>, <<"value">>]}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Additional helper functions can be added here as needed
