-module(erlmcp_sla_envelope).

-export([wrap/2, unwrap/1, validate/2, enforce_constraints/2]).

%% @doc Wrap MCP message with SLA envelope
-spec wrap(Message :: map(), SLA :: map()) -> map().
wrap(Message, SLA) ->
    #{<<"jsonrpc">> => <<"2.0">>,
      <<"sla">> => SLA,
      <<"message">> => Message,
      <<"timestamp">> => erlang:system_time(millisecond)}.

%% @doc Unwrap MCP message from SLA envelope
-spec unwrap(Envelope :: map()) -> {ok, map()} | {error, term()}.
unwrap(#{<<"message">> := Message}) ->
    {ok, Message};
unwrap(_) ->
    {error, invalid_envelope}.

%% @doc Validate SLA envelope structure
-spec validate(Envelope :: map(), Plan :: map()) -> ok | {error, term()}.
validate(#{<<"jsonrpc">> := <<"2.0">>,
           <<"sla">> := SLA,
           <<"message">> := _},
         Plan) ->
    validate_sla(SLA, Plan);
validate(_, _) ->
    {error, invalid_envelope_structure}.

validate_sla(SLA, #{<<"sla">> := PlanSLA}) ->
    case maps:get(<<"tier">>, SLA) =:= maps:get(<<"tier">>, PlanSLA) of
        true ->
            ok;
        false ->
            {error, sla_tier_mismatch}
    end;
validate_sla(_, _) ->
    {error, no_plan_sla}.

%% @doc Enforce SLA constraints on message
-spec enforce_constraints(Envelope :: map(), Constraints :: map()) -> ok | {error, term()}.
enforce_constraints(#{<<"message">> := Message}, Constraints) ->
    Size = byte_size(term_to_binary(Message)),
    MaxSize = maps:get(<<"max_message_size">>, Constraints, 1048576),
    if Size =< MaxSize ->
           ok;
       true ->
           {error, {message_too_large, Size, MaxSize}}
    end;
enforce_constraints(_, _) ->
    ok.
