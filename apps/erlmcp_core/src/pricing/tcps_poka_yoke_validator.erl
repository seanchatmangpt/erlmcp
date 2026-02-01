-module(tcps_poka_yoke_validator).

-export([validate/2, check_envelope/2, enforce_sla/2]).

%% @doc Validate poka-yoke constraints (error-proofing)
-spec validate(Request :: map(), Plan :: map()) -> ok | {error, term()}.
validate(Request, Plan) ->
    Checks =
        [fun() -> check_rate_limit(Request, Plan) end,
         fun() -> check_payload_size(Request, Plan) end,
         fun() -> check_capabilities(Request, Plan) end],
    run_checks(Checks).

run_checks([]) ->
    ok;
run_checks([Check | Rest]) ->
    case Check() of
        ok ->
            run_checks(Rest);
        {error, _} = Error ->
            Error
    end.

check_rate_limit(#{<<"method">> := Method}, #{<<"rate_limits">> := Limits}) ->
    case maps:get(Method, Limits, undefined) of
        undefined ->
            ok;
        Limit ->
            {ok, {rate_limit, Limit}}
    end;
check_rate_limit(_, _) ->
    ok.

check_payload_size(#{<<"params">> := Params},
                   #{<<"limits">> := #{<<"max_payload_bytes">> := Max}}) ->
    Size = byte_size(term_to_binary(Params)),
    if Size =< Max ->
           ok;
       true ->
           {error, {payload_too_large, Size, Max}}
    end;
check_payload_size(_, _) ->
    ok.

check_capabilities(#{<<"method">> := Method}, #{<<"capabilities">> := Caps}) ->
    case lists:member(Method, Caps) of
        true ->
            ok;
        false ->
            {error, {capability_not_available, Method}}
    end;
check_capabilities(_, _) ->
    ok.

%% @doc Check MCP envelope constraints
-spec check_envelope(Envelope :: map(), Plan :: map()) -> ok | {error, term()}.
check_envelope(#{<<"jsonrpc">> := <<"2.0">>}, _Plan) ->
    ok;
check_envelope(_, _) ->
    {error, invalid_jsonrpc_version}.

%% @doc Enforce SLA constraints
-spec enforce_sla(Request :: map(), SLA :: map()) -> ok | {error, term()}.
enforce_sla(_Request, #{<<"response_time_ms">> := MaxTime}) ->
    {ok, {max_response_time, MaxTime}};
enforce_sla(_, _) ->
    ok.
