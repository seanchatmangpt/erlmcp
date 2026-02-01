-module(erlmcp_pricing_util).

-export([format_price/1, calculate_prorated_cost/3, days_in_month/2, validate_plan_name/1,
         merge_usage/2]).

%% @doc Format price for display (cents to dollars)
-spec format_price(Cents :: integer()) -> binary().
format_price(Cents) when is_integer(Cents) ->
    Dollars = Cents / 100,
    list_to_binary(io_lib:format("$~.2f", [Dollars])).

%% @doc Calculate prorated cost for partial month
-spec calculate_prorated_cost(MonthlyCost :: integer(),
                              DaysUsed :: integer(),
                              DaysInMonth :: integer()) ->
                                 integer().
calculate_prorated_cost(MonthlyCost, DaysUsed, DaysInMonth) ->
    round(MonthlyCost * DaysUsed / DaysInMonth).

%% @doc Get number of days in month
-spec days_in_month(Year :: integer(), Month :: integer()) -> integer().
days_in_month(Year, Month) ->
    case Month of
        2 ->
            case is_leap_year(Year) of
                true ->
                    29;
                false ->
                    28
            end;
        M when M =:= 4; M =:= 6; M =:= 9; M =:= 11 ->
            30;
        _ ->
            31
    end.

is_leap_year(Year) ->
    Year rem 4 =:= 0 andalso Year rem 100 =/= 0 orelse Year rem 400 =:= 0.

%% @doc Validate plan name format
-spec validate_plan_name(Name :: atom() | binary()) -> ok | {error, term()}.
validate_plan_name(Name) when is_atom(Name) ->
    validate_plan_name(atom_to_binary(Name, utf8));
validate_plan_name(Name) when is_binary(Name) ->
    case lists:member(Name, [<<"free">>, <<"pro">>, <<"enterprise">>]) of
        true ->
            ok;
        false ->
            {error, invalid_plan_name}
    end;
validate_plan_name(_) ->
    {error, invalid_plan_name}.

%% @doc Merge two usage maps
-spec merge_usage(Usage1 :: map(), Usage2 :: map()) -> map().
merge_usage(Usage1, Usage2) ->
    maps:fold(fun(Key, Value, Acc) -> maps:update_with(Key, fun(V) -> V + Value end, Value, Acc)
              end,
              Usage1,
              Usage2).
