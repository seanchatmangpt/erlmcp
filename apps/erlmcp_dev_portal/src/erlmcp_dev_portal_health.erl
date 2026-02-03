-module(erlmcp_dev_portal_health).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize health state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle health check requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_health(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_health(<<"/health">>, Req, State) ->
    %% Return health status
    HealthStatus = check_overall_health(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(HealthStatus), Req);

handle_get_health(<<"/health/">>, Req, State) ->
    %% Redirect to health endpoint
    cowboy_req:reply(302, #{<<"location">> => <<"/health">>}, Req);

handle_get_health(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

check_overall_health() ->
    %% Check overall system health
    HealthChecks = [
        {dev_portal, check_dev_portal()},
        {api_gateway, check_api_gateway()},
        {api_management, check_api_management()},
        {database, check_database()},
        {memory, check_memory()},
        {disk, check_disk()}
    ],

    %% Determine overall status
    Statuses = [Status || {_, Status} <- HealthChecks],
    OverallStatus = case lists:all(fun(S) -> S == ok end, Statuses) of
                        true -> ok;
                        false -> degraded
                    end,

    #{
        status => OverallStatus,
        timestamp => erlang:system_time(second),
        checks => lists:map(fun({Name, Status}) ->
            #{
                name => Name,
                status => Status,
                timestamp => erlang:system_time(second)
            }
        end, HealthChecks)
    }.

check_dev_portal() ->
    %% Check developer portal health
    try
        %% Check if processes are running
        case whereis(erlmcp_dev_portal) of
            undefined -> error;
            _ -> ok
        end
    catch
        _ -> error
    end.

check_api_gateway() ->
    %% Check API gateway health
    try
        case whereis(erlmcp_api_gateway) of
            undefined -> error;
            _ -> ok
        end
    catch
        _ -> error
    end.

check_api_management() ->
    %% Check API management health
    try
        case whereis(erlmcp_api_management) of
            undefined -> error;
            _ -> ok
        end
    catch
        _ -> error
    end.

check_database() ->
    %% Check database connectivity
    try
        case mnesia:system_info(is_running) of
            yes -> ok;
            _ -> degraded
        end
    catch
        _ -> error
    end.

check_memory() ->
    %% Check memory usage
    try
        MemoryInfo = memory(),
        TotalMemory = proplists:get_value(total, MemoryInfo, 0),
        MaxMemory = 1073741824, % 1GB
        case TotalMemory < MaxMemory of
            true -> ok;
            false -> degraded
        end
    catch
        _ -> error
    end.

check_disk() ->
    %% Check disk space
    try
        case file:read_file_info("/") of
            {ok, Info} ->
                case Info#file_info.size of
                    Size when Size < 10737418240 -> ok; % 10GB
                    _ -> degraded
                end;
            {error, _} -> error
        end
    catch
        _ -> error
    end.