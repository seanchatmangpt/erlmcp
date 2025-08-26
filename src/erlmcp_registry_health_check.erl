-module(erlmcp_registry_health_check).

%% API for registry health checks
-export([health_check/0, is_healthy/0, get_status/0]).

-include_lib("kernel/include/logger.hrl").

%% Health check for registry component
-spec health_check() -> healthy | unhealthy | {error, term()}.
health_check() ->
    try
        % Check if registry process is alive
        case whereis(erlmcp_registry) of
            undefined ->
                unhealthy;
            Pid when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    false ->
                        unhealthy;
                    true ->
                        % Test basic registry functionality
                        test_registry_operations()
                end
        end
    catch
        Class:Reason ->
            ?LOG_ERROR("Registry health check failed: ~p:~p", [Class, Reason]),
            {error, {health_check_failed, {Class, Reason}}}
    end.

%% Simple boolean health check
-spec is_healthy() -> boolean().
is_healthy() ->
    health_check() =:= healthy.

%% Get detailed health status
-spec get_status() -> map().
get_status() ->
    Status = health_check(),
    Pid = whereis(erlmcp_registry),
    
    BaseStatus = #{
        component => registry,
        status => Status,
        pid => Pid,
        timestamp => erlang:timestamp()
    },
    
    case Pid of
        undefined ->
            BaseStatus#{
                alive => false,
                memory => 0,
                message_queue_len => 0
            };
        _ when is_pid(Pid) ->
            try
                ProcessInfo = process_info(Pid, [memory, message_queue_len, links]),
                Memory = proplists:get_value(memory, ProcessInfo, 0),
                MessageQueueLen = proplists:get_value(message_queue_len, ProcessInfo, 0),
                Links = proplists:get_value(links, ProcessInfo, []),
                
                BaseStatus#{
                    alive => is_process_alive(Pid),
                    memory => Memory,
                    message_queue_len => MessageQueueLen,
                    linked_processes => length(Links)
                }
            catch
                _:_ ->
                    BaseStatus#{
                        alive => false,
                        memory => 0,
                        message_queue_len => 0
                    }
            end
    end.

%% Internal function to test registry operations
-spec test_registry_operations() -> healthy | unhealthy.
test_registry_operations() ->
    try
        % Test basic registry call
        TestResult = gen_server:call(erlmcp_registry, {test_health_check}),
        case TestResult of
            {error, unknown_request} ->
                % Expected response for unknown request - registry is responding
                healthy;
            _ ->
                % Any other response indicates registry is alive and responding
                healthy
        end
    catch
        exit:{timeout, _} ->
            unhealthy;
        exit:{noproc, _} ->
            unhealthy;
        _:_ ->
            unhealthy
    end.