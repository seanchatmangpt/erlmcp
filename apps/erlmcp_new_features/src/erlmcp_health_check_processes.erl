-module(erlmcp_health_check_processes).
-export([check/0, check/1]).

%% Process health check implementation
%% Monitors process count, supervision tree, process mailbox, and crashes

-define(WARNING_PROCESS_COUNT, 1000).
-define(ERROR_PROCESS_COUNT, 2000).
-define(MAX_MAILBOX_SIZE, 100).
-define(MAX_RESTART_COUNT, 100).


check() ->
    check(#{}).

check(Options) ->
    try
        % Get process information
        ProcessInfo = get_process_info(),

        % Check process count
        ProcessCountStatus = check_process_count(ProcessInfo),

        % Check supervision tree
        SupervisionStatus = check_supervision_tree(ProcessInfo),

        % Check process mailboxes
        MailboxStatus = check_process_mailboxes(ProcessInfo, Options),

        % Check process crashes
        CrashStatus = check_process_crashes(ProcessInfo, Options),

        % Generate summary
        CriticalCount = lists:sum([
            1 || Status <- [ProcessCountStatus, SupervisionStatus, MailboxStatus, CrashStatus],
                 Status =:= critical
        ]),
        WarningCount = lists:sum([
            1 || Status <- [ProcessCountStatus, SupervisionStatus, MailboxStatus, CrashStatus],
                 Status =:= warning
        ]),

        case CriticalCount > 0 of
            true ->
                {error, <<"Critical process issues detected">>, #{
                    critical_count => CriticalCount,
                    warning_count => WarningCount,
                    details => #{
                        process_count_status => ProcessCountStatus,
                        supervision_status => SupervisionStatus,
                        mailbox_status => MailboxStatus,
                        crash_status => CrashStatus
                    }
                }};
            _ when WarningCount > 0 ->
                {warning, <<"Process issues detected">>, #{
                    critical_count => CriticalCount,
                    warning_count => WarningCount,
                    details => #{
                        process_count_status => ProcessCountStatus,
                        supervision_status => SupervisionStatus,
                        mailbox_status => MailboxStatus,
                        crash_status => CrashStatus
                    }
                }};
            true ->
                {ok, <<"Process usage within normal bounds">>, #{
                    critical_count => CriticalCount,
                    warning_count => WarningCount,
                    details => #{
                        process_count_status => ProcessCountStatus,
                        supervision_status => SupervisionStatus,
                        mailbox_status => MailboxStatus,
                        crash_status => CrashStatus
                    }
                }}
        end
    catch
        Error:Reason ->
            {error, io_lib:format("Process check failed: ~p:~p", [Error, Reason]), #{error => {Error, Reason}}}
    end.


%% Process info records
-record(process_info, {
    total_count :: non_neg_integer(),
    processes :: list(),
    supervisors :: list(),
    workers :: list(),
    mailbox_sizes :: map(),
    crash_counts :: map()
}).

get_process_info() ->
    % Get all processes
    AllProcesses = processes(),
    SupervisorProcesses = supervisors(),
    WorkerProcesses = AllProcesses -- SupervisorProcesses,

    % Get mailbox sizes for important processes
    MailboxSizes = get_mailbox_sizes(SupervisorProcesses ++ WorkerProcesses),

    % Get crash counts
    CrashCounts = get_crash_counts(SupervisorProcesses),

    #process_info{
        total_count = length(AllProcesses),
        processes = AllProcesses,
        supervisors = SupervisorProcesses,
        workers = WorkerProcesses,
        mailbox_sizes = MailboxSizes,
        crash_counts = CrashCounts
    }.

processes() ->
    % Get all processes with basic info
    lists:filtermap(fun(Pid) ->
        case process_info(Pid, [status, message_queue_len, registered_name]) of
            Info when is_list(Info) ->
                {true, {Pid, Info}};
            _ ->
                false
        end
    end, erlang:processes()).

supervisors() ->
    % Find supervisor processes
    lists:filtermap(fun(Pid) ->
        case process_info(Pid, [dictionary]) of
            Info when is_list(Info) ->
                Dict = proplists:get_value(dictionary, Info, []),
                case lists:keymember('$ancestors', 1, Dict) of
                    true ->
                        {true, {Pid, Info}};
                    false ->
                        false
                end;
            _ ->
                false
        end
    end, erlang:processes()).

get_mailbox_sizes(Processes) ->
    % Get mailbox sizes for processes
    lists:foldl(fun({Pid, Info}, Acc) ->
        MailboxSize = proplists:get_value(message_queue_len, Info, 0),
        maps:put(Pid, MailboxSize, Acc)
    end, #{}, Processes).

get_crash_counts(Supervisors) ->
    % Get crash counts from supervisors
    lists:foldl(fun({Pid, _}, Acc) ->
        % Get crash count from supervisor's dictionary
        case process_info(Pid, [dictionary]) of
            Info when is_list(Info) ->
                Dict = proplists:get_value(dictionary, Info, []),
                case lists:keyfind('$crash_count', 1, Dict) of
                    {_, Count} ->
                        maps:put(Pid, Count, Acc);
                    false ->
                        Acc
                end;
            _ ->
                Acc
        end
    end, #{}, Supervisors).

check_process_count(#process_info{total_count = TotalCount}) ->
    if
        TotalCount > ?ERROR_PROCESS_COUNT -> error;
        TotalCount > ?WARNING_PROCESS_COUNT -> warning;
        true -> ok
    end.

check_supervision_tree(#process_info{supervisors = Supervisors, workers = Workers}) ->
    % Check supervision tree health
    SupervisorCount = length(Supervisors),
    WorkerCount = length(Workers),

    % Check for unresponsive supervisors
    UnresponsiveSupervisors = get_unresponsive_supervisors(Supervisors),
    UnresponsiveCount = length(UnresponsiveSupervisors),

    % Check for missing expected workers
    MissingWorkers = check_missing_workers(Supervisors, Workers),

    % Generate status
    if
        UnresponsiveCount > 0 orelse MissingWorkers > 0 -> error;
        WorkerCount > SupervisorCount * 10 -> warning;
        true -> ok
    end.

get_unresponsive_supervisors(Supervisors) ->
    % Check if supervisors are responsive
    lists:filter(fun({Pid, _}) ->
        case process_info(Pid, [status]) of
            {status, Status} when Status =:= waiting orelse Status =:= runnable ->
                % Check if supervisor has been running recently
                case process_info(Pid, [dictionary]) of
                    Info when is_list(Info) ->
                        Dict = proplists:get_value(dictionary, Info, []),
                        case lists:keyfind('$last_restart', 1, Dict) of
                            {_, Timestamp} ->
                                % Check if recent activity
                                erlang:monotonic_time(millisecond) - Timestamp < 30000;
                            false ->
                                false
                        end;
                    _ ->
                        false
                end;
            _ ->
                false
        end
    end, Supervisors).

check_missing_workers(Supervisors, Workers) ->
    % Check if expected workers are missing
    % This would require knowledge of expected workers per supervisor
    0.

check_process_mailboxes(#process_info{mailbox_sizes = MailboxSizes}, _Options) ->
    % Check process mailbox sizes
    LargeMailboxes = maps:filter(fun(_Pid, Size) ->
        Size > ?MAX_MAILBOX_SIZE
    end, MailboxSizes),

    case maps:size(LargeMailboxes) of
        0 -> ok;
        N when N > 10 -> error;
        _ -> warning
    end.

check_process_crashes(#process_info{crash_counts = CrashCounts}, _Options) ->
    % Check process crash counts
    HighCrashSupervisors = maps:filter(fun(_Pid, Count) ->
        Count > ?MAX_RESTART_COUNT
    end, CrashCounts),

    case maps:size(HighCrashSupervisors) of
        0 -> ok;
        N when N > 5 -> error;
        _ -> warning
    end.