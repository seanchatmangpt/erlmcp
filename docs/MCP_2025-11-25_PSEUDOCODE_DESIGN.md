# MCP 2025-11-25 Features - Pseudocode Design

**Project**: erlmcp (Erlang/OTP MCP SDK)
**Phase**: SPARC Phase 2 (Pseudocode)
**Date**: 2026-01-29
**Status**: Design Phase

---

## Overview

This document provides algorithmic designs for implementing missing MCP 2025-11-25 features. All designs follow OTP patterns (gen_server, supervisor) and Chicago School TDD principles.

---

## Feature 1: Task Management (erlmcp_task_manager)

### Data Structures

```erlang
%% Task record
-record(task, {
    id :: binary(),                          % Unique task ID (UUID)
    status :: pending | working | completed | failed | cancelled,
    action :: binary() | undefined,          % Action description
    result :: term() | undefined,            % Result if completed
    error :: term() | undefined,             % Error if failed
    created_at :: integer(),                 % Creation timestamp (ms)
    updated_at :: integer(),                 % Last update timestamp (ms)
    completed_at :: integer() | undefined,   % Completion timestamp (ms)
    cancelled_at :: integer() | undefined,   % Cancellation timestamp (ms)
    cancellation_token :: reference() | undefined,  % For cancellation
    progress_token :: reference() | undefined,      % For progress updates
    metadata :: map()                        % Additional metadata
}).

%% State record
-record(state, {
    tasks :: #{binary() => #task{}},         % Task storage (in-memory or Mnesia)
    max_concurrent :: pos_integer(),         % Max concurrent tasks
    task_counter :: non_neg_integer(),       % Task ID counter
    next_task_id :: binary()                 % Pre-generated next ID
}).
```

### Algorithms

#### Task Creation Algorithm

```
FUNCTION create_task(Action, Metadata):
    1. CHECK concurrent task limit
       IF length(state.tasks) >= state.max_concurrent THEN
          RETURN error(max_concurrent_tasks_exceeded)
       END IF

    2. GENERATE unique task ID
       TaskId = generate_task_id()
       IF task exists in state.tasks THEN
          RETRY generation (max 3 attempts)
       END IF

    3. CREATE cancellation token
       CancelToken = erlmcp_cancellation:register(ClientPid, self(), <<"task">>)

    4. CREATE task record
       Task = #task{
           id = TaskId,
           status = pending,
           action = Action,
           created_at = current_timestamp(),
           updated_at = current_timestamp(),
           cancellation_token = CancelToken,
           metadata = Metadata
       }

    5. STORE task
       state.tasks[TaskId] = Task

    6. START task execution (async)
       spawn_link(fun() -> execute_task(TaskId, Action) end)

    7. RETURN {ok, TaskId}
END FUNCTION
```

#### Task Execution Algorithm

```
FUNCTION execute_task(TaskId, Action):
    1. GET task from storage
       Task = state.tasks[TaskId]

    2. UPDATE status to working
       Task1 = Task#task{status = working, updated_at = current_timestamp()}
       state.tasks[TaskId] = Task1
       notify_status_change(TaskId, working)

    3. EXECUTE action with progress tracking
       TRY
           Result = execute_action(Action)
           Task2 = Task1#task{
               status = completed,
               result = Result,
               updated_at = current_timestamp(),
               completed_at = current_timestamp()
           }
           state.tasks[TaskId] = Task2
           notify_completion(TaskId, Result)
       CATCH
           Error:Reason:Stacktrace ->
               Task3 = Task1#task{
                   status = failed,
                   error = {Error, Reason},
                   updated_at = current_timestamp()
               }
               state.tasks[TaskId] = Task3
               notify_failure(TaskId, Error)
       END TRY
END FUNCTION
```

#### Task Cancellation Algorithm

```
FUNCTION cancel_task(TaskId, Reason):
    1. GET task from storage
       IF TaskId not in state.tasks THEN
          RETURN error(task_not_found)
       END IF

    2. CHECK task status
       Task = state.tasks[TaskId]
       IF Task.status in [completed, failed, cancelled] THEN
          RETURN error(task_already_completed)
       END IF

    3. CANCEL via cancellation manager
       ok = erlmcp_cancellation:cancel(Task.cancellation_token, Reason)

    4. UPDATE task status
       Task1 = Task#task{
           status = cancelled,
           cancelled_at = current_timestamp(),
           updated_at = current_timestamp()
       }
       state.tasks[TaskId] = Task1

    5. NOTIFY cancellation
       notify_cancellation(TaskId, Reason)

    6. RETURN {ok, cancelled}
END FUNCTION
```

#### Task Listing Algorithm (with Pagination)

```
FUNCTION list_tasks(Cursor, Limit):
    1. VALIDATE limit
       IF Limit > 100 THEN
          Limit = 100  % Max page size
       END IF

    2. FILTER and SORT tasks
       AllTasks = maps:values(state.tasks)
       SortedTasks = sort_by_created_at_desc(AllTasks)

    3. APPLY cursor if present
       IF Cursor =/= undefined THEN
          FilteredTasks = filter_after_cursor(SortedTasks, Cursor)
       ELSE
          FilteredTasks = SortedTasks
       END IF

    4. TAKE page
       Page = take(FilteredTasks, Limit)

    5. GENERATE next cursor
       IF length(Page) = Limit THEN
          NextCursor = encode_cursor(last(Page))
       ELSE
          NextCursor = undefined
       END IF

    6. FORMAT response
       TaskSummaries = [format_task_summary(T) || T <- Page]
       RETURN {ok, #{tasks => TaskSummaries, nextCursor => NextCursor}}
END FUNCTION
```

### Edge Cases

1. **Concurrent task limit**: Enforce at creation, return error
2. **Task ID collision**: Retry generation with backoff
3. **Cancel non-existent task**: Return not found error
4. **Cancel completed task**: Return already completed error
5. **Progress token expiry**: Clean up tokens after completion

---

## Feature 2: Completions API (erlmcp_completion)

### Data Structures

```erlang
%% Completion request
-record(completion_request, {
    ref :: #{
        type => resource | command,
        uri => binary() | undefined,
        name => binary() | undefined
    },
    argument :: #{
        name => binary(),
        value => binary()
    }
}).

%% Completion result
-record(completion_result, {
    values :: [binary()],                    % Suggested completions
    total :: integer(),                      % Total available
    has_more :: boolean()                    % More completions available
}).

%% State record
-record(state, {
    cache :: ets:tid(),                      % Completion cache
    cache_ttl :: integer(),                  % Cache TTL (ms)
    ranking_weights :: #{
        frequency => float(),                % Frequency weight
        recency => float(),                  % Recency weight
        context => float()                   % Context weight
    }
}).
```

### Algorithms

#### Completion Generation Algorithm

```
FUNCTION generate_completion(Ref, Argument):
    1. EXTRACT reference type and target
       RefType = Ref.type
       Target = case RefType of
           resource -> Ref.uri
           command -> Ref.name
       end

    2. CHECK cache
       CacheKey = {RefType, Target, Argument.name, Argument.value}
       CASE ets:lookup(state.cache, CacheKey) OF
           [{CacheKey, Result, Timestamp}] ->
               IF current_timestamp() - Timestamp < state.cache_ttl THEN
                   RETURN {ok, Result}
               END IF
           [] ->
               ok  % Cache miss
       END CASE

    3. GENERATE candidates based on reference type
       Candidates = case RefType of
           resource ->
               generate_resource_completions(Target, Argument)
           command ->
               generate_command_completions(Target, Argument)
       end

    4. RANK candidates
       RankedCandidates = rank_completions(Candidates, Argument)

    5. SELECT top N (default: 10)
       TopCandidates = take(RankedCandidates, 10)

    6. FORMAT result
       Result = #completion_result{
           values = [C.value || C <- TopCandidates],
           total = length(Candidates),
           has_more = length(Candidates) > 10
       }

    7. CACHE result
       ets:insert(state.cache, {CacheKey, Result, current_timestamp()})

    8. RETURN {ok, Result}
END FUNCTION
```

#### Resource Completion Algorithm

```
FUNCTION generate_resource_completions(Uri, Argument):
    1. PARSE URI structure
       case Uri of
           <<"file://", Path/binary>> ->
               generate_file_completions(Path, Argument)
           <<"db://", Table/binary>> ->
               generate_db_completions(Table, Argument)
           <<"http://", _/binary>> ->
               generate_http_completions(Uri, Argument)
           _ ->
               []  % Unknown URI scheme
       end
END FUNCTION

FUNCTION generate_file_completions(Path, Argument):
    1. RESOLVE partial path
       BaseDir = filename:dirname(Path)
       Prefix = filename:basename(Path)

    2. LIST directory entries
       case file:list_dir(BaseDir) OF
           {ok, Entries} ->
               Filtered = filter_by_prefix(Entries, Prefix)
               Expanded = [filename:join(BaseDir, E) || E <- Filtered]
           {error, _} ->
               []
       end
END FUNCTION
```

#### Ranking Algorithm

```
FUNCTION rank_completions(Candidates, Argument):
    1. EXTRACT features for each candidate
       ScoredCandidates = [begin
           Frequency = get_frequency(Candidate),
           Recency = get_recency(Candidate),
           ContextMatch = calculate_context_match(Candidate, Argument),

           Score = (
               state.ranking_weights.frequency * Frequency +
               state.ranking_weights.recency * Recency +
               state.ranking_weights.context * ContextMatch
           ),
           {Candidate, Score}
       end || Candidate <- Candidates]

    2. SORT by score (descending)
       Sorted = lists:sort(fun({_, S1}, {_, S2}) -> S1 > S2 end, ScoredCandidates)

    3. EXTRACT sorted candidates
       [C || {C, _} <- Sorted]
END FUNCTION

FUNCTION calculate_context_match(Candidate, Argument):
    1. EXTRACT context from Argument.value
       Prefix = Argument.value

    2. CALCULATE string similarity
       JaroScore = jaro_winkler_similarity(Candidate.value, Prefix)

    3. RETURN normalized score (0.0-1.0)
       JaroScore
END FUNCTION
```

### Edge Cases

1. **Invalid URI scheme**: Return empty completions
2. **File permission errors**: Handle gracefully, return error
3. **Cache stampede**: Use ETS read_concurrency optimization
4. **Tie-breaking**: Use lexicographic order for equal scores

---

## Feature 3: Elicitation API (erlmcp_elicitation)

### Data Structures

```erlang
%% Elicitation request
-record(elicitation_request, {
    type :: url,                             % Only URL type supported
    name :: binary(),                        % Elicitation identifier
    description :: binary()                  % User-facing description
}).

%% Elicitation state
-record(elicitation, {
    id :: binary(),                          % Unique elicitation ID
    type :: url,
    name :: binary(),
    description :: binary(),
    url :: binary() | undefined,             % Generated URL
    value :: binary() | undefined,           % User-provided value
    status :: pending | completed | expired,
    created_at :: integer(),                 % Creation timestamp (ms)
    expires_at :: integer(),                 % Expiry timestamp (ms)
    completed_at :: integer() | undefined,   % Completion timestamp (ms)
    client_pid :: pid()                      % Client process
}).

%% State record
-record(state, {
    elicitations :: #{binary() => #elicitation{}},
    expiry_ttl :: integer()                  % Default TTL (ms)
}).
```

### Algorithms

#### Elicitation Creation Algorithm

```
FUNCTION create_elicitation(Requests, ClientPid):
    1. VALIDATE requests
       IF Requests =:= [] THEN
          RETURN error(empty_requests)
       END IF

    2. CREATE elicitation records
       CreatedElicitations = []
       FOR EACH Request IN Requests DO
           ElicitationId = generate_elicitation_id()

           %% Validate URL type
           IF Request.type =:= url THEN
               URL = generate_elicitation_url(ElicitationId)
           ELSE
               RETURN error(unsupported_elicitation_type)
           END IF

           Elicitation = #elicitation{
               id = ElicitationId,
               type = Request.type,
               name = Request.name,
               description = Request.description,
               url = URL,
               status = pending,
               created_at = current_timestamp(),
               expires_at = current_timestamp() + state.expiry_ttl,
               client_pid = ClientPid
           }

           state.elicitations[ElicitationId] = Elicitation
           CreatedElicitations = [Elicitation | CreatedElicitations]
       END FOR

    3. START expiry timers
       FOR EACH Elicitation IN CreatedElicitations DO
           erlang:send_after(state.expiry_ttl, self(), {expire, Elicitation.id})
       END FOR

    4. FORMAT response
       Response = [format_elicitation(E) || E <- lists:reverse(CreatedElicitations)]
       RETURN {ok, Response}
END FUNCTION
```

#### Elicitation Completion Algorithm

```
FUNCTION complete_elicitation(ElicitationId, Value):
    1. GET elicitation from storage
       IF ElicitationId not in state.elicitations THEN
          RETURN error(elicitation_not_found)
       END IF

    2. CHECK status
       Elicitation = state.elicitations[ElicitationId]
       IF Elicitation.status =/= pending THEN
          RETURN error(elicitation_already_completed)
       END IF

    3. VALIDATE value based on type
       case Elicitation.type of
           url ->
               IF is_valid_url(Value) THEN
                   ok
               ELSE
                   RETURN error(invalid_url)
               END IF
       end

    4. UPDATE elicitation
       Elicitation1 = Elicitation#elicitation{
           value = Value,
           status = completed,
           completed_at = current_timestamp()
       }
       state.elicitations[ElicitationId] = Elicitation1

    5. NOTIFY client
       ClientPid = Elicitation.client_pid,
       ClientPid ! {elicitation_complete, ElicitationId, Value}

    6. RETURN {ok, completed}
END FUNCTION
```

#### URL Generation Algorithm

```
FUNCTION generate_elicitation_url(ElicitationId):
    1. ENCODE elicitation ID
       EncodedId = base64:encode(ElicitationId)

    2. CONSTRUCT URL
       BaseUrl = application:get_env(erlmcp, elicitation_base_url, <<"https://erlmcp.local/elicit">>),
       URL = <<BaseUrl/binary, "/", EncodedId/binary>>

    3. ADD signature for verification
       Signature = sign_url(URL),
       SignedURL = <<URL/binary, "?sig=", Signature/binary>>

    4. RETURN SignedURL
END FUNCTION

FUNCTION is_valid_url(Url):
    1. PARSE URL
       case uri_string:parse(Url) OF
           {ok, Parsed} ->
               Scheme = uri_string:scheme(Parsed),
               IF Scheme =:= <<"https">> orelse Scheme =:= <<"http">> THEN
                   true
               ELSE
                   false
               END IF
           {error, _} ->
               false
       end
END FUNCTION
```

#### Expiry Handling Algorithm

```
FUNCTION handle_elicitation_expiry(ElicitationId):
    1. GET elicitation from storage
       IF ElicitationId not in state.elicitations THEN
          RETURN  % Already expired and cleaned up
       END IF

    2. CHECK status
       Elicitation = state.elicitations[ElicitationId]
       IF Elicitation.status =/= pending THEN
          RETURN  % Already completed
       END IF

    3. UPDATE status to expired
       Elicitation1 = Elicitation#elicitation{status = expired}
       state.elicitations[ElicitationId] = Elicitation1

    4. NOTIFY client
       ClientPid = Elicitation.client_pid,
       ClientPid ! {elicitation_expired, ElicitationId}

    5. CLEAN UP (delayed)
       erlang:send_after(60000, self(), {cleanup, ElicitationId})
END FUNCTION
```

### Edge Cases

1. **Expired elicitation completion**: Return error, don't allow completion
2. **Invalid URL in completion**: Validate scheme, reject non-HTTPS
3. **Concurrent completion attempts**: Idempotent, first completion wins
4. **Client process death**: Clean up orphaned elicitations

---

## Cross-Feature Integration

### Task → Cancellation Integration

```
%% Task manager registers cancellation token
CancelToken = erlmcp_cancellation:register(ClientPid, TaskPid, <<"task">>)

%% Task execution checks for cancellation
FUNCTION execute_action_with_cancellation(Action, CancelToken):
    execute_in_parallel(
        fun() -> perform_action(Action) end,
        fun() ->
            case erlmcp_cancellation:check(CancelToken) OF
                ok -> continue
                {error, cancelled} -> throw(cancelled)
            end
        end
    )
END FUNCTION
```

### Task → Progress Integration

```
%% Task manager creates progress token
ProgressToken = erlmcp_progress:generate_token()

%% Task execution reports progress
FUNCTION execute_action_with_progress(Action, ProgressToken):
    erlmcp_progress:create(ClientPid, <<"Starting task...">>),

    Result = perform_action(Action,
        fun(Percent, Message) ->
            erlmcp_progress:update(ProgressToken, #{
                progress => Percent,
                message => Message
            })
        end
    ),

    erlmcp_progress:complete(ProgressToken),
    Result
END FUNCTION
```

### Sampling → Elicitation Integration

```
%% Sampling can request URL elicitation
FUNCTION create_sampling_message_with_elicitation(Messages, Params):
    case maps:get(<<"includeContext">>, Params, false) of
        true ->
            %% Request resource URL from user
            {ok, [Elicitation]} = erlmcp_elicitation:create([
                #{type => url, name => <<"resource_url">>, description => <<"Select resource">>}
            ], self()),

            %% Wait for completion
            receive
                {elicitation_complete, <<"resource_url">>, Url} ->
                    %% Add resource context to messages
                    Messages1 = add_resource_context(Messages, Url),
                    create_message(Messages1, Params)
            AFTER 300000 ->
                {error, elicitation_timeout}
            end;
        false ->
            create_message(Messages, Params)
    end
END FUNCTION
```

---

## Performance Considerations

### Task Management Optimization

1. **Concurrent Task Limit**: Use ETS counter for atomic increment/check
2. **Task Storage**: Use Mnesia for persistence, ETS for cache
3. **Pagination**: Pre-sort tasks by creation time for O(1) cursor navigation

### Completion Optimization

1. **Caching**: Use ETS with read_concurrency for parallel reads
2. **Ranking**: Pre-compute frequency scores, update periodically
3. **Fuzzy Matching**: Use Trie data structure for prefix search

### Elicitation Optimization

1. **URL Generation**: Pre-generate URL pool for batch requests
2. **Expiry**: Use timer wheel for efficient timer management
3. **Validation**: Cache URL parse results

---

## Error Recovery Strategies

### Task Management

```
%% Task execution failure recovery
FUNCTION handle_task_failure(TaskId, Error):
    1. UPDATE task status to failed
    2. STORE error details
    3. NOTIFY client
    4. SCHEDULE retry if retryable
    5. CLEAN up resources
END FUNCTION

%% Retry strategy
FUNCTION should_retry_task(Task, Error):
    RetryCount = maps:get(<<"retryCount">>, Task.metadata, 0),
    MaxRetries = application:get_env(erlmcp, max_task_retries, 3),

    case Error of
        timeout -> RetryCount < MaxRetries
        {error, transient} -> RetryCount < MaxRetries
        _ -> false
    end
END FUNCTION
```

### Completion Recovery

```
%% Cache failure fallback
FUNCTION get_completion_with_fallback(Ref, Argument):
    TRY
        generate_completion(Ref, Argument)
    CATCH
        error:Reason ->
            logger:warning("Completion failed, returning empty: ~p", [Reason]),
            {ok, #completion_result{values = [], total = 0, has_more = false}}
    END TRY
END FUNCTION
```

### Elicitation Recovery

```
%% Timeout handling
FUNCTION handle_elicitation_timeout(ElicitationId):
    1. MARK as expired
    2. NOTIFY client
    3. CLEAN up resources
    4. LOG timeout event
END FUNCTION
```

---

## Testing Strategy (Chicago School TDD)

### Task Management Tests

```erlang
%% Test: Task creation
task_creation_test() ->
    %% Setup
    {ok, Pid} = erlmcp_task_manager:start_link(),

    %% Exercise
    {ok, TaskId} = erlmcp_task_manager:create_task(<<"test_action">>, #{}),

    %% Verify
    {ok, Task} = erlmcp_task_manager:get_task(TaskId),
    ?assertEqual(<<"test_action">>, Task#task.action),
    ?assertEqual(pending, Task#task.status),

    %% Teardown
    gen_server:stop(Pid).

%% Test: Task cancellation
task_cancellation_test() ->
    %% Setup
    {ok, Pid} = erlmcp_task_manager:start_link(),
    {ok, TaskId} = erlmcp_task_manager:create_task(<<"long_action">>, #{}),

    %% Exercise
    {ok, cancelled} = erlmcp_task_manager:cancel_task(TaskId, <<"test_reason">>),

    %% Verify
    {ok, Task} = erlmcp_task_manager:get_task(TaskId),
    ?assertEqual(cancelled, Task#task.status),
    ?assertMatch(#{{cancelled_at, _}}, Task),

    %% Teardown
    gen_server:stop(Pid).
```

### Completion Tests

```erlang
%% Test: File path completion
file_completion_test() ->
    %% Setup
    {ok, Pid} = erlmcp_completion:start_link(),
    Ref = #{type => resource, uri => <<"file:///tmp">>},
    Argument = #{name => <<"path">>, value => <<"test">>},

    %% Exercise
    {ok, Result} = erlmcp_completion:complete(Ref, Argument),

    %% Verify
    ?assert(is_list(Result#completion_result.values)),
    ?assertMatch(#completion_result{has_more = _}, Result),

    %% Teardown
    gen_server:stop(Pid).
```

### Elicitation Tests

```erlang
%% Test: URL elicitation creation
url_elicitation_creation_test() ->
    %% Setup
    {ok, Pid} = erlmcp_elicitation:start_link(),
    Requests = [#{
        type => url,
        name => <<"test_url">>,
        description => <<"Test URL">>
    }],

    %% Exercise
    {ok, [Elicitation]} = erlmcp_elicitation:create(Requests, self()),

    %% Verify
    ?assertMatch(<<"https://", _/binary>>, Elicitation.url),
    ?assertEqual(pending, Elicitation.status),

    %% Teardown
    gen_server:stop(Pid).
```

---

## Next Steps

1. **Review** pseudocode designs with team
2. **Validate** algorithms with stakeholders
3. **Proceed** to Phase 3 (Architecture)
4. **Identify** potential performance bottlenecks
5. **Refine** error handling strategies
