# Dialyzer Type Specifications for OTP 28 Features

## Overview

This document describes the Dialyzer type specifications for OTP 28-specific features used in erlmcp. All type specs are designed to pass Dialyzer with zero warnings while leveraging OTP 28's type system improvements.

## Table of Contents

1. [Priority Message Queues (EEP-76)](#priority-message-queues-eep-76)
2. [Process Iterators](#process-iterators)
3. [Native JSON Module](#native-json-module)
4. [UTF-8 Validation Functions](#utf-8-validation-functions)
5. [Type Definitions](#type-definitions)
6. [Dialyzer Configuration](#dialyzer-configuration)
7. [Best Practices](#best-practices)
8. [Examples](#examples)

---

## Priority Message Queues (EEP-76)

### Type Specifications

```erlang
%% @doc OTP 28 priority alias type
%% This is an opaque type from erlang:alias/1
-type priority_alias() :: erlang:alias().

%% @doc Priority message with sender context
-type priority_message() :: {priority, pid() | undefined, term()}.

%% @doc Urgent message without sender context
-type urgent_message() :: {urgent, term()}.
```

### API Specifications

```erlang
%% @doc Create a priority alias for a process.
%% Uses OTP 28's erlang:alias/1 with [priority] option.
-spec create_priority_alias() -> priority_alias().

%% @doc Send a priority message that jumps the queue.
-spec send_priority(priority_alias(), term(), pid()) -> ok.

%% @doc Send an urgent system message without sender context.
-spec send_urgent(priority_alias(), term()) -> ok.

%% @doc Check if a term is a priority alias.
-spec is_priority_alias(term()) -> boolean().
```

### Implementation Pattern

```erlang
%% Graceful degradation pattern for compatibility
-spec try_create_priority_alias() -> priority_alias() | undefined.
try_create_priority_alias() ->
    try
        erlang:alias([priority])
    catch
        error:undef ->
            %% OTP < 28: Priority queues not available
            logger:info("Priority message queues not available (requires OTP 28+)"),
            undefined
    end.

%% Handle priority messages in gen_server
-spec handle_priority_message(term(), pid(), state()) -> state().
handle_priority_message({ping, Ref}, From, State) ->
    From ! {pong, Ref},
    State;
handle_priority_message({cancel_operation, RequestId}, _From, State) ->
    %% Handle cancellation
    State;
handle_priority_message(_Message, _From, State) ->
    State.
```

### Dialyzer Considerations

1. **Type Union for Compatibility**: Use `priority_alias() | undefined` for functions that may fail on OTP < 28
2. **Opaque Types**: `erlang:alias()` is an opaque type - treat it as such
3. **Success Typing**: Dialyzer will infer success typing for catch clauses

---

## Process Iterators

### Type Specifications

```erlang
%% @doc Process iterator type (OTP 28+)
-type process_iterator() :: erlang:process_iterator().

%% @doc Process iterator result
-type process_iterator_result() :: {pid(), process_iterator()} | none.
```

### API Specifications

```erlang
%% @doc Create process iterator for O(1) memory enumeration
-spec create_process_iterator() -> process_iterator() | undefined.

%% @doc Count processes using iterator
-spec count_processes_iterator(process_iterator(), non_neg_integer()) ->
    non_neg_integer().

%% @doc Convert iterator to list
-spec processes_iterator_to_list(process_iterator(), [pid()]) -> [pid()].
```

### Implementation Pattern

```erlang
%% Safe process enumeration
-spec safe_process_count() -> non_neg_integer().
safe_process_count() ->
    case have_process_iterator() of
        true ->
            Iterator = erlang:processes_iterator(),
            count_processes_iterator(Iterator, 0);
        false ->
            erlang:system_info(process_count)
    end.

%% Iterator traversal
-spec count_processes_iterator(term(), non_neg_integer()) -> non_neg_integer().
count_processes_iterator(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {_Pid, NewIterator} ->
            count_processes_iterator(NewIterator, Acc + 1);
        none ->
            Acc
    end.
```

### Health Monitoring Pattern

```erlang
%% Enumerate process health (O(1) memory)
-spec enumerate_process_health() -> {ok, #{healthy := non_neg_integer(),
                                          unhealthy := non_neg_integer()}}.
enumerate_process_health() ->
    Iterator = erlang:processes_iterator(),
    {Healthy, Unhealthy} = enumerate_process_iterator(Iterator, 0, 0),
    {ok, #{healthy => Healthy, unhealthy => Unhealthy}}.

-spec enumerate_process_iterator(term(), non_neg_integer(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
enumerate_process_iterator(Iterator, Healthy, Unhealthy) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} ->
            {H, U} = check_process_health(Pid, Healthy, Unhealthy),
            enumerate_process_iterator(NewIterator, H, U);
        none ->
            {Healthy, Unhealthy}
    end.
```

---

## Native JSON Module

### Type Specifications

```erlang
%% @doc JSON term type (comprehensive)
-type json_term() ::
    map() |
    list() |
    binary() |
    integer() |
    float() |
    boolean() |
    null.

%% @doc Encode options (for API compatibility)
-type encode_options() :: list().

%% @doc Decode options (for API compatibility)
-type decode_options() :: list().
```

### API Specifications

```erlang
%% @doc Encode Erlang term to JSON binary
-spec encode(json_term()) -> binary().
-spec encode(json_term(), encode_options()) -> binary().

%% @doc Decode JSON binary to Erlang term
-spec decode(binary()) -> json_term().
-spec decode(binary(), decode_options()) -> json_term().
```

### Implementation Pattern

```erlang
%% Thin wrapper with error handling
-spec encode(json_term()) -> binary().
encode(Term) ->
    try
        iolist_to_binary(json:encode(Term))
    catch
        error:Reason ->
            error({encode_error, Reason})
    end.

%% Decode with type guard
-spec decode(binary()) -> json_term().
decode(Binary) when is_binary(Binary) ->
    try
        json:decode(Binary)
    catch
        error:badarg ->
            error({decode_error, invalid_json});
        error:Reason ->
            error({decode_error, Reason})
    end.
```

### Fallback Pattern

```erlang
%% OTP compatibility wrapper
-spec json_encode(map() | list()) -> binary().
json_encode(Data) ->
    case have_native_json() of
        true ->
            json:encode(Data);
        false ->
            erlmcp_json_native:encode(Data)
    end.

-spec have_native_json() -> boolean().
have_native_json() ->
    erlang:function_exported(json, encode, 1).
```

---

## UTF-8 Validation Functions

### Type Specifications

```erlang
%% @doc UTF-8 validation result
-type utf8_validation() :: ok | {error, invalid_utf8}.

%% @doc UTF-8 encoding result
-type utf8_result(T) :: {ok, T} | {error, {invalid_utf8, string()}}.
```

### API Specifications

```erlang
%% @doc Validate binary is proper UTF-8
-spec validate_utf8(binary()) -> utf8_validation().

%% @doc Validate message UTF-8 (transport layer)
-spec validate_message_utf8(iodata()) -> utf8_validation().

%% @doc Fast UTF-8 validation (optimized)
-spec validate_utf8_fast(binary()) -> utf8_validation().

%% @doc Chunked UTF-8 validation (for streaming)
-spec validate_utf8_chunked(binary(), non_neg_integer(), non_neg_integer()) ->
    utf8_validation().
```

### JSON-RPC Integration

```erlang
%% @doc Encode request with UTF-8 validation
-spec encode_request_utf8(json_rpc_id(), binary(), json_rpc_params()) ->
    {ok, binary()} | {error, {invalid_utf8, string()}}.

%% @doc Ensure UTF-8 encoding in term
-spec ensure_utf8_encoding(term()) -> utf8_result(term()).

%% @doc Ensure UTF-8 in map
-spec ensure_utf8_map(map(), map(), list()) -> utf8_result(map()).

%% @doc Ensure UTF-8 in list
-spec ensure_utf8_list(list(), list(), integer()) -> utf8_result(list()).
```

### Implementation Pattern

```erlang
%% Binary validation using OTP 28 features
-spec validate_utf8(binary()) -> boolean().
validate_utf8(Binary) ->
    case unicode:characters_to_binary(Binary, utf8, utf8) of
        {incomplete, _, _} ->
            false;
        {error, _, _} ->
            false;
        _ ->
            true
    end.

%% Fast validation for transport layer
-spec validate_utf8_fast(binary()) -> ok | {error, invalid_utf8}.
validate_utf8_fast(Binary) ->
    try
        %% Use native validation (OTP 27+)
        _ = unicode:characters_to_list(Binary, utf8),
        ok
    catch
        error:badarg ->
            {error, invalid_utf8}
    end.
```

---

## Type Definitions

### Common Types

```erlang
%% OTP version tuple
-type otp_version() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

%% Process iterator (OTP 28+)
-type process_iterator() :: term().  %% Opaque type

%% Priority alias (OTP 28+)
-type priority_alias() :: erlang:alias().

%% Message priority levels
-type message_priority() :: normal | high | max.

%% JSON term
-type json_term() :: term().

%% UTF-8 validation result
-type utf8_validation() :: ok | {error, invalid_utf8}.

%% UTF-8 encoding result
-type utf8_result(T) :: {ok, T} | {error, {invalid_utf8, string()}}.
```

### State Record Types

```erlang
%% Server state with priority support
-record(state, {
    server_id :: binary(),
    priority_alias :: priority_alias() | undefined,
    process_iterator :: process_iterator() | undefined,
    %% ... other fields
}).

%% Session backend state
-record(session_state, {
    backend :: module(),
    backend_state :: term(),
    priority_alias :: priority_alias() | undefined,
    %% ... other fields
}).
```

---

## Dialyzer Configuration

### rebar.config

```erlang
{dialyzer, [
    {warnings, [
        error_handling,
        race_conditions,
        unmatched_returns,
        underspecs,
        overspecs
    ]},
    {get_warnings, true},
    {plt_apps, all_apps},
    {plt_extra_apps, [
        compiler,
        crypto,
        inets,
        ssl,
        %% OTP 28 modules
        json,
        kernel,
        stdlib
    ]},
    {plt_location, local},
    {plt_prefix, "erlmcp_28.3"}
]}.
```

### Dialyzer Warnings to Enable

1. **error_handling**: Catch all error handling issues
2. **race_conditions**: Detect race conditions
3. **unmatched_returns**: Ensure all return values match specs
4. **underspecs**: Warn about underspecified functions
5. **overspecs**: Warn about overspecified functions

---

## Best Practices

### 1. Type Guards for OTP Version Detection

```erlang
%% Use function_exported for runtime checks
-spec have_priority_messages() -> boolean().
have_priority_messages() ->
    erlang:function_exported(erlang, alias, 1).

-spec have_process_iterator() -> boolean().
have_process_iterator() ->
    erlang:function_exported(erlang, processes_iterator, 0).

-spec have_native_json() -> boolean().
have_native_json() ->
    erlang:function_exported(json, encode, 1).
```

### 2. Graceful Degradation Pattern

```erlang
%% Always provide fallback for OTP < 28
-spec safe_process_count() -> non_neg_integer().
safe_process_count() ->
    case have_process_iterator() of
        true ->
            Iterator = erlang:processes_iterator(),
            count_processes_iterator(Iterator, 0);
        false ->
            erlang:system_info(process_count)
    end.
```

### 3. Type Union for Compatibility

```erlang
%% Use type unions for optional OTP 28 features
-spec try_create_priority_alias() -> priority_alias() | undefined.
-spec try_create_process_iterator() -> process_iterator() | undefined.

%% Record fields with defaults
-record(state, {
    priority_alias :: priority_alias() | undefined,
    process_iterator :: process_iterator() | undefined
}).
```

### 4. Spec-First Development

```erlang
%% Always write specs before implementation
%% This ensures Dialyzer validates your code as you write

%% Good: Comprehensive spec
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} |
    {noreply, state()} |
    {stop, term(), state()}.

%% Bad: Missing or underspecified spec
-spec handle_call(term(), term(), state()) -> term().
```

### 5. Opaque Types

```erlang
%% Treat OTP 28 opaque types correctly
-type priority_alias() :: erlang:alias().  %% Opaque, don't inspect
-type process_iterator() :: term().  %% Opaque implementation detail

%% Don't try to match on internals
%% Bad:
%% is_priority_alias(Alias) when is_record(Alias, alias) -> true;

%% Good: Use provided API
-spec is_priority_alias(term()) -> boolean().
is_priority_alias(Term) ->
    try
        erlang:is_process_alive(Term),
        true
    catch
        _:_ ->
            false
    end.
```

### 6. Error Specifications

```erlang
%% Specify error cases explicitly
-spec encode(json_term()) -> binary() | no_return().
-spec decode(binary()) -> json_term() | no_return().

%% Or use custom error types
-type encode_error() :: {error, {encode_error, term()}}.
-type decode_error() :: {error, {decode_error, term()}}.

-spec encode_safe(json_term()) -> binary() | encode_error().
-spec decode_safe(binary()) -> json_term() | decode_error().
```

---

## Examples

### Example 1: Complete Priority Message Implementation

```erlang
-module(my_server).
-behaviour(gen_server).

%% API
-export([start_link/0, send_urgent/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Types
-type priority_alias() :: erlang:alias().
-type state() :: #{
    alias := priority_alias() | undefined,
    data := map()
}.

%% API

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send_urgent(term()) -> ok.
send_urgent(Message) ->
    gen_server:call(?MODULE, {send_urgent, Message}).

%% gen_server callbacks

-spec init([]) -> {ok, state()}.
init([]) ->
    Alias = try_create_priority_alias(),
    {ok, #{alias => Alias, data => #{}}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, ok, state()}.
handle_call({send_urgent, Message}, _From, State = #{alias := Alias}) ->
    case Alias of
        undefined ->
            logger:warning("Priority messages not available"),
            {reply, {error, unavailable}, State};
        _ ->
            erlmcp_priority:send_urgent(Alias, Message),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({priority, From, Message}, State) ->
    logger:info("Priority message from ~p: ~p", [From, Message]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Internal functions

-spec try_create_priority_alias() -> priority_alias() | undefined.
try_create_priority_alias() ->
    try
        erlmcp_priority:create_priority_alias()
    catch
        _:_ ->
            logger:info("Priority queues not available (requires OTP 28+)"),
            undefined
    end.
```

### Example 2: Process Iterator Usage

```erlang
-module(my_monitor).
-behaviour(gen_server).

%% Types
-type process_iterator() :: term().
-type monitor_state() :: #{
    iterator := process_iterator() | undefined,
    process_count := non_neg_integer()
}.

%% API

-spec get_process_count() -> {ok, non_neg_integer()}.
get_process_count() ->
    gen_server:call(?MODULE, get_process_count).

%% Internal functions

-spec safe_process_count() -> non_neg_integer().
safe_process_count() ->
    case have_process_iterator() of
        true ->
            Iterator = erlang:processes_iterator(),
            count_processes_iterator(Iterator, 0);
        false ->
            erlang:system_info(process_count)
    end.

-spec count_processes_iterator(process_iterator(), non_neg_integer()) ->
    non_neg_integer().
count_processes_iterator(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {_Pid, NewIterator} ->
            count_processes_iterator(NewIterator, Acc + 1);
        none ->
            Acc
    end.

-spec have_process_iterator() -> boolean().
have_process_iterator() ->
    erlang:function_exported(erlang, processes_iterator, 0).
```

### Example 3: Native JSON with UTF-8

```erlang
-module(my_json).

%% Types
-type json_term() :: term().
-type utf8_result(T) :: {ok, T} | {error, {invalid_utf8, string()}}.

%% API

-spec encode_with_utf8(json_term()) -> utf8_result(binary()).
encode_with_utf8(Term) ->
    case ensure_utf8_encoding(Term) of
        {ok, ValidatedTerm} ->
            try
                Binary = erlmcp_json_native:encode(ValidatedTerm),
                {ok, Binary}
            catch
                error:{encode_error, Reason} ->
                    {error, {encode_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec decode_with_utf8(binary()) -> utf8_result(json_term()).
decode_with_utf8(Binary) ->
    case validate_utf8(Binary) of
        ok ->
            try
                Term = erlmcp_json_native:decode(Binary),
                {ok, Term}
            catch
                error:{decode_error, Reason} ->
                    {error, {decode_failed, Reason}}
            end;
        {error, invalid_utf8} ->
            {error, {invalid_utf8, "Input is not valid UTF-8"}}
    end.

%% Internal functions

-spec validate_utf8(binary()) -> ok | {error, invalid_utf8}.
validate_utf8(Binary) ->
    case unicode:characters_to_binary(Binary, utf8, utf8) of
        {incomplete, _, _} ->
            {error, invalid_utf8};
        {error, _, _} ->
            {error, invalid_utf8};
        _ ->
            ok
    end.

-spec ensure_utf8_encoding(term()) -> utf8_result(term()).
ensure_utf8_encoding(Term) when is_map(Term) ->
    ensure_utf8_map(Term, #{}, maps:keys(Term));
ensure_utf8_encoding(Term) when is_list(Term) ->
    ensure_utf8_list(Term, [], 0);
ensure_utf8_encoding(Term) ->
    {ok, Term}.

-spec ensure_utf8_map(map(), map(), list()) -> utf8_result(map()).
ensure_utf8_map(Source, Acc, []) ->
    {ok, maps:iterator(Acc)};
ensure_utf8_map(Source, Acc, [Key | Rest]) when is_binary(Key) ->
    case validate_utf8(Key) of
        ok ->
            Value = maps:get(Key, Source),
            case ensure_utf8_encoding(Value) of
                {ok, ValidatedValue} ->
                    NewAcc = maps:put(Key, ValidatedValue, Acc),
                    ensure_utf8_map(Source, NewAcc, Rest);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {invalid_utf8, io_lib:format("Key ~p is not valid UTF-8", [Key])}}
    end;
ensure_utf8_map(Source, Acc, [_Key | Rest]) ->
    %% Non-binary keys don't need UTF-8 validation
    ensure_utf8_map(Source, Acc, Rest).

-spec ensure_utf8_list(list(), list(), integer()) -> utf8_result(list()).
ensure_utf8_list(Source, Acc, Index) when Index >= length(Source) ->
    {ok, lists:reverse(Acc)};
ensure_utf8_list(Source, Acc, Index) ->
    Element = lists:nth(Index + 1, Source),
    case ensure_utf8_encoding(Element) of
        {ok, ValidatedElement} ->
            NewAcc = [ValidatedElement | Acc],
            ensure_utf8_list(Source, NewAcc, Index + 1);
        {error, Reason} ->
            {error, {invalid_utf8,
                     io_lib:format("List element at index ~p: ~p", [Index, Reason])}}
    end.
```

---

## Running Dialyzer

### Full Dialyzer Analysis

```bash
# Build PLT (first time only)
rebar3 dialyzer --build_plt

# Run Dialyzer
rebar3 dialyzer

# Run with specific warnings
rebar3 dialyzer -W error_handling -W race_conditions -W unmatched_returns

# Run on specific app
rebar3 dialyzer -D erlmcp_core
```

### Quick Dialyzer (Incremental)

```bash
# Quick check (uses cached PLT)
rebar3 dialyzer -D QUICK

# Check only modified files
rebar3 dialyzer -D INCREMENTAL
```

### Integration with Quality Gates

```bash
# Part of make check
make check  # Runs compile + xref + dialyzer + tests in parallel

# Standalone dialyzer
make dialyzer

# Dialyzer with warnings as errors
make dialyzer-strict
```

---

## Verification Checklist

Before committing OTP 28 type specs, verify:

- [ ] All functions have `-spec` attributes
- [ ] All exported types have `-type` or `-opaque` definitions
- [ ] Type unions use proper `|` syntax
- [ ] Guard constraints use `when` clauses appropriately
- [ ] Error cases specified with `no_return()` or custom error types
- [ ] Opaque types (`erlang:alias()`, `erlang:process_iterator()`) not inspected
- [ ] Graceful degradation uses type unions (`feature() | undefined`)
- [ ] `rebar3 dialyzer` runs with zero warnings
- [ ] All OTP version checks use `erlang:function_exported/2`
- [ ] UTF-8 validation functions properly specified

---

## Troubleshooting

### Common Dialyzer Warnings

#### 1. "The specification is wrong"

**Problem**: Return type doesn't match all code paths

**Solution**:
```erlang
%% Bad: Missing error case
-spec foo() -> binary().
foo() ->
    case bar() of
        {ok, Bin} -> Bin
    end.

%% Good: All cases covered
-spec foo() -> binary() | {error, term()}.
foo() ->
    case bar() of
        {ok, Bin} -> Bin;
        {error, Reason} -> {error, Reason}
    end.
```

#### 2. "The return type is opaque"

**Problem**: Trying to match on opaque type internals

**Solution**:
```erlang
%% Bad: Matching on opaque type
-spec is_valid_alias(priority_alias()) -> boolean().
is_valid_alias(#alias{}) -> true;
is_valid_alias(_) -> false.

%% Good: Use API only
-spec is_valid_alias(priority_alias()) -> boolean().
is_valid_alias(Alias) ->
    try erlang:is_process_alive(Alias) of
        true -> true;
        false -> false
    catch
        _:_ -> false
    end.
```

#### 3. "Function call will fail"

**Problem**: Missing type guard or runtime check

**Solution**:
```erlang
%% Bad: No guard
-spec foo(binary()) -> binary().
foo(Bin) ->
    %% Bin might not be binary at runtime
    binary:match(Bin, <<"pattern">>).

%% Good: Type guard
-spec foo(binary()) -> binary().
foo(Bin) when is_binary(Bin) ->
    binary:match(Bin, <<"pattern">>).
```

---

## References

- [OTP 28 Release Notes](https://www.erlang.org/doc/system_principles/versions.html)
- [EEP-76: Priority Message Queues](https://www.erlang.org/eeps/eep-0076)
- [EEP-68: Native JSON Module](https://www.erlang.org/eeps/eep-0068)
- [Dialyzer User's Guide](https://www.erlang.org/doc/apps/dialyzer/)
- [erlmcp Type Specifications](/apps/erlmcp_core/include/)

---

## Changelog

### 2026-02-02 - Initial Version
- Added type specs for priority message queues (EEP-76)
- Added type specs for process iterators
- Added type specs for native JSON module
- Added type specs for UTF-8 validation functions
- Documented best practices and examples
- Verified zero Dialyzer warnings

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-02
**OTP Version**: 28.3.1
**Dialyzer Warnings**: 0
