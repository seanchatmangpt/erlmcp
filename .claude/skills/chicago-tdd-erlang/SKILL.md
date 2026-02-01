# Chicago TDD for Erlang

Chicago School TDD applied to Erlang/OTP development.

## Principles

1. **Tests First**: Write failing test before implementation
2. **State-Based**: Test observable states, not implementation details
3. **Real Processes**: No mocks, use actual gen_server, processes
4. **AAA Pattern**: Arrange, Act, Assert

## EUnit Pattern

```erlang
%% Arrange
State = #{count => 0},
Pid = start_link(State),

%% Act
{ok, 1} = increment(Pid),

%% Assert
#{count := 1} = get_state(Pid).
```

## CT Pattern

```erlang
happy_path_test(_) ->
    %% Arrange
    Config = init_config(),

    %% Act
    {ok, _} = my_server:start_link(Config),

    %% Assert
    {ok, State} = my_server:get_state(),
    ?assertMatch(#{status := ready}, State).
```

## Proper Pattern

```erlang
prop_invariant() ->
    ?FORALL({X, Y}, {int(), int()},
        begin
            State = my_module:process(X, Y),
            invariant(State)
        end
    end).
```

## Quality Gates

- Tests must fail before implementation (RED)
- Tests must pass after implementation (GREEN)
- No mocking of erlmcp processes
- Real gen_server, supervisors in tests
- Coverage >= 80%

## References

- `apps/erlmcp_core/test/` for examples
- `CLAUDE.md` for Chicago TDD rules
