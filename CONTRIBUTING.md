# Contributing to erlmcp

**Estimated read time: 10 minutes**

We welcome contributions! This guide explains how to contribute code, documentation, and feedback to the erlmcp project.

## Code of Conduct

- Be respectful and inclusive
- Focus on the code, not the person
- Help others learn and grow
- Report issues privately to maintainers

## Getting Started

1. **Setup environment**: Follow [GETTING_STARTED.md](docs/GETTING_STARTED.md)
2. **Read architecture**: Review [ARCHITECTURE_OVERVIEW.md](docs/ARCHITECTURE_OVERVIEW.md)
3. **Explore examples**: Check [examples/README.md](examples/README.md)
4. **Join discussions**: GitHub Discussions for questions

## Essential Reading

Before starting work, familiarize yourself with:

- **Architecture**: [docs/ARCHITECTURE_OVERVIEW.md](docs/ARCHITECTURE_OVERVIEW.md) - System design, supervision trees, OTP patterns
- **Code Standards**: [CLAUDE.md](CLAUDE.md) - Mandatory development patterns, quality gates, TDD requirements
- **Testing Standards**: [archive/quality-reports/](archive/quality-reports/) - Quality gate results, test coverage reports
- **PR Template**: [.github/PULL_REQUEST_TEMPLATE.md](.github/PULL_REQUEST_TEMPLATE.md) - Required PR format and checklist

**Key Standards**:
- Test-Driven Development (TDD) - tests written FIRST
- Chicago School testing - NO mocks/fakes, real processes only
- Quality gates MUST pass: compile, test, coverage ≥80%, lint
- OTP patterns enforced: gen_server, supervision, let-it-crash

## Making Changes

### 1. Create an Issue

Before starting work:

```
Title: Brief description

Description:
- What problem does this solve?
- Why is it needed?
- Any design considerations?

Labels: [bug|enhancement|documentation]
```

### 2. Create a Branch

```bash
git checkout -b feature/my-feature
# or
git checkout -b fix/issue-123
# or
git checkout -b docs/improve-guide
```

### 3. Write Tests First (TDD)

```erlang
% test/my_feature_tests.erl
-module(my_feature_tests).
-include_lib("eunit/include/eunit.hrl").

my_function_returns_ok_test() ->
    Result = my_feature:my_function(input),
    ?assertEqual({ok, expected}, Result).
```

Run test:
```bash
rebar3 eunit --module=my_feature_tests
```

### 4. Implement Feature

```erlang
% src/my_feature.erl
-module(my_feature).
-export([my_function/1]).

-spec my_function(term()) -> {ok, term()} | {error, atom()}.
my_function(Input) ->
    case validate(Input) of
        ok -> {ok, process(Input)};
        Error -> Error
    end.
```

### 5. Full Validation

```bash
make workspace-check       # Build + lint + test
make workspace-test        # All tests
make workspace-lint        # Static analysis
make coverage-report       # Coverage metrics
```

All checks **must pass**. No exceptions.

### 6. Documentation

Update relevant files:
- Code comments for complex logic
- [docs/api-reference.md](docs/api-reference.md) for API changes
- [docs/ARCHITECTURE_OVERVIEW.md](docs/ARCHITECTURE_OVERVIEW.md) for design changes
- Docstrings for public functions

### 7. Commit

```bash
git add .
git commit -m "feat(scope): description

Longer explanation if needed.

Fixes #123"
```

**Commit message format**:
```
type(scope): description

type: feat|fix|docs|test|refactor|perf|ci
scope: module|transport|protocol|build|etc
description: what changed and why
```

### 8. Create Pull Request

Include:
- Clear description of changes
- Link to related issue (#123)
- Testing performed
- Any breaking changes
- Screenshots for UI changes (if applicable)

```markdown
## Description
Fixes #123: Brief description

## Changes
- Change 1
- Change 2

## Testing
- Tested with: make test
- Coverage: 85%
- Tested on: macOS/Linux/...

## Checklist
- [x] Tests pass
- [x] Lint passes
- [x] Documentation updated
- [x] No breaking changes
```

## Code Style Guidelines

### Naming Conventions

```erlang
% Modules: lowercase_with_underscores
-module(erlmcp_transport_tcp).

% Functions: lowercase_with_underscores
my_function(Arg1, Arg2)

% Variables: CamelCase
MyVariable, Pid, Result

% Constants: UPPERCASE_WITH_UNDERSCORES
-define(MAX_CONNECTIONS, 10000).
-define(DEFAULT_TIMEOUT, 30000).

% Records: CamelCase in definition, lowercase_with_underscores in usage
-record(server_state, {id, name, status}).
ServerState = #server_state{id = 1, name = <<"Server">>}
```

### Function Size

Keep functions small and focused:
- **Max 20 lines**: Preferred
- **Max 50 lines**: Acceptable
- **>50 lines**: Consider breaking up

```erlang
% BAD - too complex
process_request(Request) ->
    case validate(Request) of
        ok ->
            case extract_data(Request) of
                {ok, Data} ->
                    case apply_rules(Data) of
                        {ok, Result} ->
                            case format_response(Result) of
                                {ok, Response} -> {ok, Response};
                                Error -> Error
                            end;
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> Error
    end.

% GOOD - separated concerns
process_request(Request) ->
    case validate(Request) of
        ok -> apply_processing(Request);
        Error -> Error
    end.

apply_processing(Request) ->
    case extract_data(Request) of
        {ok, Data} -> apply_rules_and_format(Data);
        Error -> Error
    end.
```

### Type Hints

Always include type hints:

```erlang
% BAD - no types
my_function(X, Y) ->
    X + Y.

% GOOD - full types
-spec my_function(integer(), integer()) -> integer().
my_function(X, Y) ->
    X + Y.

% GOOD - with options
-spec my_function(X, Y) -> {ok, integer()} | {error, atom()} when
    X :: integer(),
    Y :: integer().
my_function(X, Y) ->
    case validate(X, Y) of
        ok -> {ok, X + Y};
        Error -> Error
    end.
```

### Error Handling

Always return Result types, never throw exceptions:

```erlang
% BAD - throws exception
risky_operation(X) ->
    Pid = whereis(some_server),
    gen_server:call(Pid, {process, X}).

% GOOD - returns Result
risky_operation(X) ->
    case whereis(some_server) of
        undefined ->
            {error, server_not_running};
        Pid ->
            try
                {ok, gen_server:call(Pid, {process, X})}
            catch
                Type:Reason ->
                    {error, {Type, Reason}}
            end
    end.
```

### Comments

Comment **why**, not **what**:

```erlang
% BAD - comments describe code
X = Y + 1,  % Add 1 to Y

% GOOD - comments explain intent
% Increment version for next batch
Version = CurrentVersion + 1,
```

## Testing Requirements

### Coverage Standards

- **Overall**: 80%+ coverage required
- **Public APIs**: 100% coverage required
- **Error paths**: 100% coverage required

Run coverage:
```bash
make test && make coverage-report
open _build/test/cover/index.html
```

### Test Organization

```erlang
% test/module_tests.erl - EUnit tests (unit)
-module(module_tests).
-include_lib("eunit/include/eunit.hrl").

simple_case_test() ->
    ?assertEqual(expected, module:function()).

% test/module_SUITE.erl - Common Test (integration)
-module(module_SUITE).
-include_lib("common_test/include/ct.hrl").

server_interaction(_Config) ->
    {ok, Pid} = module:start_link(),
    ok.

% test/properties_module.erl - PropEr (property-based)
-module(properties_module).
-include_lib("proper/include/proper.hrl").

prop_round_trip() ->
    ?FORALL(Data, valid_data(), encode_decode_match(Data)).
```

### Test Checklist

- [ ] Happy path test
- [ ] Error path test
- [ ] Edge cases test
- [ ] Type mismatch test
- [ ] Timeout test (if applicable)
- [ ] Concurrent access test (if applicable)
- [ ] Resource cleanup test

## Documentation Standards

### Code Documentation

```erlang
%% @doc Brief one-line description.
%% Longer description explaining behavior, parameters,
%% and important details.
%% @param InputName Description of input
%% @return Description of return value
%% @throws {error_type, Description}
%% @example
%% > erlmcp_example:my_function(<<"input">>)
%% {ok, <<"result">>}
-spec my_function(binary()) -> {ok, binary()} | {error, atom()}.
my_function(Input) ->
    % Implementation
    ok.
```

### Markdown Documentation

- Clear structure with headers
- Code examples that work
- Links to related docs
- Prerequisites clearly stated
- Estimated time to read/complete

## PR Review Checklist

Before submitting PR, verify:

- [ ] All tests pass: `make workspace-check`
- [ ] Coverage adequate: `make coverage-report` (80%+)
- [ ] Lint passes: `make workspace-lint` (0 warnings)
- [ ] Code formatted: `make format`
- [ ] Documentation updated
- [ ] No breaking changes (or documented)
- [ ] Commit messages clear
- [ ] PR description complete

## Merging Policy

PRs must:
1. Have all checks pass
2. Have 1+ approval from maintainers
3. Be up-to-date with main branch
4. Have squashed commits (optional)

Maintainers will merge using "Squash and merge" to keep history clean.

## Release Process

### Version Numbering

Uses semantic versioning: MAJOR.MINOR.PATCH

- **MAJOR**: Breaking changes
- **MINOR**: New features, backward compatible
- **PATCH**: Bug fixes

### Release Checklist

1. Update version in `src/erlmcp.app.src`
2. Update `CHANGELOG.md`
3. Run `make workspace-check`
4. Tag release: `git tag -a v0.6.0 -m "Release v0.6.0"`
5. Push: `git push origin v0.6.0`
6. Build release: `make workspace-release`
7. Create GitHub release with notes

## Performance Guidelines

### When Adding Code

1. **Consider performance impact**:
   - Does it allocate memory?
   - Does it add network calls?
   - Does it block other processes?

2. **Benchmark if significant**:
   ```bash
   make profile
   ```

3. **Document performance assumptions**:
   ```erlang
   %% Note: This function allocates O(N) memory
   %% For large N, consider streaming alternative
   ```

### When Reviewing

- Check for unnecessary allocations
- Look for potential deadlocks
- Consider concurrent scenarios
- Review timeout values

## Getting Help

- **Questions**: Open GitHub Discussion
- **Issues**: Report with reproduction steps
- **PRs**: Ask for guidance before large changes
- **Chat**: Join community discussions

## Resources

- **Erlang/OTP**: https://www.erlang.org/doc/
- **rebar3**: https://rebar3.org/
- **MCP Spec**: https://spec.modelcontextprotocol.io/
- **Community**: GitHub Issues, Discussions

## Code of Conduct

This project follows Contributor Covenant Code of Conduct. By participating, you agree to:
- Respect all contributors
- Provide constructive feedback
- Report harassment to maintainers
- Foster inclusive community

## License

By contributing, you agree your code will be licensed under Apache 2.0.

---

**Thank you for contributing!** 🚀

We appreciate:
- Code contributions
- Bug reports
- Feature requests
- Documentation improvements
- Testing and feedback

---

**Last Updated**: 2026-01-31
**Status**: Ready for contributions
**Maintainers**: See GitHub for current list
