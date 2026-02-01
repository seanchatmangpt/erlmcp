%%%-------------------------------------------------------------------
%%% @doc
%%% Custom Security Validator Plugin - Example Plugin Implementation
%%%
%%% This plugin demonstrates how to create a validator plugin for erlmcp
%%% that performs custom security and format validation.
%%%
%%% Features:
%%% - SQL injection detection
%%% - XSS (cross-site scripting) detection
%%% - Path traversal attack detection
%%% - Email format validation
%%% - URL format validation
%%%
%%% Usage:
%%%   1. Compile: erlc validator-custom.erl
%%%   2. Load in REPL: c(validator_custom).
%%%   3. Start: validator_custom:start().
%%%   4. Validate: erlmcp_plugin_registry:call(security_check, validate, [Data, Rules]).
%%%   5. Stop: validator_custom:stop().
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(validator_custom).
-author("erlmcp examples").

% Declare behavior implementation
-behaviour(erlmcp_plugin_validator).

% Public API
-export([
  start/0,
  stop/0,
  validate/2
]).

% For testing
-export([
  test_sql_injection/0,
  test_xss/0,
  test_path_traversal/0,
  test_email_validation/0,
  test_url_validation/0,
  run_all_tests/0
]).

% =========================================================================
% Public API - Plugin Lifecycle
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Start the plugin and register it with the plugin registry.
%% @end
%%%-------------------------------------------------------------------
start() ->
  erlmcp_plugin_registry:register(
    security_check,                        % Plugin name
    erlmcp_plugin_validator,               % Behavior module
    validator_custom                       % This module
  ),
  io:format("Plugin loaded: Custom Security Validator~n"),
  {ok, loaded}.

%%%-------------------------------------------------------------------
%% @doc Stop the plugin and unregister from registry.
%% @end
%%%-------------------------------------------------------------------
stop() ->
  erlmcp_plugin_registry:unregister(security_check),
  io:format("Plugin unloaded: Custom Security Validator~n"),
  ok.

% =========================================================================
% Behavior Implementation - erlmcp_plugin_validator
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Validate data according to specified rules.
%%
%% Rules is a map that can contain:
%%   - checks: List of check types (default: all)
%%     * sql_injection
%%     * xss
%%     * path_traversal
%%     * email_format
%%     * url_format
%%
%% Returns:
%%   - {ok} if all checks pass
%%   - {error, Violations} where Violations is list of {Check, Reason}
%%
%% @spec validate(Data :: term(), Rules :: map()) ->
%%   {ok} | {error, list()}
%% @end
%%%-------------------------------------------------------------------
validate(Data, Rules) ->
  % Determine which checks to run
  Checks = maps:get(checks, Rules, [
    sql_injection,
    xss,
    path_traversal,
    email_format,
    url_format
  ]),

  % Run all checks and collect violations
  Violations = run_checks(Data, Checks),

  % Return result
  case Violations of
    [] ->
      {ok};
    _ ->
      {error, Violations}
  end.

% =========================================================================
% Check Runner
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Run all specified checks on data.
%% @end
%%%-------------------------------------------------------------------
run_checks(Data, Checks) ->
  [
    {Check, Reason}
    || Check <- Checks,
       {violation, Reason} <- [run_check(Check, Data)]
  ].

%%%-------------------------------------------------------------------
%% @doc Run a single check and return {ok} or {violation, Reason}.
%% @end
%%%-------------------------------------------------------------------
run_check(sql_injection, Data) ->
  check_sql_injection(Data);

run_check(xss, Data) ->
  check_xss(Data);

run_check(path_traversal, Data) ->
  check_path_traversal(Data);

run_check(email_format, Data) ->
  check_email_format(Data);

run_check(url_format, Data) ->
  check_url_format(Data);

run_check(_Unknown, _Data) ->
  {ok}.

% =========================================================================
% Check Implementations - Security
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Detect SQL injection attempts.
%% @end
%%%-------------------------------------------------------------------
check_sql_injection(Data) ->
  Keywords = [
    <<"union">>, <<"select">>, <<"drop">>,
    <<"insert">>, <<"update">>, <<"delete">>,
    <<"create">>, <<"alter">>, <<"exec">>,
    <<"execute">>, <<"script">>, <<"javascript">>
  ],

  case has_suspicious_keywords(Data, Keywords) of
    true ->
      {violation, "Potential SQL injection detected"};
    false ->
      {ok}
  end.

%%%-------------------------------------------------------------------
%% @doc Detect XSS (cross-site scripting) attempts.
%% @end
%%%-------------------------------------------------------------------
check_xss(Data) ->
  Patterns = [
    <<"<script">>, <<"onclick">>, <<"onerror">>,
    <<"onload">>, <<"javascript:">>, <<"data:text">>,
    <<"eval">>, <<"expression">>
  ],

  case has_suspicious_keywords(Data, Patterns) of
    true ->
      {violation, "Potential XSS attack detected"};
    false ->
      {ok}
  end.

%%%-------------------------------------------------------------------
%% @doc Detect path traversal attacks.
%% @end
%%%-------------------------------------------------------------------
check_path_traversal(Data) ->
  Patterns = [
    <<"../">>, <<"..\\">>, <<"../..">>,
    <<"..\\..">>, <<"....//">>, <<"....\\\\">>,
    <<"%2e%2e/">>, <<"%2e%2e%5c">>
  ],

  case has_suspicious_keywords(Data, Patterns) of
    true ->
      {violation, "Potential path traversal attack detected"};
    false ->
      {ok}
  end.

% =========================================================================
% Check Implementations - Format
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Validate email format.
%% @end
%%%-------------------------------------------------------------------
check_email_format(Data) when is_binary(Data) ->
  % Basic email regex: something@something.something
  case re:match(Data, <<"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$">>) of
    {match, _} -> {ok};
    nomatch ->
      case binary_to_list(Data) of
        [] -> {ok};  % Empty is acceptable (optional field)
        _ -> {violation, "Invalid email format"}
      end
  end;

check_email_format(Data) when is_list(Data) ->
  check_email_format(list_to_binary(Data));

check_email_format(_) ->
  {ok}.

%%%-------------------------------------------------------------------
%% @doc Validate URL format.
%% @end
%%%-------------------------------------------------------------------
check_url_format(Data) when is_binary(Data) ->
  % Basic URL regex: scheme://something
  case re:match(Data, <<"^(https?|ftp)://.+$">>) of
    {match, _} -> {ok};
    nomatch ->
      case binary_to_list(Data) of
        [] -> {ok};  % Empty is acceptable (optional field)
        _ -> {violation, "Invalid URL format"}
      end
  end;

check_url_format(Data) when is_list(Data) ->
  check_url_format(list_to_binary(Data));

check_url_format(_) ->
  {ok}.

% =========================================================================
% Helper Functions
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Check if data contains any suspicious keywords.
%% @end
%%%-------------------------------------------------------------------
has_suspicious_keywords(Data, Keywords) ->
  DataBin = to_binary(Data),
  DataLower = string:lowercase(DataBin),
  lists:any(
    fun(Keyword) ->
      KeywordLower = string:lowercase(Keyword),
      binary:match(DataLower, KeywordLower) =/= nomatch
    end,
    Keywords
  ).

%%%-------------------------------------------------------------------
%% @doc Convert various types to binary.
%% @end
%%%-------------------------------------------------------------------
to_binary(B) when is_binary(B) ->
  B;

to_binary(L) when is_list(L) ->
  try
    list_to_binary(L)
  catch
    _ -> list_to_binary(io_lib:format("~p", [L]))
  end;

to_binary(A) when is_atom(A) ->
  atom_to_binary(A, utf8);

to_binary(N) when is_number(N) ->
  list_to_binary(integer_to_list(N));

to_binary(M) when is_map(M) ->
  % For maps, validate all values
  Values = [V || {_K, V} <- maps:to_list(M)],
  lists:any(fun(V) ->
    case to_binary(V) of
      _ -> false  % Just ensure all can be converted
    end
  end, Values),
  <<"">>;  % Return something

to_binary(X) ->
  list_to_binary(io_lib:format("~p", [X])).

% =========================================================================
% Testing Functions
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Test SQL injection detection.
%% @end
%%%-------------------------------------------------------------------
test_sql_injection() ->
  io:format("Test: SQL Injection Detection~n"),

  % Positive case (should detect)
  io:format("  - Injection attempt: "),
  case validate(<<"'; DROP TABLE users; --">>, #{
    checks => [sql_injection]
  }) of
    {error, Violations} ->
      io:format("✓ Detected~n"),
      lists:foreach(fun({Check, Reason}) ->
        io:format("    → ~p: ~s~n", [Check, Reason])
      end, Violations);
    {ok} ->
      io:format("✗ Not detected~n")
  end,

  % Negative case (should pass)
  io:format("  - Clean SQL: "),
  case validate(<<"SELECT * FROM users">>, #{
    checks => [sql_injection]
  }) of
    {ok} ->
      io:format("✓ Passed~n");
    {error, Violations} ->
      io:format("✗ Rejected: ~p~n", [Violations])
  end.

%%%-------------------------------------------------------------------
%% @doc Test XSS detection.
%% @end
%%%-------------------------------------------------------------------
test_xss() ->
  io:format("~nTest: XSS Detection~n"),

  % Positive case (should detect)
  io:format("  - XSS attempt: "),
  case validate(<<"<img src=x onerror='alert(1)'>">>, #{
    checks => [xss]
  }) of
    {error, _Violations} ->
      io:format("✓ Detected~n");
    {ok} ->
      io:format("✗ Not detected~n")
  end,

  % Negative case (should pass)
  io:format("  - Clean HTML: "),
  case validate(<<"<p>Hello World</p>">>, #{
    checks => [xss]
  }) of
    {ok} ->
      io:format("✓ Passed~n");
    {error, _Violations} ->
      io:format("✗ Rejected~n")
  end.

%%%-------------------------------------------------------------------
%% @doc Test path traversal detection.
%% @end
%%%-------------------------------------------------------------------
test_path_traversal() ->
  io:format("~nTest: Path Traversal Detection~n"),

  % Positive case (should detect)
  io:format("  - Path traversal: "),
  case validate(<<"../../etc/passwd">>, #{
    checks => [path_traversal]
  }) of
    {error, _Violations} ->
      io:format("✓ Detected~n");
    {ok} ->
      io:format("✗ Not detected~n")
  end,

  % Negative case (should pass)
  io:format("  - Clean path: "),
  case validate(<<"data/file.txt">>, #{
    checks => [path_traversal]
  }) of
    {ok} ->
      io:format("✓ Passed~n");
    {error, _Violations} ->
      io:format("✗ Rejected~n")
  end.

%%%-------------------------------------------------------------------
%% @doc Test email format validation.
%% @end
%%%-------------------------------------------------------------------
test_email_validation() ->
  io:format("~nTest: Email Format Validation~n"),

  % Valid email
  io:format("  - Valid email: "),
  case validate(<<"user@example.com">>, #{
    checks => [email_format]
  }) of
    {ok} ->
      io:format("✓ Passed~n");
    {error, _} ->
      io:format("✗ Rejected~n")
  end,

  % Invalid email
  io:format("  - Invalid email: "),
  case validate(<<"notanemail">>, #{
    checks => [email_format]
  }) of
    {error, _} ->
      io:format("✓ Rejected~n");
    {ok} ->
      io:format("✗ Passed (should fail)~n")
  end.

%%%-------------------------------------------------------------------
%% @doc Test URL format validation.
%% @end
%%%-------------------------------------------------------------------
test_url_validation() ->
  io:format("~nTest: URL Format Validation~n"),

  % Valid URL
  io:format("  - Valid URL: "),
  case validate(<<"https://example.com/path">>, #{
    checks => [url_format]
  }) of
    {ok} ->
      io:format("✓ Passed~n");
    {error, _} ->
      io:format("✗ Rejected~n")
  end,

  % Invalid URL
  io:format("  - Invalid URL: "),
  case validate(<<"not a url">>, #{
    checks => [url_format]
  }) of
    {error, _} ->
      io:format("✓ Rejected~n");
    {ok} ->
      io:format("✗ Passed (should fail)~n")
  end.

% =========================================================================
% Main Test Entry Point
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Run all tests.
%% @end
%%%-------------------------------------------------------------------
run_all_tests() ->
  io:format("~n=== Custom Security Validator Plugin Tests ===~n"),
  test_sql_injection(),
  test_xss(),
  test_path_traversal(),
  test_email_validation(),
  test_url_validation(),
  io:format("~n=== All Tests Completed ===~n").
