%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive functional test to validate what actually works
%%% This focuses on concrete functional validation
%%%-------------------------------------------------------------------
-module(actual_functionality_test).

-export([test_all/0]).

test_all() ->
    io:format("=== ERLMCP TRANSPORT FUNCTIONALITY VALIDATION ===~n~n"),

    %% Test transport module loading and function availability
    io:format("1. MODULE LOADING TESTS:~n"),
    test_module_loading(),

    %% Test basic transport functionality
    io:format("~n2. BASIC FUNCTIONALITY TESTS:~n"),
    test_basic_functionality(),

    %% Test transport integration
    io:format("~n3. INTEGRATION TESTS:~n"),
    test_integration(),

    %% Test message processing
    io:format("~n4. MESSAGE PROCESSING TESTS:~n"),
    test_message_processing(),

    %% Summary
    io:format("~n=== TEST SUMMARY ===~n"),
    io:format("Validation complete. Check output above for detailed results.~n").

%%====================================================================
%% Module Loading Tests
%%====================================================================

test_module_loading() ->
    Modules =
        [erlmcp_transport_stdio_new,
         erlmcp_transport_tcp_new,
         erlmcp_transport_http_new,
         erlmcp_registry,
         erlmcp_transport_behavior],

    lists:foreach(fun(Module) ->
                     case code:ensure_loaded(Module) of
                         {module, Module} ->
                             io:format("  â ~p: loaded successfully~n", [Module]),
                             test_module_exports(Module);
                         {error, Reason} ->
                             io:format("  â ~p: failed to load - ~p~n", [Module, Reason])
                     end
                  end,
                  Modules).

test_module_exports(Module) ->
    Exports = Module:module_info(exports),
    ExpectedFunctions = [{start_link, 2}, {send, 2}, {close, 1}, {get_info, 1}],

    case Module of
        erlmcp_registry ->
            % Registry has different functions
            RequiredFuncs = [{start_link, 0}, {find_transport, 1}],
            lists:foreach(fun({Func, Arity}) ->
                             case lists:member({Func, Arity}, Exports) of
                                 true ->
                                     io:format("    â ~p:~p/~p exported~n", [Module, Func, Arity]);
                                 false ->
                                     io:format("    â ~p:~p/~p NOT exported~n",
                                               [Module, Func, Arity])
                             end
                          end,
                          RequiredFuncs);
        erlmcp_transport_behavior ->
            % Behavior module has different exports
            ok;
        _ ->
            % Transport modules
            lists:foreach(fun({Func, Arity}) ->
                             case lists:member({Func, Arity}, Exports) of
                                 true ->
                                     io:format("    â ~p:~p/~p exported~n", [Module, Func, Arity]);
                                 false ->
                                     io:format("    â ~p:~p/~p NOT exported~n",
                                               [Module, Func, Arity])
                             end
                          end,
                          ExpectedFunctions)
    end.

%%====================================================================
%% Basic Functionality Tests
%%====================================================================

test_basic_functionality() ->
    test_stdio_basic(),
    test_tcp_basic(),
    test_http_basic().

test_stdio_basic() ->
    io:format("  STDIO Transport:~n"),
    try
        Config = #{test_mode => true},
        case erlmcp_transport_stdio_new:start_link(stdio_test, Config) of
            {ok, Pid} ->
                io:format("    ✓ Startup: successful~n"),

                % Test send
                case erlmcp_transport_stdio_new:send(Pid, <<"test message">>) of
                    ok ->
                        io:format("    ✓ Send: functional~n");
                    {error, Reason1} ->
                        io:format("    ✗ Send: failed - ~p~n", [Reason1])
                end,

                % Test get_info
                case catch erlmcp_transport_stdio_new:get_info(Pid) of
                    Info when is_map(Info) ->
                        io:format("    â Get Info: functional - type: ~p~n",
                                  [maps:get(type, Info, unknown)]);
                    {'EXIT', Reason2} ->
                        io:format("    ✗ Get Info: failed - ~p~n", [Reason2]);
                    Other2 ->
                        io:format("    ✗ Get Info: failed - ~p~n", [Other2])
                end,

                % Test close
                case catch erlmcp_transport_stdio_new:close(Pid) of
                    ok ->
                        io:format("    ✓ Close: functional~n");
                    {'EXIT', Reason3} ->
                        io:format("    ✗ Close: failed - ~p~n", [Reason3]);
                    Other3 ->
                        io:format("    ✗ Close: failed - ~p~n", [Other3])
                end,

                gen_server:stop(Pid, shutdown, 1000);
            Error ->
                io:format("    ✗ Startup: failed - ~p~n", [Error])
        end
    catch
        Class:Reason:Stack ->
            io:format("    ✗ Exception during STDIO test: ~p:~p~n", [Class, Reason])
    end.

test_tcp_basic() ->
    io:format("  TCP Transport:~n"),
    try
        Config =
            #{test_mode => true,
              host => "127.0.0.1",
              port => 8080},
        case erlmcp_transport_tcp_new:start_link(tcp_test, Config) of
            {ok, Pid} ->
                io:format("    ✓ Startup: successful~n"),

                % Test send
                case erlmcp_transport_tcp_new:send(Pid, <<"test message">>) of
                    ok ->
                        io:format("    ✓ Send: functional~n");
                    {error, Reason4} ->
                        io:format("    ✗ Send: failed - ~p~n", [Reason4])
                end,

                % Test get_info
                case catch erlmcp_transport_tcp_new:get_info(Pid) of
                    Info when is_map(Info) ->
                        io:format("    â Get Info: functional - type: ~p~n",
                                  [maps:get(type, Info, unknown)]);
                    {'EXIT', Reason5} ->
                        io:format("    ✗ Get Info: failed - ~p~n", [Reason5]);
                    Other5 ->
                        io:format("    ✗ Get Info: failed - ~p~n", [Other5])
                end,

                gen_server:stop(Pid, shutdown, 1000);
            Error ->
                io:format("    ✗ Startup: failed - ~p~n", [Error])
        end
    catch
        Class:Reason:Stack ->
            io:format("    ✗ Exception during TCP test: ~p:~p~n", [Class, Reason])
    end.

test_http_basic() ->
    io:format("  HTTP Transport:~n"),
    try
        Config =
            #{test_mode => true,
              port => 8080,
              path => "/mcp"},
        case erlmcp_transport_http_new:start_link(http_test, Config) of
            {ok, Pid} ->
                io:format("    ✓ Startup: successful~n"),

                % Test send
                case erlmcp_transport_http_new:send(Pid, <<"test message">>) of
                    ok ->
                        io:format("    ✓ Send: functional~n");
                    {error, Reason6} ->
                        io:format("    ✗ Send: failed - ~p~n", [Reason6])
                end,

                % Test get_info
                case catch erlmcp_transport_http_new:get_info(Pid) of
                    Info when is_map(Info) ->
                        io:format("    â Get Info: functional - type: ~p~n",
                                  [maps:get(type, Info, unknown)]);
                    {'EXIT', Reason7} ->
                        io:format("    ✗ Get Info: failed - ~p~n", [Reason7]);
                    Other7 ->
                        io:format("    ✗ Get Info: failed - ~p~n", [Other7])
                end,

                gen_server:stop(Pid, shutdown, 1000);
            Error ->
                io:format("    ✗ Startup: failed - ~p~n", [Error])
        end
    catch
        Class:Reason:Stack ->
            io:format("    ✗ Exception during HTTP test: ~p:~p~n", [Class, Reason])
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

test_integration() ->
    io:format("  Registry Integration:~n"),
    test_registry_integration(),

    io:format("  Transport Registration:~n"),
    test_transport_registration().

test_registry_integration() ->
    try
        case erlmcp_registry:start_link() of
            {ok, Pid} ->
                io:format("    ✓ Registry startup: successful~n"),

                % Test basic registry functions
                case catch erlmcp_registry:list_transports() of
                    List when is_list(List) ->
                        io:format("    ✓ List transports: functional (~p items)~n", [length(List)]);
                    Error ->
                        io:format("    ✗ List transports: failed - ~p~n", [Error])
                end,

                gen_server:stop(Pid, shutdown, 1000);
            {error, {already_started, _}} ->
                io:format("    ✓ Registry: already running~n");
            Error ->
                io:format("    ✗ Registry startup: failed - ~p~n", [Error])
        end
    catch
        Class:Reason ->
            io:format("    ✗ Registry integration exception: ~p:~p~n", [Class, Reason])
    end.

test_transport_registration() ->
    try
        % Ensure registry is running
        case whereis(erlmcp_registry) of
            undefined ->
                case erlmcp_registry:start_link() of
                    {ok, _} ->
                        ok;
                    {error, {already_started, _}} ->
                        ok;
                    Error ->
                        io:format("    ✗ Could not start registry: ~p~n", [Error]),
                        ok
                end;
            _ ->
                ok
        end,

        % Try to start transport and check registration
        Config = #{test_mode => true},
        case erlmcp_transport_stdio_new:start_link(reg_test_transport, Config) of
            {ok, Pid} ->
                timer:sleep(200), % Allow registration to complete

                case catch erlmcp_registry:find_transport(reg_test_transport) of
                    {ok, {RegPid, RegConfig}} ->
                        io:format("    â Transport registered: PID matches: ~p, Config has type: "
                                  "~p~n",
                                  [RegPid =:= Pid, maps:is_key(type, RegConfig)]);
                    {error, not_found} ->
                        io:format("    ✗ Transport not found in registry~n");
                    {'EXIT', Reason10} ->
                        io:format("    ✗ Registry lookup failed: ~p~n", [Reason10]);
                    Other10 ->
                        io:format("    ✗ Registry lookup failed: ~p~n", [Other10])
                end,

                gen_server:stop(Pid, shutdown, 1000);
            {error, Reason11} ->
                io:format("    ✗ Transport registration test failed: ~p~n", [Reason11])
        end
    catch
        Class:Reason ->
            io:format("    ✗ Registration test exception: ~p:~p~n", [Class, Reason])
    end.

%%====================================================================
%% Message Processing Tests
%%====================================================================

test_message_processing() ->
    io:format("  Message Processing:~n"),
    test_message_formats(),
    test_line_processing().

test_message_formats() ->
    try
        % Test JSON message creation (if transport behavior module has it)
        case catch erlmcp_transport:create_message(<<"test_method">>, #{param => value}, 1) of
            Message when is_map(Message) ->
                io:format("    ✓ Message creation: functional~n"),
                case maps:get(<<"method">>, Message, undefined) of
                    <<"test_method">> ->
                        io:format("    ✓ Message format: correct~n");
                    _ ->
                        io:format("    ✗ Message format: incorrect~n")
                end;
            Error ->
                io:format("    ✗ Message creation: failed - ~p~n", [Error])
        end
    catch
        Class:Reason ->
            io:format("    ✗ Message format test exception: ~p:~p~n", [Class, Reason])
    end.

test_line_processing() ->
    try
        % Test line trimming function from STDIO transport
        TestCases =
            [{<<"hello\n">>, <<"hello">>},
             {<<"hello\r\n">>, <<"hello">>},
             {<<"hello">>, <<"hello">>},
             {<<"\n">>, <<>>}],

        Results =
            lists:map(fun({Input, Expected}) ->
                         case catch erlmcp_transport_stdio_new:trim_line(Input) of
                             Expected -> true;
                             Other ->
                                 io:format("    â Line trim failed: ~p -> ~p (expected ~p)~n",
                                           [Input, Other, Expected]),
                                 false
                         end
                      end,
                      TestCases),

        case lists:all(fun(R) -> R end, Results) of
            true ->
                io:format("    ✓ Line processing: all test cases passed~n");
            false ->
                io:format("    ✗ Line processing: some test cases failed~n")
        end
    catch
        Class:Reason ->
            io:format("    ✗ Line processing test exception: ~p:~p~n", [Class, Reason])
    end.
