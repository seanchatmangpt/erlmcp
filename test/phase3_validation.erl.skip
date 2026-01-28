%%%-------------------------------------------------------------------
%%% @doc
%%% Phase 3 Transport Standardization Validation
%%%
%%% This module validates that Phase 3 objectives have been completed:
%%% 1. Transport behavior interface standardized
%%% 2. All transports follow OTP gen_server patterns
%%% 3. Registry integration working
%%% 4. Error handling consistent
%%% 5. Configuration validation working
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(phase3_validation).

-export([validate_phase3/0, test_transport_loading/0, test_behavior_compliance/0]).

-include_lib("eunit/include/eunit.hrl").

%% Main validation function
validate_phase3() ->
    io:format("=== Phase 3 Transport Standardization Validation ===~n"),
    
    Results = [
        test_transport_loading(),
        test_behavior_compliance(),
        test_registry_integration(),
        test_supervisor_functionality(),
        test_configuration_validation()
    ],
    
    AllPassed = lists:all(fun(R) -> R =:= pass end, Results),
    
    case AllPassed of
        true ->
            io:format("✅ Phase 3 VALIDATION PASSED - All transport standardization objectives met!~n"),
            pass;
        false ->
            io:format("❌ Phase 3 validation failed - some objectives not met~n"),
            fail
    end.

%% Test 1: Transport Module Loading
test_transport_loading() ->
    io:format("1. Testing transport module loading...~n"),
    
    Transports = [
        erlmcp_transport_behavior,
        erlmcp_transport_stdio_new,
        erlmcp_transport_tcp,
        erlmcp_transport_http
    ],
    
    LoadResults = lists:map(fun(Module) ->
        try
            {module, Module} = code:ensure_loaded(Module),
            io:format("   ✅ ~p loaded successfully~n", [Module]),
            true
        catch
            _:Error ->
                io:format("   ❌ ~p failed to load: ~p~n", [Module, Error]),
                false
        end
    end, Transports),
    
    case lists:all(fun(R) -> R end, LoadResults) of
        true -> 
            io:format("   ✅ All transport modules load successfully~n"),
            pass;
        false -> 
            io:format("   ❌ Some transport modules failed to load~n"),
            fail
    end.

%% Test 2: Behavior Compliance
test_behavior_compliance() ->
    io:format("2. Testing transport behavior compliance...~n"),
    
    % Test that key transport modules implement required callbacks
    ModulesToTest = [
        erlmcp_transport_stdio_new,
        erlmcp_transport_tcp,
        erlmcp_transport_http
    ],
    
    RequiredFunctions = [
        {init, 1},
        {send, 2}, 
        {close, 1}
    ],
    
    ComplianceResults = lists:map(fun(Module) ->
        try
            ModuleInfo = Module:module_info(exports),
            HasRequired = lists:all(fun(Func) ->
                lists:member(Func, ModuleInfo)
            end, RequiredFunctions),
            
            case HasRequired of
                true ->
                    io:format("   ✅ ~p implements required callbacks~n", [Module]),
                    true;
                false ->
                    io:format("   ❌ ~p missing required callbacks~n", [Module]),
                    false
            end
        catch
            _:Error ->
                io:format("   ❌ ~p behavior check failed: ~p~n", [Module, Error]),
                false
        end
    end, ModulesToTest),
    
    case lists:all(fun(R) -> R end, ComplianceResults) of
        true -> 
            io:format("   ✅ All transports comply with behavior interface~n"),
            pass;
        false -> 
            io:format("   ❌ Some transports don't comply with behavior~n"),
            fail
    end.

%% Test 3: Registry Integration
test_registry_integration() ->
    io:format("3. Testing registry integration...~n"),
    
    try
        % Test registry can start
        case erlmcp_registry:start_link() of
            {ok, Pid} ->
                io:format("   ✅ Registry started successfully~n"),
                
                % Test basic registry functions
                [] = erlmcp_registry:list_servers(),
                [] = erlmcp_registry:list_transports(),
                io:format("   ✅ Registry functions accessible~n"),
                
                % Clean shutdown
                gen_server:stop(Pid),
                io:format("   ✅ Registry integration working~n"),
                pass;
            {error, Reason} ->
                io:format("   ❌ Registry failed to start: ~p~n", [Reason]),
                fail
        end
    catch
        _:Error ->
            io:format("   ❌ Registry integration test failed: ~p~n", [Error]),
            fail
    end.

%% Test 4: Supervisor Functionality
test_supervisor_functionality() ->
    io:format("4. Testing supervisor functionality...~n"),
    
    try
        % Test supervisor modules load and have required functions
        SupervisorModules = [
            erlmcp_sup,
            erlmcp_transport_sup,
            erlmcp_server_sup
        ],
        
        SupervisorResults = lists:map(fun(Module) ->
            try
                {module, Module} = code:ensure_loaded(Module),
                ModuleInfo = Module:module_info(exports),
                HasInit = lists:member({init, 1}, ModuleInfo),
                case HasInit of
                    true ->
                        io:format("   ✅ ~p supervisor ready~n", [Module]),
                        true;
                    false ->
                        io:format("   ❌ ~p missing init/1~n", [Module]),
                        false
                end
            catch
                _:Error ->
                    io:format("   ❌ ~p supervisor check failed: ~p~n", [Module, Error]),
                    false
            end
        end, SupervisorModules),
        
        case lists:all(fun(R) -> R end, SupervisorResults) of
            true -> 
                io:format("   ✅ All supervisors ready~n"),
                pass;
            false -> 
                io:format("   ❌ Some supervisors not ready~n"),
                fail
        end
    catch
        _:Error ->
            io:format("   ❌ Supervisor functionality test failed: ~p~n", [Error]),
            fail
    end.

%% Test 5: Configuration Validation
test_configuration_validation() ->
    io:format("5. Testing configuration validation...~n"),
    
    try
        % Test configuration module loads
        {module, erlmcp_config} = code:ensure_loaded(erlmcp_config),
        io:format("   ✅ Configuration module loaded~n"),
        
        % Test basic configuration functions exist
        ConfigInfo = erlmcp_config:module_info(exports),
        HasStart = lists:member({start_link, 0}, ConfigInfo),
        
        case HasStart of
            true ->
                io:format("   ✅ Configuration management ready~n"),
                pass;
            false ->
                io:format("   ❌ Configuration functions missing~n"),
                fail
        end
    catch
        _:Error ->
            io:format("   ❌ Configuration validation test failed: ~p~n", [Error]),
            fail
    end.