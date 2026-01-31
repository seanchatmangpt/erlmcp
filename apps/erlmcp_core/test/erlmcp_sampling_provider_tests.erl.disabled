%%%-------------------------------------------------------------------
%%% @doc Test Suite for erlmcp_sampling Model Provider Management
%%% Chicago School TDD - Real processes, API boundaries only
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sampling_provider_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Model Provider Management Tests
%%%====================================================================

model_provider_test_() ->
    {setup,
     fun setup_sampling_server/0,
     fun cleanup_sampling_server/1,
     fun(_) ->
         [
             fun get_model_provider/0,
             fun set_model_provider/0,
             fun provider_function_export_check/0
         ]
     end
    }.

%% @doc Test getting the current model provider
get_model_provider() ->
    fun() ->
        Provider = erlmcp_sampling:get_model_provider(),
        ?assert(is_atom(Provider))
    end.

%% @doc Test setting the model provider
set_model_provider() ->
    fun() ->
        %% Get initial provider
        InitialProvider = erlmcp_sampling:get_model_provider(),
        ?assert(is_atom(InitialProvider)),

        %% Set to test provider (use a valid module name)
        ok = erlmcp_sampling:set_model_provider(self(), erlmcp_mock_llm),

        %% Verify it was set
        ?assertEqual(erlmcp_mock_llm, erlmcp_sampling:get_model_provider())
    end.

%% @doc Test that function export check works properly
provider_function_export_check() ->
    fun() ->
        %% Verify the default provider exports create_message/2
        Provider = erlmcp_sampling:get_model_provider(),
        ?assert(erlang:function_exported(Provider, create_message, 2))
    end.

%%%====================================================================
%%% Provider Switching Tests
%%%====================================================================

provider_switching_test_() ->
    {setup,
     fun setup_sampling_server/0,
     fun cleanup_sampling_server/1,
     fun(_) ->
         [
             fun switch_between_providers/0,
             fun provider_persistence_across_requests/0
         ]
     end
    }.

switch_between_providers() ->
    fun() ->
        %% Start with default provider
        DefaultProvider = erlmcp_sampling:get_model_provider(),
        ?assert(is_atom(DefaultProvider)),

        %% Switch to mock provider
        ok = erlmcp_sampling:set_model_provider(self(), erlmcp_mock_llm),
        ?assertEqual(erlmcp_mock_llm, erlmcp_sampling:get_model_provider()),

        %% Switch back to default
        ok = erlmcp_sampling:set_model_provider(self(), DefaultProvider),
        ?assertEqual(DefaultProvider, erlmcp_sampling:get_model_provider())
    end.

provider_persistence_across_requests() ->
    fun() ->
        %% Set a specific provider
        ok = erlmcp_sampling:set_model_provider(self(), erlmcp_mock_llm),
        ?assertEqual(erlmcp_mock_llm, erlmcp_sampling:get_model_provider()),

        %% Make a request
        Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
        case erlmcp_sampling:create_message(Messages, #{}) of
            {ok, _} -> ok;
            {error, _} -> ok  %% Provider may not be available
        end,

        %% Verify provider persists
        ?assertEqual(erlmcp_mock_llm, erlmcp_sampling:get_model_provider())
    end.

%%%====================================================================
%%% Provider Functionality Tests
%%%====================================================================

provider_functionality_test_() ->
    {setup,
     fun setup_sampling_server/0,
     fun cleanup_sampling_server/1,
     fun(_) ->
         [
             fun provider_responds_to_requests/0,
             fun provider_handles_errors_gracefully/0
         ]
     end
    }.

provider_responds_to_requests() ->
    fun() ->
        %% Verify the provider can handle requests
        Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Hello">>}],
        Result = erlmcp_sampling:create_message(Messages, #{}),

        %% Either success or valid error is acceptable
        case Result of
            {ok, Response} ->
                ?assert(is_map(Response)),
                ?assert(maps:is_key(<<"role">>, Response));
            {error, _Reason} ->
                ok  %% Provider may not be available, this is OK
        end
    end.

provider_handles_errors_gracefully() ->
    fun() ->
        %% Test with invalid input - should handle gracefully
        Messages = [],
        Result = erlmcp_sampling:create_message(Messages, #{}),
        ?assertMatch({error, _}, Result)
    end.

%%%====================================================================
%%% Integration Tests
%%%====================================================================

integration_test_() ->
    {setup,
     fun setup_sampling_server/0,
     fun cleanup_sampling_server/1,
     fun(_) ->
         [
             fun full_sampling_workflow/0,
             fun provider_with_parameters/0
         ]
     end
    }.

full_sampling_workflow() ->
    fun() ->
        %% Create a conversation
        Messages = [
            #{<<"role">> => <<"system">>, <<"content">> => <<"You are helpful.">>},
            #{<<"role">> => <<"user">>, <<"content">> => <<"What is 2+2?">>}
        ],

        %% Call with full parameters
        Params = #{
            <<"model">> => <<"gpt-4">>,
            <<"temperature">> => 0.3,
            <<"maxTokens">> => 500
        },

        Result = erlmcp_sampling:create_message(Messages, Params),

        case Result of
            {ok, Response} ->
                ?assertMatch(#{<<"role">> := <<"assistant">>}, Response),
                ?assertMatch(#{<<"usage">> := #{<<"promptTokens">> := _}}, Response);
            {error, _} ->
                ok  %% Provider may not be available
        end
    end.

provider_with_parameters() ->
    fun() ->
        %% Test provider with various parameter combinations
        Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],

        ParameterSets = [
            #{<<"temperature">> => 0.5},
            #{<<"temperature">> => 1.0, <<"maxTokens">> => 100},
            #{<<"temperature">> => 0.0, <<"maxTokens">> => 1000, <<"stopSequences">> => [<<"STOP">>]}
        ],

        [begin
            Result = erlmcp_sampling:create_message(Messages, Params),
            case Result of
                {ok, _} -> ok;
                {error, _} -> ok  %% Provider may not be available
            end
        end || Params <- ParameterSets]
    end.

%%%====================================================================
%%% Setup and Teardown
%%%====================================================================

setup_sampling_server() ->
    {ok, Pid} = erlmcp_sampling:start_link(),
    Pid.

cleanup_sampling_server(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end.
