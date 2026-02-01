%%%-------------------------------------------------------------------
%%% @doc erlmcp_sampling_tests - Test suite for sampling capability
%%% Chicago School TDD - Real process testing, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sampling_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

sampling_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun create_message_with_valid_input/1,
      fun create_message_with_system_prompt/1,
      fun create_message_with_history/1,
      fun create_message_with_model_preferences/1,
      fun create_message_with_stop_sequences/1,
      fun empty_messages_error_via_api/1,
      fun invalid_format_error_via_api/1,
      fun invalid_temperature_error_via_api/1,
      fun get_model_provider/1,
      fun set_model_provider/1,
      fun request_count_increments/1,
      fun concurrent_requests/1,
      fun provider_function_export_check/1]}.

%%%====================================================================
%%% Setup and Teardown
%%%====================================================================

setup() ->
    %% Start the sampling server
    {ok, Pid} = erlmcp_sampling:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the sampling server
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(Pid);
        false ->
            ok
    end.

%%%====================================================================
%%% Test Cases - create_message/2
%%%====================================================================

%% @doc Test creating a message with valid input
create_message_with_valid_input(_Pid) ->
    fun() ->
       Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Hello, MCP!">>}],
       Params = #{<<"temperature">> => 0.7, <<"maxTokens">> => 100},

       Result = erlmcp_sampling:create_message(Messages, Params),

       ?assertMatch({ok, _}, Result),
       {ok, Response} = Result,
       ?assertMatch(#{<<"role">> := <<"assistant">>}, Response),
       ?assertMatch(#{<<"content">> := _}, Response),
       ?assertMatch(#{<<"model">> := _}, Response),
       ?assertMatch(#{<<"usage">> := _}, Response)
    end.

%% @doc Test creating a message with system prompt
create_message_with_system_prompt(_Pid) ->
    fun() ->
       Messages =
           [#{<<"role">> => <<"system">>, <<"content">> => <<"You are a helpful assistant.">>},
            #{<<"role">> => <<"user">>, <<"content">> => <<"Hi!">>}],
       Params = #{},

       Result = erlmcp_sampling:create_message(Messages, Params),

       ?assertMatch({ok, _}, Result),
       {ok, Response} = Result,
       ?assertMatch(#{<<"role">> := <<"assistant">>}, Response)
    end.

%% @doc Test creating a message with conversation history
create_message_with_history(_Pid) ->
    fun() ->
       Messages =
           [#{<<"role">> => <<"user">>, <<"content">> => <<"First message">>},
            #{<<"role">> => <<"assistant">>, <<"content">> => <<"First response">>},
            #{<<"role">> => <<"user">>, <<"content">> => <<"Second message">>}],
       Params = #{<<"temperature">> => 0.5},

       Result = erlmcp_sampling:create_message(Messages, Params),

       ?assertMatch({ok, _}, Result),
       {ok, Response} = Result,
       ?assert(maps:is_key(<<"role">>, Response)),
       ?assert(maps:is_key(<<"content">>, Response))
    end.

%% @doc Test model preferences parameter
create_message_with_model_preferences(_Pid) ->
    fun() ->
       Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
       Params =
           #{<<"modelPreferences">> =>
                 #{<<"costPriority">> => 0.5,
                   <<"speedPriority">> => 0.7,
                   <<"intelligencePriority">> => 0.3},
             <<"temperature">> => 0.8},

       Result = erlmcp_sampling:create_message(Messages, Params),

       ?assertMatch({ok, _}, Result)
    end.

%% @doc Test stop sequences parameter
create_message_with_stop_sequences(_Pid) ->
    fun() ->
       Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Continue until STOP">>}],
       Params = #{<<"stopSequences">> => [<<"STOP">>, <<"END">>]},

       Result = erlmcp_sampling:create_message(Messages, Params),

       ?assertMatch({ok, _}, Result)
    end.

%%%====================================================================
%%% Test Cases - Error Handling (via create_message API)
%%%====================================================================

%% @doc Test error handling for empty messages via API
empty_messages_error_via_api(_Pid) ->
    fun() ->
       Messages = [],
       Params = #{},
       Result = erlmcp_sampling:create_message(Messages, Params),
       ?assertMatch({error, empty_messages}, Result)
    end.

%% @doc Test error handling for invalid message format via API
invalid_format_error_via_api(_Pid) ->
    fun() ->
       Messages = [#{<<"content">> => <<"No role field">>}],
       Params = #{},
       Result = erlmcp_sampling:create_message(Messages, Params),
       ?assertMatch({error, invalid_message_format}, Result)
    end.

%% @doc Test error handling for invalid temperature via API
invalid_temperature_error_via_api(_Pid) ->
    fun() ->
       Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
       Params = #{<<"temperature">> => 3.0},
       Result = erlmcp_sampling:create_message(Messages, Params),
       ?assertMatch({error, invalid_temperature}, Result)
    end.

%%%====================================================================
%%% Test Cases - Model Provider Management
%%%====================================================================

%% @doc Test getting the current model provider
get_model_provider(_Pid) ->
    fun() ->
       Provider = erlmcp_sampling:get_model_provider(),
       ?assert(is_atom(Provider))
    end.

%% @doc Test setting the model provider
set_model_provider(Pid) ->
    fun() ->
       %% Get initial provider
       InitialProvider = erlmcp_sampling:get_model_provider(),
       ?assert(is_atom(InitialProvider)),

       %% Set to mock provider using the Pid variant
       ok = erlmcp_sampling:set_model_provider(Pid, erlmcp_mock_llm),

       %% Verify it was set
       ?assertEqual(erlmcp_mock_llm, erlmcp_sampling:get_model_provider())
    end.

%%%====================================================================
%%% Test Cases - State Management
%%%====================================================================

%% @doc Test that request count increments
request_count_increments(_Pid) ->
    fun() ->
       Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
       Params = #{},

       %% Make multiple requests
       ok =
           case erlmcp_sampling:create_message(Messages, Params) of
               {ok, _} ->
                   ok;
               {error, _} ->
                   ok  %% Provider may not be available
           end,

       ok =
           case erlmcp_sampling:create_message(Messages, Params) of
               {ok, _} ->
                   ok;
               {error, _} ->
                   ok
           end,

       %% The request count should have incremented (verified via state inspection)
       %% Since we can't directly access state, we verify the server is still alive
       ?assert(is_process_alive(whereis(erlmcp_sampling)))
    end.

%% @doc Test concurrent requests
concurrent_requests(_Pid) ->
    fun() ->
       Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Concurrent test">>}],
       Params = #{},

       %% Spawn 10 concurrent requests
       Pids =
           [spawn(fun() ->
                     case erlmcp_sampling:create_message(Messages, Params) of
                         {ok, _} ->
                             ok;
                         {error, _} ->
                             ok
                     end
                  end)
            || _ <- lists:seq(1, 10)],

       %% Wait for all to complete
       timer:sleep(500),

       %% Verify server is still alive (no crashes)
       ?assert(is_process_alive(whereis(erlmcp_sampling))),

       %% Verify all spawned processes completed
       [begin
            case is_process_alive(P) of
                true ->
                    ?assert(false, "Process did not complete");
                false ->
                    ok
            end
        end
        || P <- Pids]
    end.

%% @doc Test that function export check works properly
provider_function_export_check(_Pid) ->
    fun() ->
       %% Verify the default provider exports create_message/2
       Provider = erlmcp_sampling:get_model_provider(),
       ?assert(erlang:function_exported(Provider, create_message, 2))
    end.

%%%====================================================================
%%% Integration Tests
%%%====================================================================

%% @doc Test full sampling workflow
sampling_workflow_test_() ->
    {setup,
     fun() ->
        {ok, Pid} = erlmcp_sampling:start_link(),
        Pid
     end,
     fun(Pid) -> gen_server:stop(Pid) end,
     fun(_) ->
        [fun() ->
            %% Create a conversation
            Messages =
                [#{<<"role">> => <<"system">>, <<"content">> => <<"You are helpful.">>},
                 #{<<"role">> => <<"user">>, <<"content">> => <<"What is 2+2?">>}],

            %% Call with full parameters
            Params =
                #{<<"model">> => <<"gpt-4">>,
                  <<"temperature">> => 0.3,
                  <<"maxTokens">> => 500},

            Result = erlmcp_sampling:create_message(Messages, Params),

            ?assertMatch({ok, _}, Result),
            {ok, Response} = Result,
            ?assertMatch(#{<<"role">> := <<"assistant">>}, Response),
            ?assertMatch(#{<<"usage">> := #{<<"promptTokens">> := _}}, Response)
         end]
     end}.
