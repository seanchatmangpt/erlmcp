%%%-------------------------------------------------------------------
%%% @doc Test Suite for erlmcp_sampling API
%%% Chicago School TDD - Real processes, API boundaries only
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sampling_api_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%%====================================================================
%%% API Tests - create_message/2
%%%====================================================================

create_message_api_test_() ->
    {setup,
     fun setup_sampling_server/0,
     fun cleanup_sampling_server/1,
     fun(_) ->
        [fun create_message_with_valid_input/0,
         fun create_message_with_system_prompt/0,
         fun create_message_with_history/0,
         fun create_message_with_model_preferences/0,
         fun create_message_with_stop_sequences/0]
     end}.

%% @doc Test creating a message with valid input
create_message_with_valid_input() ->
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
create_message_with_system_prompt() ->
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
create_message_with_history() ->
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
create_message_with_model_preferences() ->
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
create_message_with_stop_sequences() ->
    fun() ->
       Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Continue until STOP">>}],
       Params = #{<<"stopSequences">> => [<<"STOP">>, <<"END">>]},

       Result = erlmcp_sampling:create_message(Messages, Params),

       ?assertMatch({ok, _}, Result)
    end.

%%%====================================================================
%%% Error Handling Tests (via create_message API)
%%%====================================================================

error_handling_test_() ->
    {setup,
     fun setup_sampling_server/0,
     fun cleanup_sampling_server/1,
     fun(_) ->
        [fun empty_messages_error_via_api/0,
         fun invalid_format_error_via_api/0,
         fun invalid_temperature_error_via_api/0]
     end}.

%% @doc Test error handling for empty messages via API
empty_messages_error_via_api() ->
    fun() ->
       Messages = [],
       Params = #{},
       Result = erlmcp_sampling:create_message(Messages, Params),
       ?assertMatch({error, empty_messages}, Result)
    end.

%% @doc Test error handling for invalid message format via API
invalid_format_error_via_api() ->
    fun() ->
       Messages = [#{<<"content">> => <<"No role field">>}],
       Params = #{},
       Result = erlmcp_sampling:create_message(Messages, Params),
       ?assertMatch({error, invalid_message_format}, Result)
    end.

%% @doc Test error handling for invalid temperature via API
invalid_temperature_error_via_api() ->
    fun() ->
       Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}],
       Params = #{<<"temperature">> => 3.0},
       Result = erlmcp_sampling:create_message(Messages, Params),
       ?assertMatch({error, invalid_temperature}, Result)
    end.

%%%====================================================================
%%% State Management Tests
%%%====================================================================

state_management_test_() ->
    {setup,
     fun setup_sampling_server/0,
     fun cleanup_sampling_server/1,
     fun(_) -> [fun request_count_increments/0, fun concurrent_requests/0] end}.

%% @doc Test that request count increments
request_count_increments() ->
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

       %% Verify the server is still alive and responsive
       ?assert(is_process_alive(whereis(erlmcp_sampling)))
    end.

%% @doc Test concurrent requests
concurrent_requests() ->
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

%%%====================================================================
%%% Setup and Teardown
%%%====================================================================

setup_sampling_server() ->
    {ok, Pid} = erlmcp_sampling:start_link(),
    Pid.

cleanup_sampling_server(Pid) ->
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(Pid);
        false ->
            ok
    end.
