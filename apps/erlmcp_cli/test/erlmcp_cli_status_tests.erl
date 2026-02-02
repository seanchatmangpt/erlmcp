%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_status_tests - OTP 28 Status Tests
%%%
%%% Tests for OTP 28 feature status display including JSON support,
%%% UTF-8 capabilities, process features, and session state.
%%%
%%% Chicago School TDD: Real processes, no mocks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_status_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup function - start status server
setup() ->
    {ok, Pid} = erlmcp_cli_status:start_link(),
    Pid.

%% @doc Cleanup function - stop status server
cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% OTP Information Tests
%%====================================================================

otp_info_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Capture output
                    Output = capture_output(fun erlmcp_cli_status:show_otp_info/0),

                    %% Verify OTP release is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"OTP Release:">>)),

                    %% Verify emulator is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Emulator:">>)),

                    %% Verify scheduler threads are shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Scheduler Threads:">>)),

                    %% Verify time correction is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Time Correction:">>))
                end)
         ]
     end}.

%%====================================================================
%% JSON Support Tests
%%====================================================================

json_support_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Capture output
                    Output = capture_output(fun erlmcp_cli_status:show_json_support/0),

                    %% Verify JSON engine is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"JSON Engine:">>)),

                    %% Verify UTF-8 validation is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"UTF-8 Validation:">>)),

                    %% Verify decoder is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Decoder:">>)),

                    %% Verify encoder is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Encoder:">>))
                end)
         ]
     end}.

%%====================================================================
%% UTF-8 Support Tests
%%====================================================================

utf8_support_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Capture output
                    Output = capture_output(fun erlmcp_cli_status:show_utf8_support/0),

                    %% Verify string handling is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"String Handling:">>)),

                    %% Verify case folding is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Case Folding:">>)),

                    %% Verify title case is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Title Case:">>)),

                    %% Verify binary matching is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Binary Matching:">>))
                end)
         ]
     end}.

%%====================================================================
%% Process Features Tests
%%====================================================================

process_features_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Capture output
                    Output = capture_output(fun erlmcp_cli_status:show_process_features/0),

                    %% Verify priority messages are shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Priority Messages:">>)),

                    %% Verify process iterators are shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Process Iterators:">>)),

                    %% Verify tagged monitors are shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Tagged Monitors:">>)),

                    %% Verify memory guards are shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Memory Guards:">>)),

                    %% Verify hibernation is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Hibernation:">>))
                end)
         ]
     end}.

%%====================================================================
%% Session State Tests
%%====================================================================

session_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Capture output
                    Output = capture_output(fun erlmcp_cli_status:show_session_state/0),

                    %% Verify active sessions are shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Active Sessions:">>)),

                    %% Verify parser priority is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Parser Priority:">>)),

                    %% Verify executor priority is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Executor Priority:">>)),

                    %% Verify transport priority is shown
                    ?assertNotEqual(nomatch, binary:match(Output, <<"Transport Priority:">>))
                end)
         ]
     end}.

%%====================================================================
%% Complete Status Tests
%%====================================================================

complete_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Capture output
                    Output = capture_output(fun erlmcp_cli_status:show_status/0),

                    %% Verify all sections are present
                    ?assertNotEqual(nomatch, binary:match(Output, <<"=== OTP Information ===">>)),
                    ?assertNotEqual(nomatch, binary:match(Output, <<"=== JSON Support ===">>)),
                    ?assertNotEqual(nomatch, binary:match(Output, <<"=== Process Features ===">>)),
                    ?assertNotEqual(nomatch, binary:match(Output, <<"=== UTF-8 Support ===">>)),
                    ?assertNotEqual(nomatch, binary:match(Output, <<"=== Session State ===">>))
                end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Capture stdout output from a function
-spec capture_output(fun()) -> binary().
capture_output(Fun) ->
    %% Redirect stdout to a group leader that captures output
    Pid = self(),
    Ref = make_ref(),

    %% Spawn a process to capture output
    Capturer = spawn(fun() ->
                           group_leader(self(), Pid),
                           Fun(),
                           Pid ! {done, Ref}
                   end),

    %% Collect output messages
    collect_output(Ref, <<>>).

%% @doc Collect output messages until done
-spec collect_output(reference(), binary()) -> binary().
collect_output(Ref, Acc) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _Encoding, Msg}} ->
            From ! {io_reply, ReplyAs, ok},
            collect_output(Ref, <<Acc/binary, Msg/binary>>);
        {done, Ref} ->
            Acc;
        _ ->
            collect_output(Ref, Acc)
    after 1000 ->
        Acc
    end.
