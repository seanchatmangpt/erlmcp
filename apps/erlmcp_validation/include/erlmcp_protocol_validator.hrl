%%%-------------------------------------------------------------------
%%% @doc erlmcp_protocol_validator - Internal Records
%%% @end
%%%-------------------------------------------------------------------

-record(validation_state,
        {transport, results, test_client_pid, initialized, capabilities, session_id}).
-record(validation_result, {test_name, category, status, message, evidence, timestamp}).
