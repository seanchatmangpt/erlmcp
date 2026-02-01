%%%-------------------------------------------------------------------
%%% @doc Mnesia Session Record Definitions
%%%
%%% Shared record definitions for Mnesia-based session storage.
%%% @end
%%%-------------------------------------------------------------------

-record(erlmcp_session,
        {session_id :: binary(), session_data :: map(), last_accessed :: integer()}).
