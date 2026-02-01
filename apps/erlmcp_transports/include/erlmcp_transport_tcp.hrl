%%%-------------------------------------------------------------------
%%% @doc
%%% TCP Transport State Record Definition
%%%
%%% This header file defines the #state{} record used by
%%% erlmcp_transport_tcp gen_server and related tests.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_TRANSPORT_TCP_HRL).

-define(ERLMCP_TRANSPORT_TCP_HRL, true).

-record(state,
        {mode :: client | server,
         transport_id :: atom() | undefined,
         server_id :: atom() | undefined,
         socket :: gen_tcp:socket() | undefined,
         ranch_ref :: ranch:ref() | undefined,
         owner :: pid() | undefined,
         host :: inet:hostname() | inet:ip_address() | undefined,
         port :: inet:port_number() | undefined,
         options :: [gen_tcp:connect_option()],
         buffer = <<>> :: binary(),
         connected = false :: boolean(),
         reconnect_timer :: reference() | undefined,
         reconnect_attempts = 0 :: non_neg_integer(),
         max_reconnect_attempts = infinity :: pos_integer() | infinity,
         idle_timer :: reference() | undefined,
         resource_monitor_timer :: reference() | undefined,
         last_activity :: integer() | undefined,
         bytes_sent = 0 :: non_neg_integer(),
         bytes_received = 0 :: non_neg_integer(),
         max_message_size :: pos_integer(),
         initialized = false :: boolean()}). %% CRITICAL: Flag to track successful handler init

-endif.
