%%%-------------------------------------------------------------------
%%% @doc
%%% TCP Transport State Record Definition
%%%
%%% This header file defines the #state{} record used by
%%% erlmcp_transport_tcp gen_server and related tests.
%%%
%%% OTP 28 Features:
%%% - Hibernation support for idle connections
%%% - Priority message handling
%%% - UTF-8 validation
%%% - Enhanced health monitoring
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_TRANSPORT_TCP_HRL).

-define(ERLMCP_TRANSPORT_TCP_HRL, true).

%% OTP 28: Priority message levels
-type priority() :: normal | urgent | critical.

%% OTP 28: Hibernation state
-record(hibernate_state,
        {enabled = true :: boolean(),
         idle_threshold = 300000 :: non_neg_integer(),  %% 5 minutes
         last_activity :: integer() | undefined}).

%% OTP 28: Priority queue state
-record(priority_state,
        {urgent_queue = queue:new() :: queue:queue(),
         normal_queue = queue:new() :: queue:queue(),
         max_urgent_size = 100 :: non_neg_integer()}).

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
         initialized = false :: boolean(), %% CRITICAL: Flag to track successful handler init
         %% OTP 28: New fields
         hibernate :: #hibernate_state{},
         priority :: #priority_state{},
         utf8_validate = true :: boolean(),
         health_check_ref :: reference() | undefined}).

-endif.
