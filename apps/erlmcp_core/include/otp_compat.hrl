%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Compatibility Macros
%%%
%%% This header provides macros for OTP version-specific features
%%% and helper functions for safe cross-version development.
%%%
%%% @end
%%%-------------------------------------------------------------------

%% OTP 28+ Process Iterator API
-ifdef(OTP_RELEASE).
-if(OTP_RELEASE >= 28).
-define(HAVE_PROCESS_ITERATOR, true).
-define(SAFE_PROCESS_COUNT(), erlmcp_otp_compat:safe_process_count()).
-define(SET_PRIORITY_HIGH(), erlmcp_otp_compat:set_priority_high()).
-else.
-define(HAVE_PROCESS_ITERATOR, false).
-define(SAFE_PROCESS_COUNT(), erlang:system_info(process_count)).
-define(SET_PRIORITY_HIGH(), ok).
-endif.
-else.
%% No OTP_RELEASE defined (older OTP)
-define(HAVE_PROCESS_ITERATOR, false).
-define(SAFE_PROCESS_COUNT(), erlang:system_info(process_count)).
-define(SET_PRIORITY_HIGH(), ok).
-endif.
