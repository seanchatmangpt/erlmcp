%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_zero_trust header file
%%% Zero-trust security architecture definitions
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_ZERO_TRUST_HRL).
-define(ERLMCP_ZERO_TRUST_HRL, true).

%% Zero-trust security levels
-define(ZT_LEVEL_NONE, 0).
-define(ZT_LEVEL_BASIC, 1).
-define(ZT_LEVEL_STANDARD, 2).
-define(ZT_LEVEL_HIGH, 3).
-define(ZT_LEVEL_CRITICAL, 4).

%% Zero-trust defaults
-define(ZT_DEFAULT_LEVEL, ?ZT_LEVEL_STANDARD).
-define(ZT_MAX_ATTEMPTS, 3).
-define(ZT_LOCKOUT_DURATION, 300).

-endif.
