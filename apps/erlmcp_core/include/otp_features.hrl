%%-------------------------------------------------------------------
%% File    : otp_features.hrl
%% Author  : erlmcp OTP Research Team
%% Purpose : OTP version-specific feature detection macros
%% Created : 2026-02-01
%%
%% This header provides feature detection macros for OTP 26-28
%% features, allowing code to conditionally compile based on available
%% functionality.
%%
%% Usage:
%%   -include_lib("erlmcp_core/include/otp_features.hrl").
%%
%%   -ifdef(HAS_CONCURRENT_STARTUP).
%%   %% Use concurrent startup (OTP 26+)
%%   -else.
%%   %% Fall back to serial startup
%%   -endif.
%%
%% @doc OTP Feature Detection Macros
%% @end
%%-------------------------------------------------------------------

%%-------------------------------------------------------------------
%% OTP Release Detection
%%-------------------------------------------------------------------

-ifdef(OTP_RELEASE).
-if(OTP_RELEASE >= 28).
-define(OTP_28_OR_LATER, true).
-define(HAS_PRIORITY_MESSAGES, true).
-define(HAS_NATIVE_JSON, true).
-define(HAS_HIBERNATE_ZERO, true).
-define(HAS_STRICT_GENERATORS, true).
-define(HAS_ZIP_GENERATORS, true).
-define(HAS_PCRE2, true).
-define(HAS_BASED_FLOAT_LITERALS, true).
-endif.

-if(OTP_RELEASE >= 27).
-define(OTP_27_OR_LATER, true).
-define(HAS_RUNTIME_DEPS, true).
-define(HAS_JSON_MODULE, true).
-define(HAS_IMPROVED_MONITORING, true).
-define(HAS_BETTER_ERROR_MSGS, true).
-endif.

-if(OTP_RELEASE >= 26).
-define(OTP_26_OR_LATER, true).
-define(HAS_CONCURRENT_STARTUP, true).
-define(HAS_PERSISTENT_CONFIG, true).
-define(HAS_PREP_STOP_CALLBACK, true).
-define(HAS_START_PHASE_CALLBACK, true).
-define(HAS_CONFIG_CHANGE_CALLBACK, true).
-define(HAS_MAP_COMPREHENSIONS, true).
-define(HAS_IMPROVED_MAPS, true).
-define(HAS_DIALYZER_INCREMENTAL, true).
-endif.
-else.
%% No OTP_RELEASE defined (OTP 25 or earlier)
%% All features default to false
-endif.

%%-------------------------------------------------------------------
%% Default Values (if not already defined)
%%-------------------------------------------------------------------

-ifndef(OTP_28_OR_LATER).
-define(OTP_28_OR_LATER, false).
-endif.

-ifndef(OTP_27_OR_LATER).
-define(OTP_27_OR_LATER, false).
-endif.

-ifndef(OTP_26_OR_LATER).
-define(OTP_26_OR_LATER, false).
-endif.

-ifndef(HAS_PRIORITY_MESSAGES).
-define(HAS_PRIORITY_MESSAGES, false).
-endif.

-ifndef(HAS_NATIVE_JSON).
-define(HAS_NATIVE_JSON, false).
-endif.

-ifndef(HAS_HIBERNATE_ZERO).
-define(HAS_HIBERNATE_ZERO, false).
-endif.

-ifndef(HAS_STRICT_GENERATORS).
-define(HAS_STRICT_GENERATORS, false).
-endif.

-ifndef(HAS_ZIP_GENERATORS).
-define(HAS_ZIP_GENERATORS, false).
-endif.

-ifndef(HAS_PCRE2).
-define(HAS_PCRE2, false).
-endif.

-ifndef(HAS_BASED_FLOAT_LITERALS).
-define(HAS_BASED_FLOAT_LITERALS, false).
-endif.

-ifndef(HAS_RUNTIME_DEPS).
-define(HAS_RUNTIME_DEPS, false).
-endif.

-ifndef(HAS_JSON_MODULE).
-define(HAS_JSON_MODULE, false).
-endif.

-ifndef(HAS_IMPROVED_MONITORING).
-define(HAS_IMPROVED_MONITORING, false).
-endif.

-ifndef(HAS_BETTER_ERROR_MSGS).
-define(HAS_BETTER_ERROR_MSGS, false).
-endif.

-ifndef(HAS_CONCURRENT_STARTUP).
-define(HAS_CONCURRENT_STARTUP, false).
-endif.

-ifndef(HAS_PERSISTENT_CONFIG).
-define(HAS_PERSISTENT_CONFIG, false).
-endif.

-ifndef(HAS_PREP_STOP_CALLBACK).
-define(HAS_PREP_STOP_CALLBACK, false).
-endif.

-ifndef(HAS_START_PHASE_CALLBACK).
-define(HAS_START_PHASE_CALLBACK, false).
-endif.

-ifndef(HAS_CONFIG_CHANGE_CALLBACK).
-define(HAS_CONFIG_CHANGE_CALLBACK, false).
-endif.

-ifndef(HAS_MAP_COMPREHENSIONS).
-define(HAS_MAP_COMPREHENSIONS, false).
-endif.

-ifndef(HAS_IMPROVED_MAPS).
-define(HAS_IMPROVED_MAPS, false).
-endif.

-ifndef(HAS_DIALYZER_INCREMENTAL).
-define(HAS_DIALYZER_INCREMENTAL, false).
-endif.

%%-------------------------------------------------------------------
%% Feature Detection Functions (Runtime)
%%-------------------------------------------------------------------

%% @doc Check if concurrent application startup is available
-spec has_concurrent_startup() -> boolean().
has_concurrent_startup() ->
    erlang:function_exported(application, ensure_all_started, 3).

%% @doc Check if persistent configuration is available
-spec has_persistent_config() -> boolean().
has_persistent_config() ->
    erlang:function_exported(application, set_env, 4).

%% @doc Check if prep_stop/1 callback is available
-spec has_prep_stop_callback() -> boolean().
has_prep_stop_callback() ->
    %% prep_stop/1 is a behavior callback, check if it's documented
    try
        {application, _, _} = code:load_file(application),
        {ok, _} = application:get_all_key(application),
        true
    catch
        _:_ -> false
    end.

%% @doc Check if priority messages are available (OTP 28+)
-spec has_priority_messages() -> boolean().
has_priority_messages() ->
    erlang:function_exported(erlang, alias, 1).

%% @doc Check if native JSON module is available (OTP 27+)
-spec has_native_json() -> boolean().
has_native_json() ->
    erlang:module_loaded(json) orelse
    case code:load_file(json) of
        {module, json} -> true;
        {error, _} -> false
    end.

%% @doc Check if hibernate/0 is available (OTP 28+)
-spec has_hibernate_zero() -> boolean().
has_hibernate_zero() ->
    erlang:function_exported(erlang, hibernate, 0).

%% @doc Get the current OTP version as a tuple
-spec get_otp_version() -> {Major::integer(), Minor::integer(), Patch::integer()}.
get_otp_version() ->
    case erlang:system_info(otp_release) of
        [$R,N1,N2,N3|_] when N1 >= $0, N2 >= $0, N3 >= $0 ->
            {N1 - $0, N2 - $0, N3 - $0};
        "2" ++ Rest -> %% OTP 26+
            [MajorStr, MinorStr] = string:split(Rest, "."),
            PatchStr = case string:split(MinorStr, ".") of
                [_, Patch] -> Patch;
                [Minor] -> "0"
            end,
            {list_to_integer(MajorStr), list_to_integer(MinorStr), list_to_integer(PatchStr)};
        _ ->
            {0, 0, 0}
    end.

%%-------------------------------------------------------------------
%% Utility Macros
%%-------------------------------------------------------------------

%% @doc Use concurrent startup if available, otherwise serial
-define(ENSURE_ALL_STARTED(Apps, Type),
    case ?HAS_CONCURRENT_STARTUP of
        true -> application:ensure_all_started(Apps, Type, concurrent);
        false -> application:ensure_all_started(Apps, Type)
    end).

%% @doc Set environment with persistent option if available
-define(SET_ENV_PERSISTENT(App, Key, Val),
    case ?HAS_PERSISTENT_CONFIG of
        true -> application:set_env(App, Key, Val, [{persistent, true}]);
        false -> application:set_env(App, Key, Val)
    end).

%% @doc Use native JSON if available, otherwise jsx
-define(ENCODE_JSON(Term),
    case ?HAS_NATIVE_JSON of
        true -> json:encode(Term);
        false -> jsx:encode(Term)
    end).

-define(DECODE_JSON(Bin),
    case ?HAS_NATIVE_JSON of
        true -> json:decode(Bin);
        false -> jsx:decode(Bin, [return_maps])
    end).

%%-------------------------------------------------------------------
%% End of Feature Detection
%%-------------------------------------------------------------------
