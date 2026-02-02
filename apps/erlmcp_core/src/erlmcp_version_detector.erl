%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Version Detection and Support Level Assessment
%%%
%%% This module provides comprehensive OTP version detection and support
%%% level assessment for erlmcp multi-version compatibility.
%%%
%%% Support Levels:
%%%   - unsupported: OTP < 26
%%%   - legacy: OTP 26-27 (degraded features)
%%%   - stable: OTP 27 (standard features)
%%%   - recommended: OTP 28.3.1+ (full features)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_version_detector).

%% API
-export([
    otp_version/0,
    otp_version_tuple/0,
    otp_version_string/0,
    is_otp_supported/0,
    get_support_level/0,
    get_optimal_features/0,
    get_minimum_version/0,
    get_recommended_version/0,
    compare_versions/2,
    is_version_at_least/2
]).

%% Types
-type otp_version() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type support_level() :: unsupported | legacy | stable | recommended.
-type feature_flag() ::
    {native_json, boolean()} |
    {process_iterator, boolean()} |
    {priority_messages, boolean()} |
    {eep48_maps, boolean()} |
    {eep72_streams, boolean()} |
    {eep76_gc, boolean()} |
    {advanced_maps, boolean()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get current OTP version as {Major, Minor, Patch}
-spec otp_version() -> otp_version().
otp_version() ->
    case otp_version_string() of
        Vsn when is_list(Vsn) ->
            parse_otp_version(Vsn);
        _ ->
            {0, 0, 0}  % Fallback for unexpected format
    end.

%% @doc Get OTP version as tuple
-spec otp_version_tuple() -> otp_version().
otp_version_tuple() ->
    otp_version().

%% @doc Get OTP version as string
-spec otp_version_string() -> string().
otp_version_string() ->
    erlang:system_info(otp_release).

%% @doc Check if current OTP version is supported
-spec is_otp_supported() -> boolean().
is_otp_supported() ->
    case get_support_level() of
        unsupported -> false;
        _ -> true
    end.

%% @doc Get support level for current OTP version
-spec get_support_level() -> support_level().
get_support_level() ->
    Version = otp_version(),
    get_support_level(Version).

%% @doc Get optimal features available for current OTP version
-spec get_optimal_features() -> [feature_flag()].
get_optimal_features() ->
    Version = otp_version(),
    get_features_for_version(Version).

%% @doc Get minimum supported OTP version
-spec get_minimum_version() -> otp_version().
get_minimum_version() ->
    {26, 0, 0}.

%% @doc Get recommended OTP version
-spec get_recommended_version() -> otp_version().
get_recommended_version() ->
    {28, 3, 1}.

%% @doc Compare two OTP versions
%% Returns: gt | eq | lt
-spec compare_versions(otp_version(), otp_version()) -> gt | eq | lt.
compare_versions({M1, N1, P1}, {M2, N2, P2}) ->
    case {M1, N1, P1} of
        {M2, N2, P2} -> eq;
        {M, _, _} when M > M2 -> gt;
        {M, _, _} when M < M2 -> lt;
        {M1, N1, _} when M1 =:= M2 ->
            case {N1, N2} of
                {N, _} when N > N2 -> gt;
                {N, _} when N < N2 -> lt;
                {N1, P1} when N1 =:= N2 ->
                    case P1 of
                        P when P > P2 -> gt;
                        P when P < P2 -> lt;
                        _ -> eq
                    end
            end
    end.

%% @doc Check if current version is at least the specified version
-spec is_version_at_least(otp_version(), otp_version()) -> boolean().
is_version_at_least(Current, Required) ->
    compare_versions(Current, Required) =/= lt.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get support level for specific version
-spec get_support_level(otp_version()) -> support_level().
get_support_level(Version) ->
    case Version of
        {M, _, _} when M < 26 -> unsupported;
        {26, _, _} -> legacy;
        {27, _, _} -> stable;
        {M, _, _} when M >= 28 -> recommended
    end.

%% @private Get features available for specific version
-spec get_features_for_version(otp_version()) -> [feature_flag()].
get_features_for_version(Version) ->
    #{
        native_json => has_native_json(Version),
        process_iterator => has_process_iterator(Version),
        priority_messages => has_priority_messages(Version),
        eep48_maps => has_eep48_maps(Version),
        eep72_streams => has_eep72_streams(Version),
        eep76_gc => has_eep76_gc(Version),
        advanced_maps => has_advanced_maps(Version)
    }.

%% @private Check if native JSON is available
-spec has_native_json(otp_version()) -> boolean().
has_native_json(Version) ->
    case Version of
        {M, _, _} when M >= 27 -> true;
        _ -> false
    end.

%% @private Check if process iterator is available
-spec has_process_iterator(otp_version()) -> boolean().
has_process_iterator(Version) ->
    case Version of
        {M, _, _} when M >= 28 -> true;
        _ -> false
    end.

%% @private Check if priority messages are available
-spec has_priority_messages(otp_version()) -> boolean().
has_priority_messages(Version) ->
    case Version of
        {M, _, _} when M >= 28 -> true;
        _ -> false
    end.

%% @private Check if EEP 48 maps are available
-spec has_eep48_maps(otp_version()) -> boolean().
has_eep48_maps(_Version) ->
    true.  % EEP 48 maps are available since OTP 17

%% @private Check if EEP 72 streams are available
-spec has_eep72_streams(otp_version()) -> boolean().
has_eep72_streams(Version) ->
    case Version of
        {M, _, _} when M >= 27 -> true;
        _ -> false
    end.

%% @private Check if EEP 76 GC is available
-spec has_eep76_gc(otp_version()) -> boolean().
has_eep76_gc(Version) ->
    case Version of
        {M, N, _} when M >= 28, N >= 3 -> true;  % OTP 28.3+
        {M, _, _} when M > 28 -> true;
        _ -> false
    end.

%% @private Check if advanced maps are available
-spec has_advanced_maps(otp_version()) -> boolean().
has_advanced_maps(Version) ->
    case Version of
        {M, _, _} when M >= 27 -> true;
        _ -> false
    end.

%% @private Parse OTP version string
-spec parse_otp_version(string()) -> otp_version().
parse_otp_version(VsnStr) ->
    Parts = string:split(VsnStr, ".", all),
    ToInt =
        fun(Str) ->
           case string:to_integer(Str) of
               {Int, ""} ->
                   Int;
               {Int, _} ->
                   Int;
               _ ->
                   0
           end
        end,
    Padded =
        case Parts of
            [M1] ->
                [M1, "0", "0"];
            [M1, M2] ->
                [M1, M2, "0"];
            [M1, M2, M3 | _] ->
                [M1, M2, M3]
        end,
    [Major, Minor, Patch] = [ToInt(P) || P <- Padded],
    {Major, Minor, Patch}.