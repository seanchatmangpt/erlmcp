%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Compatibility Helper Functions
%%%
%%% This module provides helper functions used by the otp_compat.hrl
%%% header file for OTP version detection and feature fallbacks.
%%%
%%% @end
%%% @private
%%%-------------------------------------------------------------------
-module(erlmcp_otp_compat).

%% API
-export([otp_version/0]).
-export([is_otp_28_plus/0]).
-export([is_otp_27_plus/0]).
-export([have_native_json/0]).
-export([have_process_iterator/0]).
-export([have_priority_messages/0]).
-export([json_encode/1]).
-export([json_decode/1]).
-export([safe_process_count/0]).
-export([safe_processes/0]).
-export([count_processes_iterator/2]).
-export([processes_iterator_to_list/2]).
-export([set_priority_high/0]).
-export([send_priority/2]).

%% Types
-type otp_version() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get current OTP version as {Major, Minor, Patch}
-spec otp_version() -> otp_version().
otp_version() ->
    VersionStr = erlang:system_info(otp_release),
    parse_otp_version(VersionStr).

%% @doc Check if running on OTP 28+
-spec is_otp_28_plus() -> boolean().
is_otp_28_plus() ->
    case otp_version() of
        {Major, _, _} when Major >= 28 ->
            true;
        _ ->
            false
    end.

%% @doc Check if running on OTP 27+
-spec is_otp_27_plus() -> boolean().
is_otp_27_plus() ->
    case otp_version() of
        {Major, _, _} when Major >= 27 ->
            true;
        _ ->
            false
    end.

%% @doc Check if native JSON module is available
-spec have_native_json() -> boolean().
have_native_json() ->
    erlang:function_exported(json, encode, 1).

%% @doc Check if process iterator API is available
-spec have_process_iterator() -> boolean().
have_process_iterator() ->
    erlang:function_exported(erlang, processes_iterator, 0).

%% @doc Check if priority messages are available
-spec have_priority_messages() -> boolean().
have_priority_messages() ->
    is_otp_28_plus().

%%====================================================================
%% JSON Encoding/Decoding (safe)
%%====================================================================

%% @doc Encode data to JSON with automatic module selection
-spec json_encode(map() | list()) -> binary().
json_encode(Data) ->
    case have_native_json() of
        true ->
            json:encode(Data);
        false ->
            erlmcp_json_native:encode(Data)
    end.

%% @doc Decode JSON with automatic module selection
-spec json_decode(binary()) -> map() | list().
json_decode(Binary) ->
    case have_native_json() of
        true ->
            json:decode(Binary);
        false ->
            erlmcp_json_native:decode(Binary)
    end.

%%====================================================================
%% Process Enumeration (safe)
%%====================================================================

%% @doc Get process count safely (O(1) memory on OTP 28+)
-spec safe_process_count() -> non_neg_integer().
safe_process_count() ->
    case have_process_iterator() of
        true ->
            Iterator = erlang:processes_iterator(),
            count_processes_iterator(Iterator, 0);
        false ->
            erlang:system_info(process_count)
    end.

%% @doc Get list of all processes (with warnings for large counts)
-spec safe_processes() -> [pid()].
safe_processes() ->
    Count = safe_process_count(),
    case Count > 10000 of
        true ->
            logger:warning("Process enumeration of ~p processes may be inefficient. "
                           "Consider upgrading to OTP 28.3.1+ for O(1) memory.",
                           [Count]),
            get_processes();
        false ->
            get_processes()
    end.

%% @private
%% @doc Get processes using appropriate method
-spec get_processes() -> [pid()].
get_processes() ->
    case have_process_iterator() of
        true ->
            Iterator = erlang:processes_iterator(),
            processes_iterator_to_list(Iterator, []);
        false ->
            erlang:processes()
    end.

%% @private
%% @doc Count processes using iterator (OTP 28+)
%% Called by ?SAFE_PROCESS_COUNT() macro
-spec count_processes_iterator(term(), non_neg_integer()) -> non_neg_integer().
count_processes_iterator(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {_Pid, NewIterator} ->
            count_processes_iterator(NewIterator, Acc + 1);
        none ->
            Acc
    end.

%% @private
%% @doc Convert process iterator to list (OTP 28+)
%% Called by ?SAFE_PROCESSES() macro
-spec processes_iterator_to_list(term(), [pid()]) -> [pid()].
processes_iterator_to_list(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} ->
            processes_iterator_to_list(NewIterator, [Pid | Acc]);
        none ->
            lists:reverse(Acc)
    end.

%%====================================================================
%% Priority Messages
%%====================================================================

%% @doc Set process priority to high (no-op on OTP <28)
-spec set_priority_high() -> ok.
set_priority_high() ->
    case have_priority_messages() of
        true ->
            process_flag(priority, high);
        false ->
            ok
    end.

%% @doc Send message with priority (normal priority on OTP <28)
-spec send_priority(pid() | atom(), term()) -> ok.
send_priority(Dest, Msg) ->
    case have_priority_messages() of
        true ->
            erlang:send(Dest, Msg, [nosuspend, {priority, high}]);
        false ->
            erlang:send(Dest, Msg, [nosuspend])
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Parse OTP version string
%% Examples:
%%   "27" -> {27, 0, 0}
%%   "28.3.1" -> {28, 3, 1}
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
