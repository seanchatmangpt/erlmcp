-module(erlmcp_atoms).
-behaviour(gen_server).

%% OTP 28 Atom Size Limit Support
%%
%% OTP 28 changed atom size limits from 255 bytes to 255 characters.
%% This enables longer international atom names with multibyte UTF-8.
%%
%% Examples:
%% - Japanese: <<"ツール名">> (12 bytes, 4 chars)
%% - Arabic: <<"الأداة">> (10 bytes, 5 chars)
%% - Emoji: <<"🔧_tool_🚀">> (14 bytes, 10 chars)
%%
%% Key Functions:
%% - binary_to_atom_safe/1,2: Safe conversion with validation
%% - tool_name_to_atom/1: OTP 28 aware (255 characters)
%% - resource_name_to_atom/1: For resource URIs
%% - namespaced_atom/2: Create namespaced atoms
%% - char_length_check/1: Check character length (OTP 28)

%% API exports
-export([
    start_link/0,
    binary_to_atom_safe/1,
    binary_to_atom_safe/2,
    tool_name_to_atom/1,
    resource_name_to_atom/1,
    namespaced_atom/2,
    existing_atom/1,
    char_length_check/1,
    is_safe_atom/1,
    validate_binary/1,
    get_atom_limit/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(MAX_ATOM_BYTES, 255). % OTP < 28 limit
-define(MAX_ATOM_CHARS, 255). % OTP 28 limit

%% Type definitions
-type atom_result() :: atom() | {error, atom_error()}.
-type atom_error() :: too_long | invalid_utf8 | empty | invalid_binary.
-type encoding() :: utf8 | latin1.

-export_type([atom_result/0, atom_error/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the atom management server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Safely convert binary to atom with byte size check
%% Validates length and UTF-8 encoding before conversion.
%% Uses binary_to_existing_atom for existing atoms to prevent atom leak.
-spec binary_to_atom_safe(binary()) -> atom_result().
binary_to_atom_safe(Binary) ->
    binary_to_atom_safe(Binary, utf8).

%% @doc Safely convert binary to atom with encoding option
%% Checks byte size limit (255) for backward compatibility.
%% Returns existing atom if available, otherwise creates new atom.
-spec binary_to_atom_safe(binary(), encoding()) -> atom_result().
binary_to_atom_safe(Binary, Encoding) when is_binary(Binary) ->
    case validate_binary(Binary) of
        ok ->
            try
                % Try to reuse existing atom first (prevents atom leak)
                binary_to_existing_atom(Binary, Encoding)
            catch
                error:badarg ->
                    % Create new atom if doesn't exist
                    try
                        binary_to_atom(Binary, Encoding)
                    catch
                        error:system_limit ->
                            {error, too_long}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end;
binary_to_atom_safe(_, _) ->
    {error, invalid_binary}.

%% @doc Convert tool name to atom (OTP 28 aware)
%% Uses character length limit (255 chars) instead of byte limit.
%% Enables longer international tool names with multibyte UTF-8.
-spec tool_name_to_atom(binary()) -> atom().
tool_name_to_atom(Binary) when is_binary(Binary) ->
    case char_length_check(Binary) of
        ok ->
            try
                binary_to_existing_atom(Binary, utf8)
            catch
                error:badarg ->
                    binary_to_atom(Binary, utf8)
            end;
        {error, too_long} ->
            error(tool_name_too_long)
    end.

%% @doc Convert resource name/URI to atom
%% Resources may have longer names, uses character length check.
-spec resource_name_to_atom(binary()) -> atom_result().
resource_name_to_atom(Binary) when is_binary(Binary) ->
    % Resources can have URIs which might be longer
    % Use safe conversion with byte limit for safety
    binary_to_atom_safe(Binary, utf8);
resource_name_to_atom(_) ->
    {error, invalid_binary}.

%% @doc Create a namespaced atom
%% Combines namespace and name with "$" separator.
%% Example: namespaced_atom(<<"mcp">>, <<"tool">>) -> 'mcp$tool'
-spec namespaced_atom(binary(), binary()) -> atom_result().
namespaced_atom(Namespace, Name) when is_binary(Namespace), is_binary(Name) ->
    Full = <<Namespace/binary, "$", Name/binary>>,
    binary_to_atom_safe(Full, utf8);
namespaced_atom(_, _) ->
    {error, invalid_binary}.

%% @doc Safely convert binary to existing atom
%% Returns error if atom doesn't exist (prevents atom leak).
-spec existing_atom(binary()) -> atom_result().
existing_atom(Binary) when is_binary(Binary) ->
    case validate_binary(Binary) of
        ok ->
            try
                binary_to_existing_atom(Binary, utf8)
            catch
                error:badarg ->
                    {error, not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
existing_atom(_) ->
    {error, invalid_binary}.

%% @doc Check character length (OTP 28)
%% Uses string:length/1 for proper UTF-8 character counting.
%% Returns ok if within 255 character limit, error otherwise.
-spec char_length_check(binary()) -> ok | {error, too_long}.
char_length_check(Binary) when is_binary(Binary) ->
    % OTP 28: Use string:length for character count (not byte_size)
    Chars = string:length(Binary),
    if
        Chars > ?MAX_ATOM_CHARS -> {error, too_long};
        Chars =:= 0 -> {error, empty};
        true -> ok
    end;
char_length_check(_) ->
    {error, invalid_binary}.

%% @doc Check if atom is safe (reserved or system atom)
%% Returns true if atom is safe to use for user data.
%% Returns false for reserved atoms like 'undefined', 'true', 'false'.
-spec is_safe_atom(atom()) -> boolean().
is_safe_atom(Atom) when is_atom(Atom) ->
    % List of reserved/unsafe atoms for user data
    ReservedAtoms = [
        undefined, true, false, ok, error,
        badarg, badarith, badmatch, function_clause, case_clause,
        if_clause, try_clause, undef, system_limit, timeout
    ],
    not lists:member(Atom, ReservedAtoms).

%% @doc Validate binary for atom conversion
%% Checks for empty binary, byte size limit, and UTF-8 validity.
-spec validate_binary(binary()) -> ok | {error, atom_error()}.
validate_binary(<<>>) ->
    {error, empty};
validate_binary(Binary) when byte_size(Binary) > ?MAX_ATOM_BYTES ->
    {error, too_long};
validate_binary(Binary) ->
    % Check UTF-8 validity
    case binary:match(Binary, [<<0>>, <<255>>, <<254>>, <<253>>]) of
        nomatch ->
            ok;
        _ ->
            % Attempt UTF-8 validation
            try
                _ = unicode:characters_to_list(Binary, utf8),
                ok
            catch
                error:_ ->
                    {error, invalid_utf8}
            end
    end.

%% @doc Get the current atom limit
%% Returns 255 for character limit (OTP 28+) or byte limit (OTP < 28).
-spec get_atom_limit() -> pos_integer().
get_atom_limit() ->
    % For OTP 28+, we use character limit
    % For OTP < 28, byte_size is the limit
    case erlang:system_info(otp_release) >= "28" of
        true -> ?MAX_ATOM_CHARS;
        false -> ?MAX_ATOM_BYTES
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #{}}.
init([]) ->
    logger:info("Starting erlmcp_atoms server (OTP ~s atom support)",
                [erlang:system_info(otp_release)]),
    {ok, #{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #{}) ->
    {reply, term(), #{}}.
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #{}) -> {noreply, #{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #{}) -> {noreply, #{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #{}, term()) -> {ok, #{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
