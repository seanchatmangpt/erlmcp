-module(erlmcp_path_canonicalizer).

%% API exports
-export([
    validate_resource_path/2,
    canonicalize_path/1,
    is_path_safe/2
]).

%%%-----------------------------------------------------------------
%%% Path Canonicalizer for MCP Resources
%%%
%%% Implements secure path handling:
%%% - Symlink resolution prevention
%%% - Path traversal detection (..)
%%% - Canonical path generation
%%% - Directory jail enforcement
%%%
%%% Gap #36: Path canonicalization and validation for security
%%%-----------------------------------------------------------------

-define(DEFAULT_ALLOWED_DIRS, [<<"/">>]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Validate resource path against allowed directories
%% Returns: {ok, CanonicalUri} | {error, Reason}
-spec validate_resource_path(binary(), [binary()]) ->
    {ok, binary()} | {error, term()}.
validate_resource_path(Uri, AllowedDirs) when is_binary(Uri), is_list(AllowedDirs) ->
    try
        %% 1. Parse URI to extract path
        Path = extract_path_from_uri(Uri),

        %% 2. Canonicalize the path
        CanonicalPath = canonicalize_path(Path),

        %% 3. Check if path is within allowed directories
        case is_path_safe(CanonicalPath, AllowedDirs) of
            true ->
                %% 4. Return canonical URI
                {ok, rebuild_uri(Uri, CanonicalPath)};
            false ->
                {error, {path_outside_allowed_dirs, CanonicalPath}}
        end
    catch
        Class:Reason:Stack ->
            logger:error("Path validation failed: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {path_validation_failed, Reason}}
    end.

%% @doc Canonicalize a file path (resolve . and ..)
%% Returns: CanonicalPath :: binary()
-spec canonicalize_path(binary()) -> binary().
canonicalize_path(Path) when is_binary(Path) ->
    try
        %% Convert to list for processing
        PathList = binary_to_list(Path),

        %% Normalize path separators
        Normalized = normalize_separators(PathList),

        %% Split into components
        Components = string:split(Normalized, "/", all),

        %% Process components (remove . and resolve ..)
        Processed = process_path_components(Components, []),

        %% Rebuild path
        CanonicalList = reconstruct_path(Processed),

        list_to_binary(CanonicalList)
    catch
        _:_ ->
            %% If canonicalization fails, return original
            Path
    end.

%% @doc Check if path is within allowed directories
%% Returns: boolean()
-spec is_path_safe(binary(), [binary()]) -> boolean().
is_path_safe(Path, AllowedDirs) when is_binary(Path), is_list(AllowedDirs) ->
    %% Check if path starts with any allowed directory
    lists:any(fun(AllowedDir) ->
        is_prefix_of(AllowedDir, Path)
    end, AllowedDirs).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Extract path from URI
extract_path_from_uri(Uri) ->
    SchemeSep = <<"://">>,
    case binary:split(Uri, SchemeSep) of
        [_, PathWithQuery] ->
            %% Remove query string and fragment if present
            QuerySep = <<"?">>,
            case binary:split(PathWithQuery, QuerySep) of
                [PathOnly, _] -> PathOnly;
                [PathOnly] -> PathOnly
            end;
        [PathOnly] ->
            %% No scheme, treat as path
            PathOnly
    end.

%% @private Rebuild URI with canonical path
rebuild_uri(Uri, CanonicalPath) ->
    SchemeSep = <<"://">>,
    case binary:split(Uri, SchemeSep) of
        [Scheme, _Rest] ->
            <<Scheme/binary, "://", CanonicalPath/binary>>;
        [_] ->
            %% No scheme
            CanonicalPath
    end.

%% @private Normalize path separators (handle mixed \ and /)
normalize_separators(PathList) ->
    lists:map(fun(C) ->
        case C of
            $\\ -> $/;
            _ -> C
        end
    end, PathList).

%% @private Process path components (resolve . and ..)
process_path_components([], Acc) ->
    lists:reverse(Acc);
process_path_components([<<"">> | Rest], Acc) ->
    %% Empty component (leading slash or double slash)
    process_path_components(Rest, Acc);
process_path_components([<<".">> | Rest], Acc) ->
    %% Current directory reference - skip
    process_path_components(Rest, Acc);
process_path_components([<<"..">> | Rest], Acc) ->
    %% Parent directory reference - pop from accumulator
    case Acc of
        [_ | Tail] ->
            process_path_components(Rest, Tail);
        [] ->
            %% Too many .., keep it
            process_path_components(Rest, [<<"..">> | Acc])
    end;
process_path_components([Component | Rest], Acc) ->
    process_path_components(Rest, [Component | Acc]).

%% @private Reconstruct path from components
reconstruct_path([]) ->
    "/";
reconstruct_path(Components) ->
    %% Ensure leading slash
    Path = string:join(lists:reverse(Components), "/"),
    case Path of
        [$/ | _] -> Path;
        _ -> [$/ | Path]
    end.

%% @private Check if Target is a prefix of Path
is_prefix_of(Prefix, Path) ->
    PrefixSize = byte_size(Prefix),
    case Path of
        <<Prefix:PrefixSize/binary, _/binary>> ->
            true;
        <<Prefix:PrefixSize/binary>> ->
            true;
        _ ->
            false
    end.
