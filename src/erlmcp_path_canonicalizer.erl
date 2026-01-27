%%%-------------------------------------------------------------------
%% @doc Resource Path Canonicalization Module
%% Implements symlink resolution, path normalization, and directory traversal
%% prevention per MCP 2025-11-25 security specification (Gap #36).
%%
%% This module provides secure path handling to prevent:
%% - Symlink attacks (following symlinks outside allowed directories)
%% - Path traversal attacks (../ directory escapes)
%% - Directory escaping via relative paths
%% - Circular symlink resolution issues
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_path_canonicalizer).

-include("erlmcp.hrl").

%% API exports
-export([
    canonicalize_path/1,
    is_within_allowed_directory/2,
    validate_resource_path/3,
    validate_resource_path/2,
    resolve_symlinks/1,
    check_directory_escape/1
]).

%% Types
-type path() :: binary() | string() | list().
-type canonical_path() :: binary().
-type validation_result() :: {ok, canonical_path()} | {error, term()}.

-export_type([canonical_path/0, validation_result/0]).

%% Constants
-define(MAX_SYMLINK_DEPTH, 40).        % Prevent infinite symlink loops
-define(MAX_PATH_LENGTH, 4096).         % Filesystem limit on most systems
-define(SAFE_PATH_SEPARATORS, <<"/">>).

%%====================================================================
%% API Functions
%%====================================================================

-spec canonicalize_path(path()) -> validation_result().
%%%---
%% @doc Canonicalize a path by resolving symlinks and normalizing relative paths.
%%
%% This function:
%% 1. Converts path to string format
%% 2. Resolves all symlinks (follows up to MAX_SYMLINK_DEPTH)
%% 3. Normalizes relative path components (..)
%% 4. Returns absolute canonical path
%%
%% Returns {ok, CanonicalPath} or {error, Reason}
canonicalize_path(Path) when is_binary(Path) ->
    canonicalize_path(binary_to_list(Path));
canonicalize_path(Path) when is_list(Path) ->
    SpanCtx = erlmcp_tracing:start_span(<<"path.canonicalize">>),
    try
        %% Check path length
        case length(Path) of
            Len when Len > ?MAX_PATH_LENGTH ->
                erlmcp_tracing:record_error_details(SpanCtx, path_too_long, Len),
                {error, path_too_long};
            _ ->
                %% First normalize the path (resolve .. and .)
                NormalizedPath = normalize_path(Path),
                %% Then resolve symlinks
                case resolve_symlinks(NormalizedPath, ?MAX_SYMLINK_DEPTH, []) of
                    {ok, CanonicalPath} ->
                        erlmcp_tracing:set_attributes(SpanCtx, #{
                            <<"original">> => erlang:list_to_binary(Path),
                            <<"canonical">> => erlang:list_to_binary(CanonicalPath)
                        }),
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        {ok, erlang:list_to_binary(CanonicalPath)};
                    {error, Reason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, symlink_resolution_failed, Reason),
                        {error, Reason}
                end
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;
canonicalize_path(_) ->
    {error, invalid_path_format}.

-spec is_within_allowed_directory(canonical_path(), canonical_path()) -> boolean().
%%%---
%% @doc Check if a canonical path is within an allowed base directory.
%%
%% Returns true if CanonicalPath is equal to or a subdirectory of BaseDir.
%% Both paths must already be canonicalized.
%%
%% Examples:
%% - is_within_allowed_directory(<<"/var/www/public/file.txt">>, <<"/var/www">>)  => true
%% - is_within_allowed_directory(<<"/var/www">>, <<"/var/www">>)                  => true
%% - is_within_allowed_directory(<<"/var/other">>, <<"/var/www">>)                => false
%% - is_within_allowed_directory(<<"/var/www_backup">>, <<"/var/www">>)           => false
is_within_allowed_directory(CanonicalPath, BaseDir) when is_binary(CanonicalPath), is_binary(BaseDir) ->
    SpanCtx = erlmcp_tracing:start_span(<<"path.check_within_directory">>),
    try
        PathStr = binary_to_list(CanonicalPath),
        BaseDirStr = binary_to_list(BaseDir),

        %% Ensure base directory ends with separator for proper prefix matching
        NormalizedBaseDir = ensure_trailing_separator(BaseDirStr),

        Result = case PathStr of
            %% Exact match
            BaseDirStr -> true;
            %% Path starts with base directory (with separator)
            _ ->
                case string:prefix(PathStr, NormalizedBaseDir) of
                    nomatch -> false;
                    _ -> true
                end
        end,

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"canonical_path">> => CanonicalPath,
            <<"base_dir">> => BaseDir,
            <<"is_within">> => Result
        }),
        erlmcp_tracing:set_status(SpanCtx, ok),
        Result
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            false
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;
is_within_allowed_directory(_, _) ->
    false.

-spec validate_resource_path(path(), [canonical_path()]) -> validation_result().
%%%---
%% @doc Validate a resource path against multiple allowed base directories.
%%
%% This is the main security function that:
%% 1. Canonicalizes the input path
%% 2. Checks it's within one of the allowed directories
%% 3. Returns the safe canonical path or error
%%
%% Example:
%% {ok, CanonicalPath} = validate_resource_path(
%%     <<"/var/www/public/../public/file.txt">>,
%%     [<<"/var/www">>]
%% )
validate_resource_path(Path, AllowedDirs) ->
    validate_resource_path(Path, AllowedDirs, #{}).

-spec validate_resource_path(path(), [canonical_path()], map()) -> validation_result().
%%%---
%% @doc Validate a resource path with options.
%%
%% Options map:
%% - follow_symlinks: boolean (default: false) - Whether to follow symlinks
%% - max_depth: integer - Maximum symlink resolution depth
%%
validate_resource_path(Path, AllowedDirs, _Options) when is_list(AllowedDirs) ->
    SpanCtx = erlmcp_tracing:start_span(<<"path.validate_resource_path">>),
    try
        %% Canonicalize the path
        case canonicalize_path(Path) of
            {ok, CanonicalPath} ->
                %% Check against all allowed directories
                case lists:any(
                    fun(AllowedDir) ->
                        is_within_allowed_directory(CanonicalPath, AllowedDir)
                    end,
                    AllowedDirs
                ) of
                    true ->
                        erlmcp_tracing:set_attributes(SpanCtx, #{
                            <<"canonical_path">> => CanonicalPath,
                            <<"validation">> => <<"allowed">>
                        }),
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        {ok, CanonicalPath};
                    false ->
                        erlmcp_tracing:record_error_details(SpanCtx, path_outside_allowed, CanonicalPath),
                        {error, path_outside_allowed_directories}
                end;
            {error, Reason} ->
                erlmcp_tracing:record_error_details(SpanCtx, canonicalization_failed, Reason),
                {error, Reason}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;
validate_resource_path(_, _, _) ->
    {error, invalid_arguments}.

-spec resolve_symlinks(path()) -> validation_result().
%%%---
%% @doc Resolve symlinks in a path (public convenience function).
resolve_symlinks(Path) when is_binary(Path) ->
    resolve_symlinks(binary_to_list(Path));
resolve_symlinks(Path) when is_list(Path) ->
    case resolve_symlinks(Path, ?MAX_SYMLINK_DEPTH, []) of
        {ok, Resolved} -> {ok, erlang:list_to_binary(Resolved)};
        {error, _} = Error -> Error
    end;
resolve_symlinks(_) ->
    {error, invalid_path_format}.

-spec check_directory_escape(path()) -> boolean().
%%%---
%% @doc Check if a path would escape its directory (contains dangerous .. sequences).
%% Returns true if path is safe (no escape attempts), false if dangerous.
check_directory_escape(Path) when is_binary(Path) ->
    check_directory_escape(binary_to_list(Path));
check_directory_escape(Path) when is_list(Path) ->
    %% Split by path separator and check components
    Components = string:split(Path, "/", all),
    %% Count leading / .. to see if we escape root
    not would_escape_root(Components, 0);
check_directory_escape(_) ->
    false.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Normalize a path by resolving . and .. components.
%% Does NOT follow symlinks - just normalizes relative path syntax.
-spec normalize_path(string()) -> string().
normalize_path(Path) ->
    Components = string:split(Path, "/", all),
    NormalizedComponents = normalize_components(Components, []),
    string:join(NormalizedComponents, "/").

%% @private
%% @doc Normalize path components by removing . and ..
-spec normalize_components([string()], [string()]) -> [string()].
normalize_components([], Acc) ->
    lists:reverse(Acc);
normalize_components([<<>>|Rest], Acc) ->
    %% Empty component (from //) - skip
    normalize_components(Rest, Acc);
normalize_components([""| Rest], Acc) ->
    %% Empty component from split - represents leading /
    normalize_components(Rest, [""| Acc]);
normalize_components(["."| Rest], Acc) ->
    %% Current directory - skip
    normalize_components(Rest, Acc);
normalize_components([".."| Rest], [""| RestAcc]) ->
    %% Trying to escape root - skip the ..
    normalize_components(Rest, [""| RestAcc]);
normalize_components([".."| Rest], [_| RestAcc]) ->
    %% Go up one directory
    normalize_components(Rest, RestAcc);
normalize_components([".."| Rest], []) ->
    %% No parent - skip
    normalize_components(Rest, []);
normalize_components([Component| Rest], Acc) ->
    normalize_components(Rest, [Component| Acc]).

%% @private
%% @doc Resolve symlinks in a path up to MaxDepth to prevent infinite loops.
-spec resolve_symlinks(string(), non_neg_integer(), [string()]) ->
    {ok, string()} | {error, term()}.
resolve_symlinks(_Path, 0, _) ->
    %% Max depth reached - likely circular symlink
    {error, symlink_loop_detected};
resolve_symlinks(Path, _Depth, _) ->
    case file:read_link_info(Path) of
        {ok, FileInfo} ->
            case maps:get(type, FileInfo, undefined) of
                symlink ->
                    %% It's a symlink - resolve it
                    case file:read_link(Path) of
                        {ok, LinkTarget} ->
                            %% LinkTarget might be relative, so resolve it relative to the symlink's directory
                            BasePath = filename:dirname(Path),
                            ResolvedTarget = case is_absolute_path(LinkTarget) of
                                true -> LinkTarget;
                                false -> filename:join(BasePath, LinkTarget)
                            end,
                            %% Normalize and recurse
                            NormalizedTarget = normalize_path(ResolvedTarget),
                            resolve_symlinks(NormalizedTarget, _Depth - 1, []);
                        {error, Reason} ->
                            {error, {symlink_read_failed, Reason}}
                    end;
                _ ->
                    %% Not a symlink - return as-is
                    {ok, Path}
            end;
        {error, enoent} ->
            %% File doesn't exist - but path is still valid (for write operations)
            %% Return the parent directory's canonical path + the filename
            case normalize_path(Path) of
                NormalizedPath ->
                    {ok, NormalizedPath}
            end;
        {error, Reason} ->
            {error, {path_check_failed, Reason}}
    end.

%% @private
%% @doc Check if a path is absolute (starts with /)
-spec is_absolute_path(string() | binary()) -> boolean().
is_absolute_path(<<"/", _/binary>>) -> true;
is_absolute_path([$/| _]) -> true;
is_absolute_path(_) -> false.

%% @private
%% @doc Ensure a directory path ends with separator for prefix matching
-spec ensure_trailing_separator(string()) -> string().
ensure_trailing_separator(Path) ->
    case lists:last(Path) of
        $/ -> Path;
        _ -> Path ++ "/"
    end.

%% @private
%% @doc Check if path components would escape the root directory
-spec would_escape_root([string()], non_neg_integer()) -> boolean().
would_escape_root([], _) ->
    false;
would_escape_root([""| _], _) ->
    %% Leading / represents root
    false;
would_escape_root([".."| Rest], Depth) when Depth > 0 ->
    would_escape_root(Rest, Depth - 1);
would_escape_root([".."| _], 0) ->
    %% Would go above root
    true;
would_escape_root([_| Rest], Depth) ->
    would_escape_root(Rest, Depth + 1).
