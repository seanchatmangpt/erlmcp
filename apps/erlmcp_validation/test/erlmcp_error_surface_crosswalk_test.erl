%% @doc erlmcp-side mirror of the marketplace refusal-crosswalk's 51-code
%% erlmcp column. Pinned table derived from a real grep of this tree at
%% write time (branch otp27/ssl-followups):
%%
%%   grep -oE 'REFUSAL_[A-Z_]+, *[0-9]+' \
%%     apps/erlmcp_core/include/erlmcp_refusal.hrl
%%
%% Courts:
%%   (a) every pinned entry's definition site still exists and still
%%       defines that macro at that code (fail-closed drift detection),
%%       and the transports mirror header is byte-identical;
%%   (b) sweep court -- the source tree is re-greped at test time via
%%       file:read_file over apps/*/src and apps/*/include; any
%%       REFUSAL-shaped numeric code definition outside the pinned table
%%       fails with the offending code/macros listed (new codes must be
%%       added to the crosswalk or this court fails).
-module(erlmcp_error_surface_crosswalk_test).

-include_lib("eunit/include/eunit.hrl").

%% Canonical header (definition site of record).
-define(CANONICAL_HRL, "apps/erlmcp_core/include/erlmcp_refusal.hrl").
%% Byte-identical mirror copy shipped with transports.
-define(MIRROR_HRL, "apps/erlmcp_transports/include/erlmcp_refusal.hrl").

%% Pinned table: {Code, MacroName, DefinitionSiteRelPath}
%% 51 entries -- exactly the codes the marketplace crosswalk claims.
pinned_table() ->
    H = ?CANONICAL_HRL,
    [%% Queue and Backpressure (1001-1005)
     {1001, "REFUSAL_QUEUE_CAP_EXCEEDED", H},
     {1002, "REFUSAL_QUEUE_BYTE_CAP_EXCEEDED", H},
     {1003, "REFUSAL_QUEUE_TENANT_CAP_EXCEEDED", H},
     {1004, "REFUSAL_BUFFER_OVERFLOW", H},
     {1005, "REFUSAL_BACKPRESSURE_ACTIVE", H},
     %% Authentication & Authorization (1011-1016)
     {1011, "REFUSAL_AUTH_FAILED", H},
     {1012, "REFUSAL_AUTH_EXPIRED", H},
     {1013, "REFUSAL_AUTH_INVALID_CREDENTIALS", H},
     {1014, "REFUSAL_AUTHZ_FORBIDDEN", H},
     {1015, "REFUSAL_AUTH_MISSING", H},
     {1016, "REFUSAL_SESSION_INVALID", H},
     %% Validation (1021-1029)
     {1021, "REFUSAL_INVALID_PARAMS", H},
     {1022, "REFUSAL_INVALID_JSON_SCHEMA", H},
     {1023, "REFUSAL_INVALID_URI", H},
     {1024, "REFUSAL_INVALID_CONTENT_TYPE", H},
     {1025, "REFUSAL_INVALID_HEADER", H},
     {1026, "REFUSAL_INVALID_SESSION_ID", H},
     {1027, "REFUSAL_INVALID_PROTOCOL_VERSION", H},
     {1028, "REFUSAL_MISSING_REQUIRED_FIELD", H},
     {1029, "REFUSAL_FIELD_TYPE_MISMATCH", H},
     %% Path security (1036-1040)
     {1036, "REFUSAL_PATH_TRAVERSAL_DETECTED", H},
     {1037, "REFUSAL_PATH_INVALID", H},
     {1038, "REFUSAL_SYMLINK_TRAVERSAL_DETECTED", H},
     {1039, "REFUSAL_URI_OUT_OF_BOUNDS", H},
     {1040, "REFUSAL_CANONICAL_PATH_VIOLATION", H},
     %% Entity existence (1046-1052)
     {1046, "REFUSAL_RESOURCE_NOT_FOUND", H},
     {1047, "REFUSAL_RESOURCE_DUPLICATE", H},
     {1048, "REFUSAL_TOOL_NOT_FOUND", H},
     {1049, "REFUSAL_TOOL_DUPLICATE", H},
     {1050, "REFUSAL_PROMPT_NOT_FOUND", H},
     {1051, "REFUSAL_PROMPT_DUPLICATE", H},
     {1052, "REFUSAL_ENTITY_DUPLICATE", H},
     %% Rate limiting (1056-1060)
     {1056, "REFUSAL_RATE_LIMIT_EXCEEDED", H},
     {1057, "REFUSAL_RATE_LIMIT_PER_SECOND", H},
     {1058, "REFUSAL_RATE_LIMIT_PER_MINUTE", H},
     {1059, "REFUSAL_QUOTA_EXCEEDED", H},
     {1060, "REFUSAL_CONCURRENT_LIMIT_EXCEEDED", H},
     %% Protocol/transport (1066-1070)
     {1066, "REFUSAL_PROTOCOL_ERROR", H},
     {1067, "REFUSAL_TRANSPORT_ERROR", H},
     {1068, "REFUSAL_MESSAGE_TOO_LARGE", H},
     {1069, "REFUSAL_TIMEOUT", H},
     {1070, "REFUSAL_UNSUPPORTED_ENCODING", H},
     %% Server lifecycle (1076-1080)
     {1076, "REFUSAL_SERVER_UNINITIALIZED", H},
     {1077, "REFUSAL_SERVER_SHUTTING_DOWN", H},
     {1078, "REFUSAL_SERVICE_UNAVAILABLE", H},
     {1079, "REFUSAL_INTERNAL_ERROR", H},
     {1080, "REFUSAL_DEPENDENCY_UNAVAILABLE", H},
     %% Health/degradation (1086-1089)
     {1086, "REFUSAL_CIRCUIT_BREAKER_OPEN", H},
     {1087, "REFUSAL_HEALTH_CHECK_FAILED", H},
     {1088, "REFUSAL_DEGRADED_SERVICE", H},
     {1089, "REFUSAL_RESOURCE_EXHAUSTED", H}].

pinned_codes() ->
    [C || {C, _M, _S} <- pinned_table()].

%%--------------------------------------------------------------------
%% Repo root resolution: walk up from this module's beam location until
%% the canonical header exists.
%%--------------------------------------------------------------------
repo_root() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    {ok, Cwd} = file:get_cwd(),
    Roots = [BeamDir, Cwd, filename:absname("")],
    case [R || R <- Roots, filelib:is_regular(filename:join(R, ?CANONICAL_HRL))] of
        [Root | _] -> Root;
        [] -> erlang:error({repo_root_not_found, Roots})
    end.

%%--------------------------------------------------------------------
%% Court (a): pinned entries -- definition sites exist, macro at code,
%% mirror header byte-identical.
%%--------------------------------------------------------------------
pinned_entries_still_defined_test_() ->
    {timeout, 60,
     fun() ->
             Root = repo_root(),
             CanonPath = filename:join(Root, ?CANONICAL_HRL),
             {ok, CanonBin} = file:read_file(CanonPath),
             %% Every pinned entry points at the canonical header.
             Sites = lists:usort([Site || {_C, _M, Site} <- pinned_table()]),
             ?assertEqual([?CANONICAL_HRL], Sites),
             %% Mirror copy is byte-identical.
             ?assertEqual({ok, CanonBin},
                          file:read_file(filename:join(Root, ?MIRROR_HRL))),
             Defined = scan_defines(CanonBin),
             ?assertEqual(51, length(Defined)),
             Missing =
                 [Entry || Entry = {C, M, _S} <- pinned_table(),
                           not lists:member({C, M}, Defined)],
             ?assertEqual([], Missing)
     end}.

%%--------------------------------------------------------------------
%% Sweep court (b): no REFUSAL-shaped numeric code definition anywhere
%% under apps/*/src or apps/*/include outside the pinned table.
%%--------------------------------------------------------------------
sweep_court_test_() ->
    {timeout, 120,
     fun() ->
             Root = repo_root(),
             Pinned = lists:usort([{C, M} || {C, M, _S} <- pinned_table()]),
             ?assertEqual(51, length(Pinned)),
             Sources = source_files(Root),
             ?assert(length(Sources) > 100),
             Found = lists:append(
                       [begin
                            {ok, B} = file:read_file(F),
                            scan_defines(B)
                        end || F <- Sources]),
             Extras = lists:usort(Found) -- Pinned,
             case Extras of
                 [] -> ok;
                 Unknown ->
                     erlang:error(
                       io_lib:format("REFUSAL code definitions in the tree "
                                     "absent from the pinned crosswalk table "
                                     "(add to crosswalk or fix source): ~p",
                                     [Unknown]))
             end,
             %% Sweep found every pinned entry too (table covers the surface).
             ?assertEqual([], lists:usort(Pinned) -- lists:usort(Found))
     end}.

%%--------------------------------------------------------------------
%% Surface size: exactly 51 unique codes, 51 unique macros, bijective.
%%--------------------------------------------------------------------
surface_size_is_51_test_() ->
    fun() ->
             Codes = pinned_codes(),
             Macros = [M || {_C, M, _S} <- pinned_table()],
             ?assertEqual(51, length(Codes)),
             ?assertEqual(51, length(lists:usort(Codes))),
             ?assertEqual(51, length(lists:usort(Macros)))
     end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
source_files(Root) ->
    AppsDir = filename:join(Root, "apps"),
    {ok, Apps} = file:list_dir(AppsDir),
    lists:append(
      [begin
           Dir = filename:join(AppsDir, App),
           case filelib:is_dir(Dir) of
               true ->
                   lists:append([source_files_in(filename:join(Dir, Sub))
                                 || Sub <- ["src", "include"]]);
               false ->
                   []
           end
       end || App <- lists:sort(Apps)]).

source_files_in(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            [filename:join(Dir, F)
             || F <- Files,
                filename:extension(F) =:= ".erl"
                orelse filename:extension(F) =:= ".hrl"];
        {error, _} ->
            []
    end.

%% Extract {Code, Macro} pairs from `-define(REFUSAL_X, 10NN).` shapes.
scan_defines(Bin) ->
    {ok, MP} =
        re:compile("-define\\s*\\(\\s*(REFUSAL_[A-Z0-9_]+)\\s*,\\s*(-?10[0-9]{2})\\s*\\)"),
    case re:run(Bin, MP, [global, {capture, [2, 1], list}]) of
        {match, Captures} ->
            [{list_to_integer(C), M} || [C, M] <- Captures];
        nomatch ->
            [];
        {error, Err} ->
            erlang:error({regex_error, Err})
    end.
