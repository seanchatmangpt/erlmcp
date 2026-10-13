%% @doc Court: refusal taxonomy integer disjointness (fail-closed).
%%
%% Guards against the class of defect where erlmcp.hrl's ERR_REFUSED_*
%% band and erlmcp_refusal.hrl's REFUSAL_* band collide by integer under
%% different semantics. Reads the real headers from disk (Chicago-style:
%% real collaborators, no doubles) and asserts:
%%
%% 1. Every ERR_REFUSED_* integer is disjoint from every REFUSAL_*
%%    integer.
%% 2. The core and transports copies of erlmcp_refusal.hrl are
%%    byte-identical.
%% 3. include/erlmcp.hrl and apps/erlmcp_core/include/erlmcp.hrl are the
%%    same file (symlink) / byte-identical content.
%%
%% A future macro added inside a colliding band fails this court at the
%% integer level, independent of macro names.
-module(erlmcp_refusal_disjoint_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%%--------------------------------------------------------------------
%% Repo-root and header paths
%%--------------------------------------------------------------------

root() ->
    case root_from(cwd_candidates()) of
        undefined -> erlang:error({repo_root_not_found, file:get_cwd()});
        R -> R
    end.

cwd_candidates() ->
    case file:get_cwd() of
        {ok, Cwd} -> [Cwd, filename:absname(".")];
        error -> [filename:absname(".")]
    end.

root_from([]) -> undefined;
root_from([P | Rest]) ->
    Abs = filename:absname(P),
    case filelib:is_regular(filename:join(Abs, "rebar.config")) of
        true -> Abs;
        false ->
            case filename:dirname(Abs) of
                Abs -> root_from(Rest);
                Parent -> root_from([Parent | Rest])
            end
    end.

h(Rel) -> filename:join(root(), Rel).

-define(HRL_MAIN, "include/erlmcp.hrl").
-define(HRL_CORE_LINK, "apps/erlmcp_core/include/erlmcp.hrl").
-define(HRL_REFUSAL_CORE, "apps/erlmcp_core/include/erlmcp_refusal.hrl").
-define(HRL_REFUSAL_TRANSPORTS, "apps/erlmcp_transports/include/erlmcp_refusal.hrl").

%%--------------------------------------------------------------------
%% Macro extraction: parse real header bytes
%%--------------------------------------------------------------------

read(Rel) ->
    Path = h(Rel),
    {ok, Bin} = file:read_file(Path),
    Bin.

%% Extract {MacroName, Int} pairs from "-define(NAME, 123)." lines.
extract_codes(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    [ {binary_to_list(MName), Int}
      || Line <- Lines,
         {MName, Int} <- [parse_define(Line)],
         MName =/= <<>> ].

parse_define(Line) ->
    case re:run(Line, <<"^-define\\(([A-Z0-9_]+), *(-?[0-9]+)\\)\\.">>,
                [{capture, [1, 2], binary}]) of
        {match, [MName, IntBin]} ->
            {MName, binary_to_integer(IntBin)};
        nomatch -> {<<>>, 0}
    end.

err_refused_codes() ->
    Bin = read(?HRL_MAIN),
    [ P || {N, _} = P <- extract_codes(Bin),
           lists:prefix("ERR_REFUSED", N) ].

refusal_codes() ->
    Bin = read(?HRL_REFUSAL_CORE),
    [ P || {N, _} = P <- extract_codes(Bin),
           lists:prefix("REFUSAL_", N) orelse lists:prefix("ERR_REFUSED", N) ]
        ++ [ P || {N, _} = P <- extract_codes(read(?HRL_REFUSAL_TRANSPORTS)),
                  lists:prefix("REFUSAL_", N) ].

%%--------------------------------------------------------------------
%% Courts
%%--------------------------------------------------------------------

%% Every ERR_REFUSED_* integer must be absent from the REFUSAL_* set.
disjoint_integers_test_() ->
    {setup, fun root/0, fun(_) -> ok end,
     fun(_) ->
        ErrCodes = err_refused_codes(),
        RefusalInts = [I || {_, I} <- refusal_codes()],
        [ {N, ?_assertNot(lists:member(I, RefusalInts))}
          || {N, I} <- ErrCodes ]
     end}.

%% Aggregate collision report with a readable failure message.
band_disjoint_aggregate_test() ->
    _ = root(),
    ErrCodes = err_refused_codes(),
    RefusalInts = [I || {_, I} <- refusal_codes()],
    Collisions = [ {N, I}
                   || {N, I} <- ErrCodes, lists:member(I, RefusalInts) ],
    ?assertEqual([], Collisions,
                 {colliding_codes, Collisions,
                  msg, "ERR_REFUSED_* integers collide with REFUSAL_* in "
                       "erlmcp_refusal.hrl - renumber before use"}).

%% Mirror-header byte consistency: two REFUSAL copies must be identical.
mirror_byte_consistency_test() ->
    _ = root(),
    CoreBin = read(?HRL_REFUSAL_CORE),
    TransportsBin = read(?HRL_REFUSAL_TRANSPORTS),
    ?assertEqual(byte_size(CoreBin), byte_size(TransportsBin)),
    ?assertEqual(CoreBin, TransportsBin).

%% The apps/erlmcp_core erlmcp.hrl must resolve to include/erlmcp.hrl
%% (symlink) so edits to one cannot silently diverge from the other.
core_header_is_same_file_test() ->
    Main = h(?HRL_MAIN),
    Link = h(?HRL_CORE_LINK),
    {ok, #file_info{type = symlink}} = file:read_link_info(Link),
    {ok, Target} = file:read_link(Link),
    Resolved = filename:join(filename:dirname(Link), Target),
    {ok, #file_info{inode = InodeMain}} = file:read_link_info(Main),
    {ok, #file_info{inode = InodeLink}} = file:read_link_info(Resolved),
    ?assertEqual(InodeMain, InodeLink).

%% ERR_REFUSED band sanity: MIN/MAX bracket the defined codes, no
%% duplicates inside the band.
band_internal_consistency_test_() ->
    {setup, fun root/0, fun(_) -> ok end,
     fun(_) ->
        Codes = err_refused_codes(),
        {MinDefs, Rest} = lists:partition(
                            fun({N, _}) ->
                                N =:= "ERR_REFUSED_MIN" orelse
                                N =:= "ERR_REFUSED_MAX"
                            end, Codes),
        Ints = [I || {N, I} <- Rest, N =/= "ERR_REFUSED_MIN",
                                        N =/= "ERR_REFUSED_MAX"],
        {_, MinI} = lists:keyfind("ERR_REFUSED_MIN", 1, MinDefs),
        {_, MaxI} = lists:keyfind("ERR_REFUSED_MAX", 1, MinDefs),
        [ ?_assert(I >= MinI andalso I =< MaxI)
          || I <- Ints ]
        ++ [ ?_assertEqual(length(Ints), length(lists:usort(Ints))) ]
     end}.

%% Include-order smoke: a module including both headers compiles with
%% no macro clash (both taxonomies reachable simultaneously).
coinclude_compile_test() ->
    Root = root(),
    Src = "%% auto by erlmcp_refusal_disjoint_tests\n"
          "-module(erlmcp_disjoint_coinc_probe).\n"
          "-include(\"erlmcp.hrl\").\n"
          "-include(\"erlmcp_refusal.hrl\").\n"
          "-export([probe/0]).\n"
          "probe() ->\n"
          "    {?ERR_REFUSED_CONTENT_POLICY, ?REFUSAL_QUEUE_CAP_EXCEEDED}.\n",
    TmpDir = filename:join(Root, "_build/test_court_probe"),
    ok = filelib:ensure_dir(filename:join(TmpDir, "x")),
    SrcPath = filename:join(TmpDir, "erlmcp_disjoint_coinc_probe.erl"),
    ok = file:write_file(SrcPath, Src),
    try
        {ok, erlmcp_disjoint_coinc_probe, Bin} =
            compile:file(SrcPath, [{i, filename:join(Root, "include")},
                                   {i, filename:join(Root, "apps/erlmcp_core/include")},
                                   binary, return_errors]),
        {module, _} = code:load_binary(erlmcp_disjoint_coinc_probe, SrcPath, Bin),
        {A, B} = erlmcp_disjoint_coinc_probe:probe(),
        ?assert(A =/= B),
        ?assertEqual(1001, B),
        ok
    after
        file:del_dir_r(TmpDir)
    end.
