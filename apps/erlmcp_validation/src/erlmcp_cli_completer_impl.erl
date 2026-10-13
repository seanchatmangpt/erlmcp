%%%-------------------------------------------------------------------
%%% @doc gen_server implementation for erlmcp_cli_completer instances.
%%% The public constructor is erlmcp_cli_completer:init/1,2 (tests-as-spec
%%% L184 requires init/1 to return {ok, Pid}), so the gen_server behaviour
%%% callbacks live here.
%%%
%%% Entry shape: {Kind, Name, Desc, Type, Subs}
%%%   Kind = command | flag | file | env
%%%   Name = binary
%%%   Desc = binary() | undefined
%%%   Type = atom() (command/flag/file/...) | undefined
%%%   Subs = [{subcommand, binary()}]
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_completer_impl).

-behaviour(gen_server).

-export([start_link/1, timeout_of/1, await_idle/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,
        {entries = [] :: [entry()],
         fuzzy = false :: boolean(),
         case_sensitive = true :: boolean(),
         file_completion = false :: boolean(),
         cache_enabled = false :: boolean(),
         cache = #{} :: #{{complete, binary()} => [binary()]},
         cache_hits = 0 :: non_neg_integer(),
         cache_misses = 0 :: non_neg_integer(),
         usage = #{} :: #{binary() => {non_neg_integer(), integer()}},
         providers = #{} :: #{term() => fun((binary()) -> [binary() | string()])},
         timeout = infinity :: timeout()}).

-type entry() :: {atom(), binary(), binary() | undefined, atom() | undefined,
                  [{subcommand, binary()}]}.

%%%===================================================================
%%% API helpers
%%%===================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Read the configured call timeout for this completer instance.
timeout_of(Pid) ->
    gen_server:call(Pid, get_timeout, 30000).

%% @doc Wait until the server finishes an in-flight (slow) handler, so the
%% caller can proceed (e.g. stop the server) after a client-side timeout.
await_idle(Pid) ->
    Ref = erlang:monitor(process, Pid),
    gen_server:cast(Pid, {await_idle, self()}),
    receive
        {idle, Pid} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _} ->
            ok
    after 60000 ->
        erlang:demonitor(Ref, [flush]),
        timeout
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    {ok, #state{entries = maps:get(entries, Opts, []),
                timeout = maps:get(timeout, Opts, infinity)}}.

handle_call(get_timeout, _From, State) ->
    {reply, State#state.timeout, State};
handle_call({add_entries, Entries}, _From, State) ->
    {reply, ok, State#state{entries = dedup(State#state.entries ++ Entries)}};
handle_call(get_commands, _From, State) ->
    {reply, {ok, [Name || {command, Name, _, _, _} <- State#state.entries]}, State};
handle_call(get_all_entries, _From, State) ->
    {reply, {ok, [{K, N} || {K, N, _, _, _} <- State#state.entries]}, State};
handle_call({complete, Mode, Prefix, OutFmt, Context}, _From, State) ->
    {Matches, NewState} = do_complete(Mode, Prefix, OutFmt, Context, State),
    {reply, {ok, Matches}, NewState};
handle_call({complete_path, Prefix}, _From, State) ->
    {reply, {ok, list_files(Prefix)}, State};
handle_call({add_env_var, Name}, _From, State) ->
    {reply, ok, State#state{entries = State#state.entries ++ [{env, Name, undefined, env, []}]}};
handle_call(enable_file_completion, _From, State) ->
    {reply, ok, State#state{file_completion = true}};
handle_call(enable_fuzzy, _From, State) ->
    {reply, ok, State#state{fuzzy = true}};
handle_call({set_case_sensitive, Bool}, _From, State) ->
    {reply, ok, State#state{case_sensitive = Bool}};
handle_call({record_usage, Name}, _From, State) ->
    Usage = State#state.usage,
    {Count, _} = maps:get(Name, Usage, {0, 0}),
    Entry = {Count + 1, erlang:monotonic_time(nanosecond)},
    {reply, ok, State#state{usage = maps:put(Name, Entry, Usage)}};
handle_call(enable_cache, _From, State) ->
    {reply, ok, State#state{cache_enabled = true}};
handle_call(get_cache_stats, _From, State) ->
    {reply, {ok, #{hits => State#state.cache_hits, misses => State#state.cache_misses}}, State};
handle_call(clear_cache, _From, State) ->
    {reply, ok,
     State#state{cache = #{}, cache_hits = 0, cache_misses = 0}};
handle_call({add_provider, Id, Fun}, _From, State) ->
    {reply, ok, State#state{providers = maps:put(Id, Fun, State#state.providers)}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({await_idle, Client}, State) ->
    Client ! {idle, self()},
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Completion logic
%%%===================================================================

do_complete(Mode, Prefix, OutFmt, Context, State0) ->
    {Matches, State1} = cached(Mode, Prefix, Context, State0),
    case OutFmt of
        info ->
            {[{Name, desc_of(find_entry(Name, State1))} || Name <- Matches], State1};
        type ->
            {[{Name, type_of(find_entry(Name, State1))} || Name <- Matches], State1};
        _ ->
            {Matches, State1}
    end.

cached(Mode, Prefix, Context, State0) ->
    Cacheable = Mode =:= prefix
                andalso Context =:= undefined
                andalso State0#state.cache_enabled,
    case Cacheable andalso maps:find({complete, Prefix}, State0#state.cache) of
        {ok, Matches} ->
            {Matches, State0#state{cache_hits = State0#state.cache_hits + 1}};
        _ ->
            {Matches, State1} = raw_complete(Mode, Prefix, Context, State0),
            State2 =
                case Cacheable of
                    true ->
                        State1#state{cache = maps:put({complete, Prefix},
                                                      Matches,
                                                      State1#state.cache),
                                     cache_misses = State1#state.cache_misses + 1};
                    false ->
                        State1
                end,
            {Matches, State2}
    end.

raw_complete(Mode, Prefix, Context, State) ->
    Matches0 = base_matches(Prefix, Context, State),
    Matches1 = run_providers(Prefix, Matches0, State),
    Matches2 = dedup_names(Matches1),
    {sort_matches(Mode, Matches2, State), State}.

%% Prefix matching against entries (commands/flags/files/env), plus
%% subcommand completion ("validate s"), wildcard ("test*"), env vars
%% ("$P"), fuzzy fallback (when enabled).
base_matches(Prefix, Context, State) ->
    IsCS = State#state.case_sensitive,
    case binary_split(Prefix, <<" ">>) of
        {Head, Rest} when Rest =/= <<>> ->
            %% "cmd sub..." — complete the last token within the command's
            %% subcommands
            Stem = last_token(Rest),
            Subs = subcommands_of(Head, State),
            [S || {subcommand, S} <- Subs, prefix_match(IsCS, S, Stem)];
        _ ->
            case Context of
                [Cmd | _] when is_binary(Cmd), Prefix =/= <<>> ->
                    Subs = subcommands_of(Cmd, State),
                    case Subs of
                        [] ->
                            prefix_matches(Prefix, IsCS, State);
                        _ ->
                            [S || {subcommand, S} <- Subs, prefix_match(IsCS, S, Prefix)]
                    end;
                _ ->
                    prefix_matches(Prefix, IsCS, State)
            end
    end.

last_token(Rest) ->
    case binary_split(Rest, <<" ">>) of
        {_, More} when More =/= <<>> ->
            last_token(More);
        _ ->
            Rest
    end.

binary_split(Bin, Sep) ->
    case binary:split(Bin, Sep) of
        [A, B] ->
            {A, B};
        [A] ->
            {A, <<>>}
    end.

prefix_matches(Prefix, IsCS, State) ->
    case Prefix of
        <<"$", Rest/binary>> ->
            [Name
             || {env, Name, _, _, _} <- State#state.entries,
                prefix_match(IsCS, Name, <<"$", Rest/binary>>)
                orelse prefix_match(IsCS, Name, Rest)];
        _ ->
            {Wildcard, Base} =
                case wildcard_split(Prefix) of
                    {true, B} ->
                        {true, B};
                    false ->
                        {false, Prefix}
                end,
            Candidates = entry_names(State) ++ path_candidates(State),
            BaseMatches = [C || C <- Candidates, prefix_match(IsCS, C, Base)],
            case Wildcard of
                true ->
                    BaseMatches;
                false ->
                    case BaseMatches of
                        [] ->
                            fuzzy_or_empty(Prefix, Candidates, IsCS, State);
                        _ ->
                            BaseMatches
                    end
            end
    end.

fuzzy_or_empty(Prefix, Candidates, IsCS, State) ->
    case State#state.fuzzy andalso byte_size(Prefix) > 0 of
        true ->
            [C || C <- Candidates, is_subsequence(fold(Prefix, IsCS), fold(C, IsCS))];
        false ->
            []
    end.

subcommands_of(Cmd, State) ->
    case [Subs || {command, N, _, _, Subs} <- State#state.entries, N =:= Cmd, Subs =/= []] of
        [Subs | _] ->
            Subs;
        [] ->
            []
    end.

prefix_match(_IsCS, _Candidate, <<>>) ->
    true;
prefix_match(IsCS, Candidate, Prefix) ->
    C = fold(Candidate, IsCS),
    P = fold(Prefix, IsCS),
    case byte_size(C) >= byte_size(P) of
        true ->
            binary:part(C, 0, byte_size(P)) =:= P;
        false ->
            false
    end.

fold(S, true) ->
    S;
fold(S, false) ->
    string:lowercase(S).

wildcard_split(Prefix) ->
    S = byte_size(Prefix) - 1,
    case S >= 0 andalso binary:at(Prefix, S) =:= $* of
        true ->
            {true, binary:part(Prefix, 0, S)};
        false ->
            false
    end.

entry_names(State) ->
    [N || {_, N, _, _, _} <- State#state.entries].

path_candidates(State) ->
    case State#state.file_completion of
        true ->
            [];
        false ->
            []
    end.

list_files(Prefix) ->
    {Dir, Stem} = split_path(Prefix),
    case file:list_dir(Dir) of
        {ok, Names} ->
            Matches =
                [filename:join(unicode:characters_to_binary(Dir),
                               unicode:characters_to_binary(N))
                 || N <- Names,
                    prefix_match(false,
                                 unicode:characters_to_binary(N),
                                 unicode:characters_to_binary(Stem))],
            lists:sort(Matches);
        {error, _} ->
            []
    end.

split_path(Prefix) ->
    Str = unicode:characters_to_list(Prefix),
    case Str of
        [] ->
            {"./", ""};
        _ ->
            case lists:last(Str) of
                $/ ->
                    {Str, ""};
                _ ->
                    Dir = filename:dirname(Str),
                    Stem = filename:basename(Str),
                    {Dir ++ "/", Stem}
            end
    end.

run_providers(Prefix, Base, State) ->
    maps:fold(fun(_Id, Fun, Acc) ->
                 case catch Fun(Prefix) of
                     Matches when is_list(Matches) ->
                         Acc ++ [unicode:characters_to_binary(M) || M <- Matches];
                     _ ->
                         Acc
                 end
              end,
              Base,
              State#state.providers).

sort_matches(ranked_freq, Names, State) ->
    lists:sort(fun(A, B) -> usage_count(A, State) >= usage_count(B, State) end, Names);
sort_matches(ranked_recency, Names, State) ->
    lists:sort(fun(A, B) -> usage_time(A, State) >= usage_time(B, State) end, Names);
sort_matches(_Other, Names, _State) ->
    lists:sort(Names).

usage_count(Name, State) ->
    element(1, maps:get(Name, State#state.usage, {0, 0})).

usage_time(Name, State) ->
    element(2, maps:get(Name, State#state.usage, {0, 0})).

desc_of({_, _, Desc, _, _}) when is_binary(Desc) ->
    Desc;
desc_of(_) ->
    undefined.

type_of({_, _, _, Type, _}) when is_atom(Type) ->
    Type;
type_of(_) ->
    undefined.

find_entry(Name, State) ->
    case [E || {_, N, _, _, _} = E <- State#state.entries, N =:= Name] of
        [E | _] ->
            E;
        [] ->
            undefined
    end.

dedup(Entries) ->
    lists:usort(fun({K1, N1, _, _, _}, {K2, N2, _, _, _}) ->
                   {K1, N1} =< {K2, N2}
                end,
                Entries).

dedup_names(Names) ->
    lists:usort(Names).

is_subsequence(Pattern, Subject) ->
    is_subseq(binary_to_list(Pattern), binary_to_list(Subject)).

is_subseq([], _Subject) ->
    true;
is_subseq(_Pattern, []) ->
    false;
is_subseq([P | RestP], [P | RestS]) ->
    is_subseq(RestP, RestS);
is_subseq(Pattern, [_ | RestS]) ->
    is_subseq(Pattern, RestS).
