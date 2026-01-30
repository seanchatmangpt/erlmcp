#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc Add State Versioning to gen_server Modules
%%%
%%% This script adds version fields to all gen_server modules that
%%% don't have them yet, enabling proper hot code loading.
%%%
%%% Usage: ./add_state_versioning.erl <module_path>
%%%-------------------------------------------------------------------
-mode(compile).

main([ModulePath]) ->
    add_versioning(ModulePath);
main(_) ->
    io:format("Usage: add_state_versioning.erl <module_path>~n"),
    halt(1).

%% @private Add versioning to a module
add_versioning(ModulePath) ->
    io:format("Processing: ~s~n", [ModulePath]),

    %% Read file
    {ok, Content} = file:read_file(ModulePath),
    Text = binary_to_list(Content),

    %% Check if already has version field
    case has_version_field(Text) of
        true ->
            io:format("  Already has version field, skipping~n"),
            ok;
        false ->
            io:format("  Adding version field...~n"),
            NewContent = add_version_to_module(Text),
            file:write_file(ModulePath, list_to_binary(NewContent)),
            io:format("  Done~n")
    end.

%% @private Check if module already has version field
has_version_field(Text) ->
    case re:run(Text, "version\\s*::\\s*(v1|v2|v3|state_version\\(\\))", [caseless]) of
        {match, _} -> true;
        nomatch -> false
    end.

%% @private Add version field to module
add_version_to_module(Text) ->
    %% Find state record definition
    case re:run(Text, "-record\\(state,\\s*\\{", [caseless]) of
        {match, [{Start, _}]} ->
            %% Find the opening brace
            Before = string:substr(Text, 1, Start + 17),  % "-record(state, {"
            After = string:substr(Text, Start + 18),

            %% Insert version field as first field
            VersionField = "version = v1 :: v1 | v2,  % State version for hot code loading\n    ",

            %% Also need to update code_change
            NewText = Before ++ VersionField ++ After,
            update_code_change(NewText);
        nomatch ->
            io:format("  No state record found, skipping~n"),
            Text
    end.

%% @private Update code_change/3 function
update_code_change(Text) ->
    %% Find trivial code_change implementation
    TrivialPattern = "code_change\\(_OldVsn,\\s*State,\\s*_Extra\\)\\s*->\\s*\\{ok,\\s*State\\}\\.",

    case re:run(Text, TrivialPattern, [caseless, dotall]) of
        {match, [{Start, Length}]} ->
            %% Extract code_change location
            Before = string:substr(Text, 1, Start),
            After = string:substr(Text, Start + Length),

            %% Generate proper code_change implementation
            ModuleName = extract_module_name(Text),

            NewCodeChange = io_lib:format(
                "code_change(OldVsn, State, Extra) ->~n"
                "    try~n"
                "        logger:info(\"~p: Code change from ~p\", [?MODULE, OldVsn]),~n"
                "        NewState = migrate_state(OldVsn, State, Extra),~n"
                "        logger:info(\"~p: Code change completed successfully\", [?MODULE]),~n"
                "        {ok, NewState}~n"
                "    catch~n"
                "        Class:Reason:Stack ->~n"
                "            logger:error(\"~p: Code change failed: ~p:~p~n~p\",~n"
                "                        [?MODULE, Class, Reason, Stack]),~n"
                "            error({code_change_failed, Class, Reason})~n"
                "    end.~n~n"
                "%% @private Migrate state based on version~n"
                "-spec migrate_state(term(), #state{}, term()) -> #state{}.~n"
                "migrate_state(_OldVsn, #state{version = v1} = State, _Extra) ->~n"
                "    State;~n"
                "migrate_state({down, _FromVsn}, #state{} = State, _Extra) ->~n"
                "    case State#state.version of~n"
                "        undefined -> State#state{version = v1};~n"
                "        _ -> State~n"
                "    end;~n"
                "migrate_state(OldVsn, #state{version = undefined} = State, _Extra)~n"
                "  when is_list(OldVsn); is_atom(OldVsn) ->~n"
                "    logger:info(\"~p: Upgrading legacy state to v1\", [?MODULE]),~n"
                "    State#state{version = v1};~n"
                "migrate_state(OldVsn, State, _Extra) ->~n"
                "    logger:warning(\"~p: Unknown code_change from version ~p\", [?MODULE, OldVsn]),~n"
                "    State.~n~n",
                [ModuleName]),

            Before ++ NewCodeChange ++ After;
        nomatch ->
            %% Already has proper code_change or no code_change found
            Text
    end.

%% @private Extract module name from file
extract_module_name(Text) ->
    case re:run(Text, "-module\\((\\w+)\\)", [caseless]) of
        {match, [{_, Len}, {NameStart, NameLen}]} ->
            Name = string:substr(Text, NameStart + 1, NameLen),
            list_to_atom(Name);
        nomatch ->
            unknown_module
    end.
