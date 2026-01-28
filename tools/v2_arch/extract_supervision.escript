#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

% Extract supervision tree structure from erlmcp source code
% Generates supervision.json with all supervisor hierarchies

-mode(compile).

-record(supervisor, {
    module :: atom(),
    strategy :: atom(),
    intensity :: non_neg_integer(),
    period :: non_neg_integer(),
    children :: [child_spec()]
}).

-record(child_spec, {
    id :: atom() | term(),
    module :: atom(),
    type :: supervisor | worker,
    restart :: permanent | temporary | transient,
    shutdown :: integer() | infinity
}).

main([]) ->
    SupDir = "src",
    case file:list_dir(SupDir) of
        {ok, Files} ->
            SupFiles = [F || F <- Files, string:str(F, "_sup.erl") > 0],
            Supervisors = extract_supervisors(SupDir, SupFiles),
            Output = format_json(Supervisors),
            write_output(Output);
        {error, Reason} ->
            io:format("Error reading src directory: ~p~n", [Reason]),
            halt(1)
    end;
main(_) ->
    io:format("Usage: extract_supervision.escript~n"),
    halt(1).

extract_supervisors(Dir, Files) ->
    lists:filtermap(fun(File) ->
        Path = filename:join(Dir, File),
        case extract_supervisor_from_file(Path) of
            {ok, Sup} -> {true, Sup};
            error -> false
        end
    end, Files).

extract_supervisor_from_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            extract_supervisor_from_content(Path, Content);
        {error, _} ->
            error
    end.

extract_supervisor_from_content(Path, Content) ->
    Module = extract_module_name(Path),
    ContentStr = binary_to_list(Content),

    % Extract strategy
    Strategy = extract_strategy(ContentStr),

    % Extract intensity and period
    {Intensity, Period} = extract_intensity_period(ContentStr),

    % Extract children
    Children = extract_children(ContentStr),

    case Strategy =/= undefined of
        true ->
            {ok, #{
                module => Module,
                strategy => Strategy,
                intensity => Intensity,
                period => Period,
                children => Children
            }};
        false ->
            error
    end.

extract_module_name(Path) ->
    Basename = filename:basename(Path, ".erl"),
    list_to_atom(Basename).

extract_strategy(Content) ->
    case re:run(Content, "strategy\\s*=>\\s*(one_for_one|one_for_all|rest_for_one|simple_one_for_one)",
                [{capture, [1], list}]) of
        {match, [Strategy]} ->
            list_to_atom(Strategy);
        nomatch ->
            undefined
    end.

extract_intensity_period(Content) ->
    Intensity = case re:run(Content, "intensity\\s*=>\\s*(\\d+)",
                            [{capture, [1], list}]) of
        {match, [I]} -> list_to_integer(I);
        nomatch -> 5
    end,
    Period = case re:run(Content, "period\\s*=>\\s*(\\d+)",
                         [{capture, [1], list}]) of
        {match, [P]} -> list_to_integer(P);
        nomatch -> 60
    end,
    {Intensity, Period}.

extract_children(Content) ->
    % Find ChildSpecs list
    case re:run(Content, "ChildSpecs\\s*=\\s*\\[([^\\]]+)\\]",
                [{capture, [1], list}]) of
        {match, [ChildContent]} ->
            parse_child_specs(ChildContent);
        nomatch ->
            []
    end.

parse_child_specs(Content) ->
    % Split by #{ patterns
    Specs = re:split(Content, "#{", [{return, list}]),
    lists:filtermap(fun(Spec) ->
        case parse_child_spec("#{" ++ Spec) of
            {ok, Child} -> {true, Child};
            error -> false
        end
    end, Specs).

parse_child_spec(Spec) ->
    try
        Id = extract_child_id(Spec),
        Module = extract_child_module(Spec),
        Type = extract_child_type(Spec),
        Restart = extract_child_restart(Spec),
        Shutdown = extract_child_shutdown(Spec),
        {ok, #{
            id => Id,
            module => Module,
            type => Type,
            restart => Restart,
            shutdown => Shutdown
        }}
    catch
        _:_ -> error
    end.

extract_child_id(Spec) ->
    case re:run(Spec, "id\\s*=>\\s*([a-zA-Z_][a-zA-Z0-9_]*)",
                [{capture, [1], list}]) of
        {match, [Id]} -> list_to_atom(Id);
        nomatch -> undefined
    end.

extract_child_module(Spec) ->
    case re:run(Spec, "start\\s*=>\\s*\\{([a-zA-Z_][a-zA-Z0-9_]*),",
                [{capture, [1], list}]) of
        {match, [Module]} -> list_to_atom(Module);
        nomatch -> undefined
    end.

extract_child_type(Spec) ->
    case re:run(Spec, "type\\s*=>\\s*(supervisor|worker)",
                [{capture, [1], list}]) of
        {match, [Type]} -> list_to_atom(Type);
        nomatch -> worker
    end.

extract_child_restart(Spec) ->
    case re:run(Spec, "restart\\s*=>\\s*(permanent|temporary|transient)",
                [{capture, [1], list}]) of
        {match, [Restart]} -> list_to_atom(Restart);
        nomatch -> temporary
    end.

extract_child_shutdown(Spec) ->
    case re:run(Spec, "shutdown\\s*=>\\s*(infinity|\\d+)",
                [{capture, [1], list}]) of
        {match, [Shutdown]} ->
            case Shutdown of
                "infinity" -> infinity;
                N -> list_to_integer(N)
            end;
        nomatch -> 5000
    end.

format_json(Supervisors) ->
    io_lib:format("~s~n", [jsx:encode(#{
        supervisors => Supervisors,
        timestamp => erlang:system_time(second),
        source => "extract_supervision.escript"
    })]).

write_output(Output) ->
    OutputFile = "tools/v2_arch/supervision.json",
    case file:write_file(OutputFile, Output) of
        ok ->
            io:format("Supervision tree extracted to ~s~n", [OutputFile]);
        {error, Reason} ->
            io:format("Error writing output: ~p~n", [Reason]),
            halt(1)
    end.
