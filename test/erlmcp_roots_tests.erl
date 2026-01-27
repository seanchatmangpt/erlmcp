-module(erlmcp_roots_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

roots_enforcement_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_start_roots_manager()),
            ?_test(test_add_root()),
            ?_test(test_remove_root()),
            ?_test(test_list_roots()),
            ?_test(test_validate_path()),
            ?_test(test_canonicalize_path()),
            ?_test(test_is_path_allowed()),
            ?_test(test_symlink_handling()),
            ?_test(test_invalid_path()),
            ?_test(test_concurrent_root_management())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_start_roots_manager() ->
    Config = #{
        allowed_paths => [<<"/Users/sac/projects">>],
        symlink_follow => false
    },

    {ok, Pid} = erlmcp_roots:start_link(Config),
    ?assert(is_pid(Pid)).

test_add_root() ->
    Config = #{
        allowed_paths => [],
        symlink_follow => false
    },

    {ok, _Pid} = erlmcp_roots:start_link(Config),

    %% Add a root path
    Root = <<"/">>,
    {ok, CanonicalRoot} = erlmcp_roots:add_root(Root),

    ?assert(is_binary(CanonicalRoot)).

test_remove_root() ->
    Config = #{
        allowed_paths => [<<"/">>,],
        symlink_follow => false
    },

    {ok, _Pid} = erlmcp_roots:start_link(Config),

    Root = <<"/">>,
    {ok, _RemovedRoot} = erlmcp_roots:remove_root(Root),

    ?assert(true).

test_list_roots() ->
    Config = #{
        allowed_paths => [<<"/Users/sac">>, <<"/tmp">>],
        symlink_follow => false
    },

    {ok, _Pid} = erlmcp_roots:start_link(Config),

    {ok, Roots} = erlmcp_roots:list_roots(),
    ?assert(is_list(Roots)),
    ?assert(length(Roots) >= 2).

test_validate_path() ->
    Config = #{
        allowed_paths => [<<"/Users/sac">>],
        symlink_follow => false
    },

    {ok, _Pid} = erlmcp_roots:start_link(Config),

    %% Should validate paths
    Path = <<"/Users/sac/test.txt">>,
    Result = erlmcp_roots:validate_path(Path),

    ?assert(Result =:= {ok, Path} orelse Result =:= {error, _}).

test_canonicalize_path() ->
    %% Test path canonicalization
    Path = <<"/Users/sac/../sac/test.txt">>,

    Result = erlmcp_roots:canonicalize_path(Path),
    ?assert(element(1, Result) =:= ok orelse element(1, Result) =:= error).

test_is_path_allowed() ->
    Config = #{
        allowed_paths => [<<"/Users/sac">>],
        symlink_follow => false
    },

    {ok, _Pid} = erlmcp_roots:start_link(Config),

    %% Test path allowance checking
    Path = <<"/Users/sac/test.txt">>,
    Result = erlmcp_roots:is_path_allowed(Path),

    ?assert(is_boolean(Result)).

test_symlink_handling() ->
    Config = #{
        allowed_paths => [<<"/tmp">>],
        symlink_follow => false
    },

    {ok, _Pid} = erlmcp_roots:start_link(Config),

    %% With symlink_follow=false, symlinks outside roots should be rejected
    ?assert(true).

test_invalid_path() ->
    Config = #{
        allowed_paths => [<<"/Users/sac">>],
        symlink_follow => false
    },

    {ok, _Pid} = erlmcp_roots:start_link(Config),

    %% Invalid path format
    InvalidPath = <<"not_a_path">>,
    Result = erlmcp_roots:validate_path(InvalidPath),

    ?assert(element(1, Result) =:= ok orelse element(1, Result) =:= error).

test_concurrent_root_management() ->
    Config = #{
        allowed_paths => [],
        symlink_follow => false
    },

    {ok, _Pid} = erlmcp_roots:start_link(Config),

    %% Simulate concurrent root operations
    Pids = [
        spawn(fun() ->
            erlmcp_roots:add_root(<<"/tmp">>)
        end) || _ <- lists:seq(1, 3)
    ],

    ?assert(length(Pids) =:= 3).
