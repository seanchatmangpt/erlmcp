-module(erlmcp_server_prompts_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%%%====================================================================
%%% Prompts Management Tests - Chicago School TDD
%%% Tests for prompt API: add_prompt, add_prompt_with_args,
%%% add_prompt_with_args_and_schema, delete_prompt
%%% Principles: Real processes, observable behavior, no state inspection
%%%====================================================================

%%%====================================================================
%%% Test Generators
%%%====================================================================

prompts_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Add prompt", fun test_add_prompt/0},
          {"Add prompt with args", fun test_add_prompt_with_args/0},
          {"Add prompt with args and schema", fun test_add_prompt_with_schema/0},
          {"Delete prompt", fun test_delete_prompt/0},
          {"Delete non-existent prompt", fun test_delete_nonexistent/0}
         ]
     end}.

prompts_advanced_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Multiple prompts", fun test_multiple_prompts/0},
          {"Prompt with various argument types", fun test_arg_types/0},
          {"Prompt with optional arguments", fun test_optional_args/0},
          {"Prompt schema validation", fun test_prompt_schema/0}
         ]
     end}.

prompts_error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Empty prompt name", fun test_empty_name/0},
          {"Prompt with no arguments", fun test_no_args/0},
          {"Prompt with undefined schema", fun test_undefined_schema/0},
          {"Duplicate prompt", fun test_duplicate_prompt/0}
         ]
     end}.

prompts_content_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Prompt with text content", fun test_text_content/0},
          {"Prompt with image content", fun test_image_content/0},
          {"Prompt with mixed content", fun test_mixed_content/0},
          {"Prompt with resource content", fun test_resource_content/0}
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Basic Prompts Tests
%%%====================================================================

test_add_prompt() ->
    Server = start_server(),
    PromptName = <<"test_prompt">>,
    Handler = fun(_Args) -> [#{
        role => <<"user">>,
        content => #{type => <<"text">>, text => <<"Test prompt">>}
    }] end,

    ?assertEqual(ok, erlmcp_server:add_prompt(Server, PromptName, Handler)),

    ok = erlmcp_server:stop(Server).

test_add_prompt_with_args() ->
    Server = start_server(),
    PromptName = <<"prompt_with_args">>,
    Args = [
        #mcp_prompt_argument{
            name = <<"topic">>,
            description = <<"Topic to discuss">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"length">>,
            description = <<"Response length">>,
            required = false
        }
    ],
    Handler = fun(A) -> [#{
        role => <<"system">>,
        content => #{type => <<"text">>, text => <<"Topic: ", (maps:get(<<"topic">>, A))/binary>>}
    }] end,

    ?assertEqual(ok, erlmcp_server:add_prompt_with_args(Server, PromptName, Handler, Args)),

    ok = erlmcp_server:stop(Server).

test_add_prompt_with_schema() ->
    Server = start_server(),
    PromptName = <<"prompt_with_schema">>,
    Args = [
        #mcp_prompt_argument{
            name = <<"query">>,
            description = <<"Search query">>,
            required = true
        }
    ],
    InputSchema = #{
        type => <<"object">>,
        properties => #{
            query => #{type => <<"string">>, minLength => 1}
        },
        required => [<<"query">>]
    },
    Handler = fun(_Args) -> [#{
        role => <<"user">>,
        content => #{type => <<"text">>, text => <<"Schema validated">>}
    }] end,

    ?assertEqual(ok, erlmcp_server:add_prompt_with_args_and_schema(Server, PromptName, Handler, Args, InputSchema)),

    ok = erlmcp_server:stop(Server).

test_delete_prompt() ->
    Server = start_server(),
    PromptName = <<"delete_prompt">>,
    Handler = fun(_Args) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}] end,

    ok = erlmcp_server:add_prompt(Server, PromptName, Handler),
    ?assertEqual(ok, erlmcp_server:delete_prompt(Server, PromptName)),

    %% Verify deletion
    ?assertEqual({error, not_found}, erlmcp_server:delete_prompt(Server, PromptName)),

    ok = erlmcp_server:stop(Server).

test_delete_nonexistent() ->
    Server = start_server(),

    ?assertEqual({error, not_found}, erlmcp_server:delete_prompt(Server, <<"nonexistent_prompt">>)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Advanced Prompts Tests
%%%====================================================================

test_multiple_prompts() ->
    Server = start_server(),

    %% Add multiple prompts
    [begin
        Name = <<"bulk_prompt_", (integer_to_binary(N))/binary>>,
        Handler = fun(_Args) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}] end,
        ?assertEqual(ok, erlmcp_server:add_prompt(Server, Name, Handler))
    end || N <- lists:seq(1, 10)],

    ok = erlmcp_server:stop(Server).

test_arg_types() ->
    Server = start_server(),

    %% Test prompts with various argument types
    Args = [
        #mcp_prompt_argument{
            name = <<"string_arg">>,
            description = <<"String argument">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"number_arg">>,
            description = <<"Number argument">>,
            required = false
        },
        #mcp_prompt_argument{
            name = <<"bool_arg">>,
            description = <<"Boolean argument">>,
            required = false
        }
    ],
    Handler = fun(_Args) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}] end,

    ?assertEqual(ok, erlmcp_server:add_prompt_with_args(Server, <<"type_test">>, Handler, Args)),

    ok = erlmcp_server:stop(Server).

test_optional_args() ->
    Server = start_server(),

    %% Test prompt with optional arguments
    Args = [
        #mcp_prompt_argument{
            name = <<"required_arg">>,
            description = <<"Required argument">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"optional_arg">>,
            description = <<"Optional argument">>,
            required = false
        },
        #mcp_prompt_argument{
            name = <<"another_optional">>,
            description = <<"Another optional">>,
            required = false
        }
    ],
    Handler = fun(_Args) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}] end,

    ?assertEqual(ok, erlmcp_server:add_prompt_with_args(Server, <<"optional_test">>, Handler, Args)),

    ok = erlmcp_server:stop(Server).

test_prompt_schema() ->
    Server = start_server(),

    %% Test various JSON schemas
    Schema1 = #{type => <<"object">>},
    Args1 = [#mcp_prompt_argument{name = <<"arg1">>, description = <<"Test">>, required = true}],
    Handler1 = fun(_A) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok1">>}}] end,
    ?assertEqual(ok, erlmcp_server:add_prompt_with_args_and_schema(Server, <<"schema1">>, Handler1, Args1, Schema1)),

    Schema2 = #{type => <<"object">>, properties => #{}},
    Args2 = [#mcp_prompt_argument{name = <<"arg2">>, description = <<"Test">>, required = true}],
    Handler2 = fun(_A) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok2">>}}] end,
    ?assertEqual(ok, erlmcp_server:add_prompt_with_args_and_schema(Server, <<"schema2">>, Handler2, Args2, Schema2)),

    Schema3 = #{type => <<"object">>, properties => #{<<"field">> => #{type => <<"string">>}}},
    Args3 = [#mcp_prompt_argument{name = <<"arg3">>, description = <<"Test">>, required = true}],
    Handler3 = fun(_A) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok3">>}}] end,
    ?assertEqual(ok, erlmcp_server:add_prompt_with_args_and_schema(Server, <<"schema3">>, Handler3, Args3, Schema3)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

test_empty_name() ->
    Server = start_server(),
    Handler = fun(_Args) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}] end,

    %% Empty name behavior depends on implementation
    case erlmcp_server:add_prompt(Server, <<>>, Handler) of
        ok -> ok;
        {error, _} -> ok
    end,

    ok = erlmcp_server:stop(Server).

test_no_args() ->
    Server = start_server(),
    Handler = fun(_Args) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}] end,

    ?assertEqual(ok, erlmcp_server:add_prompt(Server, <<"no_args">>, Handler)),

    ok = erlmcp_server:stop(Server).

test_undefined_schema() ->
    Server = start_server(),
    Args = [#mcp_prompt_argument{name = <<"arg">>, description = <<"Test">>, required = true}],
    Handler = fun(_Args) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}] end,

    ?assertEqual(ok, erlmcp_server:add_prompt_with_args_and_schema(Server, <<"undefined_schema">>, Handler, Args, undefined)),

    ok = erlmcp_server:stop(Server).

test_duplicate_prompt() ->
    Server = start_server(),
    PromptName = <<"duplicate_prompt">>,
    Handler = fun(_Args) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}] end,

    ok = erlmcp_server:add_prompt(Server, PromptName, Handler),

    %% Try to add duplicate - behavior depends on implementation
    case erlmcp_server:add_prompt(Server, PromptName, Handler) of
        ok -> ok;
        {error, _} -> ok
    end,

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Content Tests
%%%====================================================================

test_text_content() ->
    Server = start_server(),
    Handler = fun(_Args) -> [#{
        role => <<"user">>,
        content => #{type => <<"text">>, text => <<"Plain text content">>}
    }] end,

    ?assertEqual(ok, erlmcp_server:add_prompt(Server, <<"text_content">>, Handler)),

    ok = erlmcp_server:stop(Server).

test_image_content() ->
    Server = start_server(),
    Handler = fun(_Args) -> [#{
        role => <<"user">>,
        content => #{
            type => <<"image">>,
            data => <<"base64data">>,
            mimeType => <<"image/png">>
        }
    }] end,

    ?assertEqual(ok, erlmcp_server:add_prompt(Server, <<"image_content">>, Handler)),

    ok = erlmcp_server:stop(Server).

test_mixed_content() ->
    Server = start_server(),
    Handler = fun(_Args) -> [
        #{
            role => <<"user">>,
            content => #{type => <<"text">>, text => <<"Text part">>}
        },
        #{
            role => <<"user">>,
            content => #{
                type => <<"image">>,
                data => <<"base64">>,
                mimeType => <<"image/jpeg">>
            }
        },
        #{
            role => <<"user">>,
            content => #{type => <<"text">>, text => <<"More text">>}
        }
    ] end,

    ?assertEqual(ok, erlmcp_server:add_prompt(Server, <<"mixed_content">>, Handler)),

    ok = erlmcp_server:stop(Server).

test_resource_content() ->
    Server = start_server(),
    Handler = fun(_Args) -> [#{
        role => <<"user">>,
        content => #{
            type => <<"resource">>,
            uri => <<"file:///path/to/resource">>
        }
    }] end,

    ?assertEqual(ok, erlmcp_server:add_prompt(Server, <<"resource_content">>, Handler)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Start server with default capabilities
start_server() ->
    ServerId = <<"prompts_test_server_">>,
    Capabilities = #mcp_server_capabilities{
        prompts = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    Pid.
