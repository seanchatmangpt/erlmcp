-module(erlmcp_stdio_tests).

-include_lib("eunit/include/eunit.hrl").

stdio_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_server_lifecycle()),
             ?_test(test_add_tool()),
             ?_test(test_add_resource()),
             ?_test(test_add_prompt()),
             ?_test(test_is_running()),
             ?_test(test_error_when_not_running())
         ]
     end}.

setup() ->
    % Ensure the application is started
    application:ensure_all_started(erlmcp),
    % Make sure stdio server is stopped before tests
    erlmcp_stdio:stop(),
    ok.

cleanup(_) ->
    % Stop the stdio server if it's running
    erlmcp_stdio:stop().

test_server_lifecycle() ->
    % Initially not running
    ?assertEqual(false, erlmcp_stdio:is_running()),
    
    % Test starting the server
    ?assertEqual(ok, erlmcp_stdio:start()),
    ?assertEqual(true, erlmcp_stdio:is_running()),
    
    % Test stopping the server
    ?assertEqual(ok, erlmcp_stdio:stop()),
    ?assertEqual(false, erlmcp_stdio:is_running()),
    
    % Test that stopping again is ok
    ?assertEqual(ok, erlmcp_stdio:stop()).

test_add_tool() ->
    % Start server
    ?assertEqual(ok, erlmcp_stdio:start()),
    
    % Add a simple tool
    Handler = fun(#{<<"input">> := Input}) ->
        <<"Processed: ", Input/binary>>
    end,
    
    ?assertEqual(ok, erlmcp_stdio:add_tool(<<"test_tool">>, <<"Test tool">>, Handler)),
    
    % Add a tool with schema
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"input">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"input">>]
    },
    
    ?assertEqual(ok, erlmcp_stdio:add_tool(<<"schema_tool">>, <<"Tool with schema">>, Handler, Schema)),
    
    % Clean up
    erlmcp_stdio:stop().

test_add_resource() ->
    % Start server
    ?assertEqual(ok, erlmcp_stdio:start()),
    
    % Add a simple resource
    Handler = fun(Uri) ->
        <<"Content for ", Uri/binary>>
    end,
    
    ?assertEqual(ok, erlmcp_stdio:add_resource(<<"test://resource">>, <<"Test resource">>, Handler)),
    
    % Add a resource with custom mime type
    ?assertEqual(ok, erlmcp_stdio:add_resource(<<"test://json">>, <<"JSON resource">>, Handler, <<"application/json">>)),
    
    % Clean up
    erlmcp_stdio:stop().

test_add_prompt() ->
    % Start server
    ?assertEqual(ok, erlmcp_stdio:start()),
    
    % Add a simple prompt
    Handler = fun(Args) ->
        Topic = maps:get(<<"topic">>, Args, <<"default">>),
        [#{
            <<"role">> => <<"user">>,
            <<"content">> => #{
                <<"type">> => <<"text">>,
                <<"text">> => <<"Write about ", Topic/binary>>
            }
        }]
    end,
    
    ?assertEqual(ok, erlmcp_stdio:add_prompt(<<"test_prompt">>, <<"Test prompt">>, Handler)),
    
    % Add a prompt with arguments
    Arguments = [
        #{<<"name">> => <<"topic">>, <<"description">> => <<"Topic to write about">>, <<"required">> => true}
    ],
    
    ?assertEqual(ok, erlmcp_stdio:add_prompt(<<"arg_prompt">>, <<"Prompt with args">>, Handler, Arguments)),
    
    % Clean up
    erlmcp_stdio:stop().

test_is_running() ->
    % Initially not running
    ?assertEqual(false, erlmcp_stdio:is_running()),
    
    % Start and check
    ?assertEqual(ok, erlmcp_stdio:start()),
    ?assertEqual(true, erlmcp_stdio:is_running()),
    
    % Stop and check
    ?assertEqual(ok, erlmcp_stdio:stop()),
    ?assertEqual(false, erlmcp_stdio:is_running()).

test_error_when_not_running() ->
    % Ensure server is not running
    erlmcp_stdio:stop(),
    
    % Test that API calls fail when server is not running
    Handler = fun(_) -> <<"test">> end,
    
    ?assertEqual({error, stdio_server_not_running}, 
                 erlmcp_stdio:add_tool(<<"test">>, <<"Test">>, Handler)),
    ?assertEqual({error, stdio_server_not_running}, 
                 erlmcp_stdio:add_resource(<<"test://uri">>, <<"Test">>, Handler)),
    ?assertEqual({error, stdio_server_not_running}, 
                 erlmcp_stdio:add_prompt(<<"test">>, <<"Test">>, Handler)).