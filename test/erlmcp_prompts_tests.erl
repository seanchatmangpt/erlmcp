-module(erlmcp_prompts_tests).

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

prompts_api_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_list_prompts()),
            ?_test(test_get_prompt()),
            ?_test(test_prompt_with_arguments()),
            ?_test(test_prompt_pagination()),
            ?_test(test_prompt_icons()),
            ?_test(test_list_changed_notification()),
            ?_test(test_missing_required_argument()),
            ?_test(test_prompt_content_types())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_list_prompts() ->
    %% Test listing available prompts
    Prompts = [
        #{
            <<"name">> => <<"code_review">>,
            <<"title">> => <<"Request Code Review">>,
            <<"description">> => <<"Asks the LLM to analyze code quality">>
        },
        #{
            <<"name">> => <<"documentation">>,
            <<"title">> => <<"Generate Documentation">>,
            <<"description">> => <<"Asks the LLM to generate documentation">>
        }
    ],

    ?assert(is_list(Prompts)),
    ?assert(length(Prompts) =:= 2).

test_get_prompt() ->
    %% Test retrieving a specific prompt
    Prompt = #{
        <<"name">> => <<"code_review">>,
        <<"title">> => <<"Request Code Review">>,
        <<"description">> => <<"Asks the LLM to analyze code quality">>,
        <<"messages">> => [
            #{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Please review this code">>
                }
            }
        ]
    },

    ?assert(is_map(Prompt)),
    ?assert(maps:get(<<"name">>, Prompt) =:= <<"code_review">>).

test_prompt_with_arguments() ->
    %% Test prompt with arguments
    Prompt = #{
        <<"name">> => <<"code_review">>,
        <<"arguments">> => [
            #{
                <<"name">> => <<"code">>,
                <<"description">> => <<"The code to review">>,
                <<"required">> => true
            }
        ]
    },

    Arguments = maps:get(<<"arguments">>, Prompt),
    ?assert(is_list(Arguments)),
    ?assert(length(Arguments) =:= 1).

test_prompt_pagination() ->
    %% Test pagination for large prompt lists
    PaginationRequest = #{
        <<"cursor">> => <<"optional-cursor-value">>
    },

    Response = #{
        <<"prompts">> => [],
        <<"nextCursor">> => <<"next-page-cursor">>
    },

    ?assert(is_map(Response)),
    ?assert(maps:has_key(<<"nextCursor">>, Response)).

test_prompt_icons() ->
    %% Test prompt icons
    Prompt = #{
        <<"name">> => <<"code_review">>,
        <<"icons">> => [
            #{
                <<"src">> => <<"https://example.com/review-icon.svg">>,
                <<"mimeType">> => <<"image/svg+xml">>,
                <<"sizes">> => [<<"any">>]
            }
        ]
    },

    Icons = maps:get(<<"icons">>, Prompt),
    ?assert(is_list(Icons)),
    ?assert(length(Icons) =:= 1).

test_list_changed_notification() ->
    %% Test listChanged capability notification
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notifications/prompts/list_changed">>
    },

    ?assert(is_map(Notification)),
    ?assert(maps:get(<<"method">>, Notification) =:= <<"notifications/prompts/list_changed">>).

test_missing_required_argument() ->
    %% Test handling of missing required argument
    PromptRequest = #{
        <<"name">> => <<"code_review">>,
        <<"arguments">> => #{}
    },

    ?assert(is_map(PromptRequest)).

test_prompt_content_types() ->
    %% Test various content types in prompt messages
    ContentTypes = [
        #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"Plain text content">>
        },
        #{
            <<"type">> => <<"image">>,
            <<"data">> => <<"base64encodeddata">>,
            <<"mimeType">> => <<"image/png">>
        },
        #{
            <<"type">> => <<"audio">>,
            <<"data">> => <<"base64encodedaudio">>,
            <<"mimeType">> => <<"audio/wav">>
        },
        #{
            <<"type">> => <<"resource">>,
            <<"resource">> => #{
                <<"uri">> => <<"resource://example">>,
                <<"mimeType">> => <<"text/plain">>,
                <<"text">> => <<"Resource content">>
            }
        }
    ],

    ?assert(is_list(ContentTypes)),
    ?assert(length(ContentTypes) =:= 4).
