-module(erlmcp_elicitation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_elicitation:start_link(),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

elicitation_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_create_form()),
            ?_test(test_create_url_elicitation()),
            ?_test(test_get_elicitation()),
            ?_test(test_submit_response()),
            ?_test(test_cancel_elicitation()),
            ?_test(test_form_expiry()),
            ?_test(test_concurrent_elicitations()),
            ?_test(test_invalid_elicitation_id())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_create_form() ->
    ServerPid = self(),
    Title = <<"Personal Information">>,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>},
            <<"email">> => #{<<"type">> => <<"string">>, <<"format">> => <<"email">>}
        }
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
    ?assert(is_binary(ElicitationId)),
    ?assert(byte_size(ElicitationId) > 0).

test_create_url_elicitation() ->
    ServerPid = self(),
    Title = <<"Approve Action">>,
    Url = <<"https://example.com/approve">>,

    {ok, ElicitationId} = erlmcp_elicitation:create_url_elicitation(ServerPid, Title, Url),
    ?assert(is_binary(ElicitationId)).

test_get_elicitation() ->
    ServerPid = self(),
    Title = <<"Test Form">>,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"answer">> => #{<<"type">> => <<"string">>}
        }
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),

    ?assert(is_map(Elicitation)),
    ?assert(maps:get(<<"id">>, Elicitation) =:= ElicitationId),
    ?assert(maps:get(<<"title">>, Elicitation) =:= Title),
    ?assert(maps:get(<<"type">>, Elicitation) =:= <<"form">>).

test_submit_response() ->
    ServerPid = self(),
    Title = <<"Test Form">>,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"answer">> => #{<<"type">> => <<"string">>}
        }
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),

    Response = #{<<"answer">> => <<"test response">>},
    {ok, SubmittedId} = erlmcp_elicitation:submit_response(ElicitationId, Response),

    ?assert(SubmittedId =:= ElicitationId),

    {ok, UpdatedElicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),
    ?assert(maps:get(<<"status">>, UpdatedElicitation) =:= <<"completed">>).

test_cancel_elicitation() ->
    ServerPid = self(),
    Title = <<"Test Form">>,
    Schema = #{
        <<"type">> => <<"object">>
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
    {ok, CancelledId} = erlmcp_elicitation:cancel_elicitation(ElicitationId),

    ?assert(CancelledId =:= ElicitationId),

    {ok, UpdatedElicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),
    ?assert(maps:get(<<"status">>, UpdatedElicitation) =:= <<"cancelled">>).

test_form_expiry() ->
    ServerPid = self(),
    Title = <<"Test Form">>,
    Schema = #{
        <<"type">> => <<"object">>
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),

    CreatedAt = maps:get(<<"createdAt">>, Elicitation),
    ExpiresAt = maps:get(<<"expiresAt">>, Elicitation),

    ?assert(ExpiresAt > CreatedAt),
    ?assert(ExpiresAt - CreatedAt =:= 600000).  %% 10 minutes

test_concurrent_elicitations() ->
    ServerPid = self(),
    Title = <<"Concurrent Test">>,
    Schema = #{<<"type">> => <<"object">>},

    Ids = lists:map(
        fun(_) ->
            {ok, Id} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
            Id
        end,
        lists:seq(1, 5)
    ),

    ?assert(length(Ids) =:= 5),
    ?assert(length(lists:usort(Ids)) =:= 5).  %% All IDs unique

test_invalid_elicitation_id() ->
    InvalidId = <<"invalid_id_does_not_exist">>,
    {error, not_found} = erlmcp_elicitation:get_elicitation(InvalidId),

    ?assert(true).
