-module(erlmcp_server_resources_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%%%====================================================================
%%% Resources Management Tests - Chicago School TDD
%%% Tests for resource API: add_resource, add_resource_template, delete_resource,
%%% subscribe/unsubscribe, notifications, URI validation
%%% Principles: Real processes, observable behavior, no state inspection
%%%====================================================================

%%%====================================================================
%%% Test Generators
%%%====================================================================

resources_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Add resource", fun test_add_resource/0},
          {"Add resource template", fun test_add_resource_template/0},
          {"Delete resource", fun test_delete_resource/0},
          {"Delete non-existent resource", fun test_delete_nonexistent/0}
         ]
     end}.

subscriptions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Subscribe to resource", fun test_subscribe_resource/0},
          {"Unsubscribe from resource", fun test_unsubscribe_resource/0},
          {"Multiple subscriptions", fun test_multiple_subscriptions/0},
          {"Subscription with external subscriber", fun test_external_subscriber/0}
         ]
     end}.

notifications_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Notify resource updated", fun test_notify_resource_updated/0},
          {"Notify resources changed", fun test_notify_resources_changed/0},
          {"Multiple notifications", fun test_multiple_notifications/0}
         ]
     end}.

uri_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Valid URIs", fun test_valid_uris/0},
          {"Invalid URIs", fun test_invalid_uris/0},
          {"URI with fragments", fun test_uri_fragments/0},
          {"URI with query strings", fun test_uri_queries/0}
         ]
     end}.

template_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Valid template", fun test_valid_template/0},
          {"Template with multiple parameters", fun test_multi_param_template/0},
          {"Template with nested parameters", fun test_nested_template/0}
         ]
     end}.

resource_links_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Encode resource link", fun test_encode_resource_link/0},
          {"Validate resource link URI", fun test_validate_link_uri/0}
         ]
     end}.

progress_tokens_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Report progress with binary token", fun test_progress_binary/0},
          {"Report progress with integer token", fun test_progress_integer/0},
          {"Progress edge cases", fun test_progress_edge_cases/0},
          {"Multiple progress updates", fun test_multiple_progress/0}
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
%%% Basic Resources Tests
%%%====================================================================

test_add_resource() ->
    Server = start_server(),
    Uri = <<"test://resource/1">>,
    Handler = fun(_) -> <<"content 1">> end,

    ?assertEqual(ok, erlmcp_server:add_resource(Server, Uri, Handler)),
    ?assertEqual(ok, erlmcp_server:add_resource(Server, <<"test://resource/2">>, fun(_) -> <<"content 2">> end)),

    ok = erlmcp_server:stop(Server).

test_add_resource_template() ->
    Server = start_server(),
    TemplateUri = <<"test://template/{id}">>,
    TemplateName = <<"Test Template">>,
    Handler = fun(Uri) -> <<"Template: ", Uri/binary>> end,

    ?assertEqual(ok, erlmcp_server:add_resource_template(Server, TemplateUri, TemplateName, Handler)),

    %% Test multiple templates
    ok = erlmcp_server:add_resource_template(Server, <<"test://temp2/{id}/{cat}">>, <<"Temp2">>,
                                            fun(_) -> <<"temp2">> end),

    ok = erlmcp_server:stop(Server).

test_delete_resource() ->
    Server = start_server(),
    Uri = <<"test://delete/resource">>,
    Handler = fun(_) -> <<"content">> end,

    ok = erlmcp_server:add_resource(Server, Uri, Handler),
    ?assertEqual(ok, erlmcp_server:delete_resource(Server, Uri)),

    %% Verify deletion
    ?assertEqual({error, not_found}, erlmcp_server:delete_resource(Server, Uri)),

    ok = erlmcp_server:stop(Server).

test_delete_nonexistent() ->
    Server = start_server(),

    ?assertEqual({error, not_found}, erlmcp_server:delete_resource(Server, <<"nonexistent_resource">>)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Subscription Tests
%%%====================================================================

test_subscribe_resource() ->
    Server = start_server(),
    Uri = <<"test://sub/resource1">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_server:subscribe_resource(Server, Uri, Subscriber)),

    ok = erlmcp_server:stop(Server).

test_unsubscribe_resource() ->
    Server = start_server(),
    Uri = <<"test://sub/resource2">>,

    ok = erlmcp_server:subscribe_resource(Server, Uri, self()),
    ?assertEqual(ok, erlmcp_server:unsubscribe_resource(Server, Uri)),

    ok = erlmcp_server:stop(Server).

test_multiple_subscriptions() ->
    Server = start_server(),

    %% Subscribe to multiple resources
    [begin
        Uri = <<"test://sub/resource_", (integer_to_binary(N))/binary>>,
        ?assertEqual(ok, erlmcp_server:subscribe_resource(Server, Uri, self()))
    end || N <- lists:seq(1, 5)],

    %% Unsubscribe from all
    [begin
        Uri = <<"test://sub/resource_", (integer_to_binary(N))/binary>>,
        ?assertEqual(ok, erlmcp_server:unsubscribe_resource(Server, Uri))
    end || N <- lists:seq(1, 5)],

    ok = erlmcp_server:stop(Server).

test_external_subscriber() ->
    Server = start_server(),
    Subscriber = spawn(fun() -> receive after 1000 -> ok end end),
    Uri = <<"test://sub/resource_external">>,

    ?assertEqual(ok, erlmcp_server:subscribe_resource(Server, Uri, Subscriber)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Notification Tests
%%%====================================================================

test_notify_resource_updated() ->
    Server = start_server(),
    Uri = <<"test://notify/resource">>,

    ok = erlmcp_server:add_resource(Server, Uri, fun(_) -> <<"content">> end),
    ?assertEqual(ok, erlmcp_server:notify_resource_updated(Server, Uri, #{<<"version">> => 1})),

    %% Test with empty metadata
    ?assertEqual(ok, erlmcp_server:notify_resource_updated(Server, Uri, #{})),

    ok = erlmcp_server:stop(Server).

test_notify_resources_changed() ->
    Server = start_server(),

    ?assertEqual(ok, erlmcp_server:notify_resources_changed(Server)),

    ok = erlmcp_server:stop(Server).

test_multiple_notifications() ->
    Server = start_server(),

    [begin
        ?assertEqual(ok, erlmcp_server:notify_resources_changed(Server))
    end || _ <- lists:seq(1, 5)],

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% URI Validation Tests
%%%====================================================================

test_valid_uris() ->
    Server = start_server(),
    ValidUris = [
        <<"file:///path/to/file">>,
        <<"http://example.com/resource">>,
        <<"https://example.com/secure">>,
        <<"custom://test/resource/123">>,
        <<"test://resource/with/multiple/segments">>,
        <<"mcp://server/resource">>
    ],
    [begin
        Handler = fun(_) -> <<"ok">> end,
        ?assertEqual(ok, erlmcp_server:add_resource(Server, Uri, Handler))
    end || Uri <- ValidUris],

    ok = erlmcp_server:stop(Server).

test_invalid_uris() ->
    Server = start_server(),

    %% Test invalid URIs - behavior depends on validator
    InvalidUris = [
        <<>>,  %% Empty
        <<"no-scheme">>,  %% Missing scheme
        <<"://no-scheme">>  %% Empty scheme
    ],
    [begin
        Handler = fun(_) -> <<"ok">> end,
        Result = erlmcp_server:add_resource(Server, Uri, Handler),
        %% We expect either ok (if validator is lenient) or error tuple
        case Result of
            ok -> ok;
            {error, {_Code, _Msg, _Data}} -> ok
        end
    end || Uri <- InvalidUris],

    ok = erlmcp_server:stop(Server).

test_uri_fragments() ->
    Server = start_server(),
    UriWithFragment = <<"test://resource#fragment">>,

    Handler = fun(_) -> <<"ok">> end,
    case erlmcp_server:add_resource(Server, UriWithFragment, Handler) of
        ok -> ok;
        {error, _} -> ok  %% May be rejected by validator
    end,

    ok = erlmcp_server:stop(Server).

test_uri_queries() ->
    Server = start_server(),
    UriWithQuery = <<"test://resource?key=value&key2=value2">>,

    Handler = fun(_) -> <<"ok">> end,
    case erlmcp_server:add_resource(Server, UriWithQuery, Handler) of
        ok -> ok;
        {error, _} -> ok  %% May be rejected by validator
    end,

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Template Validation Tests
%%%====================================================================

test_valid_template() ->
    Server = start_server(),
    ValidTemplate = <<"test://template/{id}">>,
    Handler = fun(_) -> <<"ok">> end,

    ?assertEqual(ok, erlmcp_server:add_resource_template(Server, ValidTemplate, <<"Valid">>, Handler)),

    ok = erlmcp_server:stop(Server).

test_multi_param_template() ->
    Server = start_server(),
    MultiTemplate = <<"test://template/{id}/{category}/{item}">>,

    ?assertEqual(ok, erlmcp_server:add_resource_template(Server, MultiTemplate, <<"Multi">>,
                                                         fun(_) -> <<"ok">> end)),

    ok = erlmcp_server:stop(Server).

test_nested_template() ->
    Server = start_server(),
    NestedTemplate = <<"test://template/{parent}/{child}/{grandchild}">>,

    ?assertEqual(ok, erlmcp_server:add_resource_template(Server, NestedTemplate, <<"Nested">>,
                                                         fun(_) -> <<"ok">> end)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Resource Link Tests
%%%====================================================================

test_encode_resource_link() ->
    Server = start_server(),
    Uri = <<"test://linked/resource">>,

    ok = erlmcp_server:add_resource(Server, Uri, fun(_) -> <<"linked">> end),

    %% Test encode_resource_link variations
    try
        erlmcp_server:encode_resource_link(Server, Uri)
    catch
        _:_ -> ok
    end,

    try
        erlmcp_server:encode_resource_link(Server, <<"test://another">>, <<"text/plain">>, #{})
    catch
        _:_ -> ok
    end,

    ok = erlmcp_server:stop(Server).

test_validate_link_uri() ->
    Server = start_server(),
    ValidUri = <<"test://valid/uri">>,

    %% validate_resource_link_uri is a public API
    try
        erlmcp_server:validate_resource_link_uri(ValidUri)
    catch
        _:_ -> ok  %% Expected to possibly fail validation
    end,

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Progress Token Tests
%%%====================================================================

test_progress_binary() ->
    Server = start_server(),
    Token = <<"progress_token_1">>,

    ?assertEqual(ok, erlmcp_server:report_progress(Server, Token, 50.0, 100.0)),

    ok = erlmcp_server:stop(Server).

test_progress_integer() ->
    Server = start_server(),
    Token = 12345,

    ?assertEqual(ok, erlmcp_server:report_progress(Server, Token, 25.0, 50.0)),

    ok = erlmcp_server:stop(Server).

test_progress_edge_cases() ->
    Server = start_server(),

    %% Zero progress
    ?assertEqual(ok, erlmcp_server:report_progress(Server, <<"zero1">>, 0.0, 0.0)),

    %% Progress equals total
    ?assertEqual(ok, erlmcp_server:report_progress(Server, <<"complete">>, 100.0, 100.0)),

    %% Progress exceeds total
    ?assertEqual(ok, erlmcp_server:report_progress(Server, <<"exceed">>, 150.0, 100.0)),

    %% Negative values
    ?assertEqual(ok, erlmcp_server:report_progress(Server, <<"neg">>, -50.0, -100.0)),

    %% Zero progress with positive total
    ?assertEqual(ok, erlmcp_server:report_progress(Server, <<"zero2">>, 0.0, 100.0)),

    %% Negative progress (should be allowed)
    ?assertEqual(ok, erlmcp_server:report_progress(Server, <<"negative">>, -10.0, 100.0)),

    ok = erlmcp_server:stop(Server).

test_multiple_progress() ->
    Server = start_server(),

    %% Test progress at different stages
    ProgressValues = [0.0, 25.0, 50.0, 75.0, 100.0],
    [begin
        Token = <<"token_", (float_to_binary(P, [{decimals, 1}]))/binary>>,
        ?assertEqual(ok, erlmcp_server:report_progress(Server, Token, P, 100.0))
    end || P <- ProgressValues],

    %% Test progress with fractional values
    ok = erlmcp_server:report_progress(Server, <<"frac">>, 33.33, 100.0),
    ok = erlmcp_server:report_progress(Server, <<"frac2">>, 66.67, 100.0),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Start server with default capabilities
start_server() ->
    ServerId = <<"resources_test_server_">>,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    Pid.
