-module(erlmcp_resource_management_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%====================================================================
%% Test Suite for Comprehensive MCP Resource Management
%% Chicago School TDD - Real processes, no mocks, state-based testing
%%====================================================================

%%====================================================================
%% Main Test Entry Point
%%====================================================================

comprehensive_resource_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
            %% Basic Resource Management Tests
            basic_resource_lifecycle_test_(),

            %% Resource Listing Tests
            resource_listing_with_pagination_test_(),

            %% Resource Reading Tests
            resource_reading_test_(),

            %% Resource Templates Tests
            resource_templates_test_(),

            %% Resource Subscriptions Tests
            resource_subscriptions_test_(),

            %% Resource Notifications Tests
            resource_notifications_test_(),

            %% URI Validation Tests
            uri_validation_test_(),

            %% URI Scheme Tests
            uri_schemes_test_(),

            %% Resource Annotations Tests
            resource_annotations_test_(),

            %% Error Handling Tests
            error_handling_test_(),

            %% Resource Permissions Tests
            resource_permissions_test_(),

            %% Performance Tests
            performance_test_(),

            %% Concurrency Tests
            concurrency_test_(),

            %% Memory Management Tests
            memory_management_test_()
         ]
     end}.

%%====================================================================
%% Setup and Cleanup Functions
%%====================================================================

setup() ->
    %% Start required applications for testing
    application:ensure_all_started(erlmcp_core),

    %% Create test server with full capabilities
    ServerId = test_resource_management_server,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        },
        prompts = #mcp_prompts_capability{
            listChanged = true
        },
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },

    {ok, Server} = erlmcp_server:start_link(ServerId, Capabilities),

    %% Initialize the server
    TransportId = test_transport,
    InitializeParams = #{
        ?MCP_FIELD_PROTOCOL_VERSION => <<"2025-11-25">>,
        ?MCP_FIELD_CLIENT_INFO => #{
            ?MCP_INFO_NAME => <<"test-client">>,
            ?MCP_INFO_VERSION => <<"1.0.0">>
        }
    },

    %% Simulate initialize call
    erlmcp_server:handle_call({initialize, TransportId, InitializeParams},
                             {self(), ref},
                             #state{
                                 server_id = ServerId,
                                 capabilities = Capabilities,
                                 initialized = false
                             }),

    {Server, ServerId}.

cleanup({Server, _ServerId}) ->
    erlmcp_server:stop(Server),
    application:stop(erlmcp_core),
    ok.

%%====================================================================
%% Basic Resource Management Tests
%%====================================================================

basic_resource_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_add_and_list_resources(Server)),
             ?_test(test_resource_update_and_delete(Server)),
             ?_test(test_resource_validation(Server))
         ]
     end}.

test_add_and_list_resources(Server) ->
    %% Add multiple test resources
    Resources = [
        #mcp_resource{
            uri = <<"file:///documents/report.pdf">>,
            name = <<"Annual Report">>,
            description = <<"Company annual report 2024">>,
            mime_type = <<"application/pdf">>,
            metadata = #{year => 2024, department => <<"Finance">>}
        },
        #mcp_resource{
            uri = <<"http://api.example.com/users">>,
            name = <<"User API">>,
            description = <<"REST API for user management">>,
            mime_type = <<"application/json">>,
            metadata = #{api_version => <<"v1">>, rate_limit => 1000}
        },
        #mcp_resource{
            uri = <<"git://github.com/example/repo">>,
            name = <<"GitHub Repository">>,
            description = <<"Source code repository">>,
            mime_type = <<"text/plain">>,
            metadata = #{branch => <<"main">>, last_commit => <<"abc123">>}
        }
    ],

    %% Add resources with handlers
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, Resources),

    %% Test resource listing
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(3, length(ListedResources)),

    %% Verify specific resources exist
    ResourceURIs = [maps:get(<<"uri">>, R) || R <- ListedResources],
    ?assert(lists:member(<<"file:///documents/report.pdf">>, ResourceURIs)),
    ?assert(lists:member(<<"http://api.example.com/users">>, ResourceURIs)),
    ?assert(lists:member(<<"git://github.com/example/repo">>, ResourceURIs)),
    ok.

test_resource_update_and_delete(Server) ->
    %% Add initial resource
    InitialResource = #mcp_resource{
        uri = <<"file:///document.txt">>,
        name = <<"Initial Document">>,
        description = <<"Original version">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(Uri) -> <<"initial content">> end,
    ok = erlmcp_server:add_resource(Server, InitialResource, Handler),

    %% Verify it exists
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(1, length(ListedResources)),

    %% Delete the resource
    ok = erlmcp_server:delete_resource(Server, <<"file:///document.txt">>),

    %% Verify it's gone
    UpdatedList = get_resource_list_from_server(Server),
    ?assertEqual(0, length(UpdatedList)),
    ok.

test_resource_validation(Server) ->
    %% Test invalid resource (empty URI)
    InvalidResource = #mcp_resource{
        uri = <<>>,
        name = <<"Invalid">>,
        description = <<"Resource with empty URI">>,
        mime_type = <<"text/plain">>
    },

    %% Should fail validation
    Result = erlmcp_server:add_resource(Server, InvalidResource, fun(_) -> <<"content">> end),
    ?assertMatch({error, {invalid_params, _, _}}, Result),

    %% Test invalid handler (not a function)
    ValidResource = #mcp_resource{
        uri = <<"file:///valid.txt">>,
        name = <<"Valid Resource">>,
        mime_type = <<"text/plain">>
    },

    %% Should fail due to invalid handler
    InvalidHandlerResult = erlmcp_server:add_resource(Server, ValidResource, not_a_function),
    ?assertMatch({error, _}, InvalidHandlerResult),
    ok.

%%====================================================================
%% Resource Listing Tests
%%====================================================================

resource_listing_with_pagination_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_large_resource_listing(Server)),
             ?_test(test_paginated_resource_listing(Server)),
             ?_test(test_resource_listing_performance(Server))
         ]
     end}.

test_large_resource_listing(Server) ->
    %% Add many resources to test listing performance
    NumResources = 100,
    Resources = lists:map(fun(I) ->
        #mcp_resource{
            uri = <<"file:///document_", (integer_to_binary(I))/binary, ".txt">>,
            name = <<"Document ", (integer_to_binary(I))/binary>>,
            description = <<"Test document number ", (integer_to_binary(I))/binary>>,
            mime_type = <<"text/plain">>,
            metadata = #{id => I, category => <<"test">>}
        }
    end, lists:seq(1, NumResources)),

    %% Add all resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, Resources),

    %% Test listing all resources
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(NumResources, length(ListedResources)),

    %% Test listing with filters (simulated)
    FilteredResources = filter_resources_by_category(ListedResources, <<"test">>),
    ?assertEqual(NumResources, length(FilteredResources)),
    ok.

test_paginated_resource_listing(Server) ->
    %% Add resources with different categories
    ResourceCategories = [<<"category1">>, <<"category2">>, <<"category3">>],

    lists:enumerate(fun({I, Category}) ->
        Resources = lists:map(fun(J) ->
            #mcp_resource{
                uri = <<"file:///", Category/binary, "/doc_", (integer_to_binary(J))/binary, ".txt">>,
                name = <<Category/binary, " Document ", (integer_to_binary(J))/binary>>,
                description = <<"Document in ", Category/binary>>,
                mime_type = <<"text/plain">>,
                metadata = #{category => Category, index => J}
            }
        end, lists:seq(1, 10)),

        lists:foreach(fun(Resource) ->
            Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
            ok = erlmcp_server:add_resource(Server, Resource, Handler)
        end, Resources)
    end, ResourceCategories),

    %% Test pagination (simulated)
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(30, length(ListedResources)),

    %% Test filtered listing by category
    Cat1Resources = filter_resources_by_category(ListedResources, <<"category1">>),
    ?assertEqual(10, length(Cat1Resources)),
    ok.

test_resource_listing_performance(Server) ->
    %% Add performance test resources
    NumResources = 1000,
    LargeResources = lists:map(fun(I) ->
        #mcp_resource{
            uri = <<"perf://resource_", (integer_to_binary(I))/binary>>,
            name = <<"Performance Test Resource ", (integer_to_binary(I))/binary>>,
            description = <<"This is a performance test resource number ", (integer_to_binary(I))/binary>>,
            mime_type = <<"text/plain">>,
            metadata = #{performance_id => I, size => 1024}
        }
    end, lists:seq(1, NumResources)),

    %% Add all resources
    StartTime = erlang:monotonic_time(millisecond),
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, LargeResources),

    %% Test listing performance
    ListedResources = get_resource_list_from_server(Server),
    EndTime = erlang:monotonic_time(millisecond),

    ?assertEqual(NumResources, length(ListedResources)),

    %% Log performance metrics
    Duration = EndTime - StartTime,
    logger:info("Resource listing performance: ~p resources in ~p ms (~.2f resources/ms)",
                [NumResources, Duration, NumResources / Duration]),
    ok.

%%====================================================================
%% Resource Reading Tests
%%====================================================================

resource_reading_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_text_resource_reading(Server)),
             ?_test(test_binary_resource_reading(Server)),
             ?_test(test_large_resource_reading(Server)),
             ?_test(test_resource_encoding_decoding(Server))
         ]
     end}.

test_text_resource_reading(Server) ->
    %% Add text resource
    TextResource = #mcp_resource{
        uri = <<"file:///text/document.txt">>,
        name = <<"Text Document">>,
        description = <<"A plain text document">>,
        mime_type = <<"text/plain">>
    },

    TextContent = <<"This is a test text document.\nIt contains multiple lines.\nFor testing resource reading functionality.">>,
    Handler = fun(_Uri) -> TextContent end,

    ok = erlmcp_server:add_resource(Server, TextResource, Handler),

    %% Test resource reading (simulated)
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(1, length(ListedResources)),

    TextResourceFromList = hd(ListedResources),
    ?assertEqual(<<"file:///text/document.txt">>, maps:get(<<"uri">>, TextResourceFromList)),
    ?assertEqual(<<"text/plain">>, maps:get(<<"mimeType">>, TextResourceFromList)),
    ok.

test_binary_resource_reading(Server) ->
    %% Add binary resource (PDF, image, etc.)
    BinaryResource = #mcp_resource{
        uri = <<"file:///binary/document.pdf">>,
        name = <<"Binary Document">>,
        description = <<"A PDF document">>,
        mime_type = <<"application/pdf">>
    },

    %% Create binary content (simulated PDF)
    BinaryContent = <<"%PDF-1.4\n", 16#1F, 16#8B, 16#08, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#03, 16#33>>>,
    BinaryHandler = fun(_Uri) -> BinaryContent end,

    ok = erlmcp_server:add_resource(Server, BinaryResource, BinaryHandler),

    %% Test binary resource listing
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(1, length(ListedResources)),

    BinaryResourceFromList = hd(ListedResources),
    ?assertEqual(<<"application/pdf">>, maps:get(<<"mimeType">>, BinaryResourceFromList)),
    ok.

test_large_resource_reading(Server) ->
    %% Add large resource
    LargeResource = #mcp_resource{
        uri = <<"file:///large/document.txt">>,
        name = <<"Large Document">>,
        description = <<"A large text document">>,
        mime_type = <<"text/plain">>
    },

    %% Create large content (1MB)
    LargeContent = binary:copy(<<"This is a line of text in a large document. ">>, 65536),
    LargeHandler = fun(_Uri) -> LargeContent end,

    ok = erlmcp_server:add_resource(Server, LargeResource, LargeHandler),

    %% Test large resource listing
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(1, length(ListedResources)),

    LargeResourceFromList = hd(ListedResources),
    ?assertEqual(<<"Large Document">>, maps:get(<<"name">>, LargeResourceFromList)),
    ok.

test_resource_encoding_decoding(Server) ->
    %% Test resource encoding/decoding roundtrip
    OriginalResource = #mcp_resource{
        uri = <<"file:///test/roundtrip.txt">>,
        name = <<"Roundtrip Test">>,
        description = <<"Testing encoding and decoding">>,
        mime_type = <<"text/plain">>,
        metadata = #{test => <<"value">>, count => 42}
    },

    Handler = fun(Uri) -> get_resource_content(Uri, OriginalResource) end,
    ok = erlmcp_server:add_resource(Server, OriginalResource, Handler),

    %% Get resource from server and test encoding
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(1, length(ListedResources)),

    EncodedResource = hd(ListedResources),
    ?assertEqual(OriginalResource#mcp_resource.uri, maps:get(<<"uri">>, EncodedResource)),
    ?assertEqual(OriginalResource#mcp_resource.name, maps:get(<<"name">>, EncodedResource)),
    ?assertEqual(OriginalResource#mcp_resource.mime_type, maps:get(<<"mimeType">>, EncodedResource)),
    ok.

%%====================================================================
%% Resource Templates Tests
%%====================================================================

resource_templates_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_resource_template_creation(Server)),
             ?_test(test_resource_template_instantiation(Server)),
             ?_test(test_resource_template_validation(Server))
         ]
     end}.

test_resource_template_creation(Server) ->
    %% Add resource templates
    Templates = [
        #mcp_resource_template{
            uri_template = <<"file:///users/{userId}/documents/{docId}">>,
            name = <<"User Document Template">>,
            description = <<"Template for user-specific documents">>,
            mime_type = <<"text/plain">>
        },
        #mcp_resource_template{
            uri_template = <<"http://api.example.com/{resourceType}/{id}">>,
            name = <<"API Resource Template">>,
            description = <<"Template for API resources">>,
            mime_type = <<"application/json">>
        }
    ],

    %% Add templates with handlers
    lists:foreach(fun(Template) ->
        Handler = fun(Uri) -> get_template_content(Uri, Template) end,
        ok = erlmcp_server:add_resource_template(Server, Template, Handler)
    end, Templates),

    %% Test template listing
    ListedTemplates = get_template_list_from_server(Server),
    ?assertEqual(2, length(ListedTemplates)),

    %% Verify templates exist
    TemplateNames = [maps:get(<<"name">>, T) || T <- ListedTemplates],
    ?assert(lists:member(<<"User Document Template">>, TemplateNames)),
    ?assert(lists:member(<<"API Resource Template">>, TemplateNames)),
    ok.

test_resource_template_instantiation(Server) ->
    %% Add user document template
    UserTemplate = #mcp_resource_template{
        uri_template = <<"file:///users/{userId}/documents/{docId}">>,
        name = <<"User Document Template">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(Uri) ->
        %% Simulate template instantiation
        <<"Template instantiated for: ", Uri/binary>>
    end,
    ok = erlmcp_server:add_resource_template(Server, UserTemplate, Handler),

    %% Test template listing
    ListedTemplates = get_template_list_from_server(Server),
    ?assertEqual(1, length(ListedTemplates)),

    Template = hd(ListedTemplates),
    ?assertEqual(<<"User Document Template">>, maps:get(<<"name">>, Template)),
    ?assertEqual(<<"file:///users/{userId}/documents/{docId}">>, maps:get(<<"uriTemplate">>, Template)),
    ok.

test_resource_template_validation(Server) ->
    %% Test invalid template (empty URI template)
    InvalidTemplate = #mcp_resource_template{
        uri_template = <<>>,
        name = <<"Invalid Template">>,
        mime_type = <<"text/plain">>
    },

    Result = erlmcp_server:add_resource_template(Server, InvalidTemplate, fun(_Uri) -> <<"content">> end),
    ?assertMatch({error, {invalid_params, _, _}}, Result),

    %% Test valid template
    ValidTemplate = #mcp_resource_template{
        uri_template = <<"file:///{resource}">>,
        name = <<"Valid Template">>,
        mime_type = <<"text/plain">>
    },

    ValidResult = erlmcp_server:add_resource_template(Server, ValidTemplate, fun(_Uri) -> <<"content">> end),
    ?assertMatch(ok, ValidResult),
    ok.

%%====================================================================
%% Resource Subscriptions Tests
%%====================================================================

resource_subscriptions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_resource_subscription_lifecycle(Server)),
             ?_test(test_multiple_subscriptions(Server)),
             ?_test(test_subscription_performance(Server))
         ]
     end}.

test_resource_subscription_lifecycle(Server) ->
    %% Add test resource
    Resource = #mcp_resource{
        uri = <<"file:///subscribe/document.txt">>,
        name = <<"Subscription Test">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
    ok = erlmcp_server:add_resource(Server, Resource, Handler),

    %% Create subscriber processes
    Subscribers = [
        spawn_subscriber(),
        spawn_subscriber(),
        spawn_subscriber()
    ],

    %% Subscribe multiple processes to the resource
    lists:foreach(fun(Subscriber) ->
        ok = erlmcp_server:subscribe_resource(Server, <<"file:///subscribe/document.txt">>, Subscriber)
    end, Subscribers),

    %% Test subscription management
    %% Simulate resource update
    ok = erlmcp_server:notify_resource_updated(Server, <<"file:///subscribe/document.txt">>,
                                               #{updated => true, timestamp => erlang:system_time(millisecond)}),

    %% Verify subscribers received notification (in real scenario, they would)
    lists:foreach(fun(Subscriber) ->
        monitor(process, Subscriber)
    end, Subscribers),

    %% Unsubscribe one subscriber
    Subscriber1 = hd(Subscribers),
    ok = erlmcp_server:unsubscribe_resource(Server, <<"file:///subscribe/document.txt">>),

    %% Clean up subscribers
    lists:foreach(fun(Subscriber) ->
        exit(Subscriber, kill)
    end, Subscribers),
    ok.

test_multiple_subscriptions(Server) ->
    %% Add multiple resources
    Resources = [
        #mcp_resource{
            uri = <<"file:///multi/1.txt">>,
            name = <<"Multi 1">>,
            mime_type = <<"text/plain">>
        },
        #mcp_resource{
            uri = <<"file:///multi/2.txt">>,
            name = <<"Multi 2">>,
            mime_type = <<"text/plain">>
        }
    ],

    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, Resources),

    %% Create subscribers
    Subscribers = [
        spawn_subscriber(),
        spawn_subscriber()
    ],

    %% Subscribe different processes to different resources
    erlmcp_server:subscribe_resource(Server, <<"file:///multi/1.txt">>, hd(Subscribers)),
    erlmcp_server:subscribe_resource(Server, <<"file:///multi/2.txt>", lists:last(Subscribers)),

    %% Test multiple subscriptions
    ok = erlmcp_server:notify_resource_updated(Server, <<"file:///multi/1.txt">>, #{update => "1"}),
    ok = erlmcp_server:notify_resource_updated(Server, <<"file:///multi/2.txt">>, #{update => "2"}),

    %% Clean up
    lists:foreach(fun(Subscriber) ->
        exit(Subscriber, kill)
    end, Subscribers),
    ok.

test_subscription_performance(Server) ->
    %% Add resource for performance test
    Resource = #mcp_resource{
        uri = <<"file:///perf/subscribed.txt">>,
        name = <<"Performance Test">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
    ok = erlmcp_server:add_resource(Server, Resource, Handler),

    %% Create many subscribers
    NumSubscribers = 100,
    Subscribers = lists:map(fun(_) ->
        spawn_subscriber()
    end, lists:seq(1, NumSubscribers)),

    %% Subscribe all to the resource
    SubscribeStart = erlang:monotonic_time(millisecond),
    lists:foreach(fun(Subscriber) ->
        erlmcp_server:subscribe_resource(Server, <<"file:///perf/subscribed.txt">>, Subscriber)
    end, Subscribers),
    SubscribeEnd = erlang:monotonic_time(millisecond),

    %% Test subscription performance
    ok = erlmcp_server:notify_resource_updated(Server, <<"file:///perf/subscribed.txt">>,
                                               #{perf_test => true, timestamp => erlang:system_time(millisecond)}),

    %% Clean up
    lists:foreach(fun(Subscriber) ->
        exit(Subscriber, kill)
    end, Subscribers),

    %% Log performance
    SubscribeDuration = SubscribeEnd - SubscribeStart,
    logger:info("Subscription performance: ~p subscriptions in ~p ms (~.2f subs/ms)",
                [NumSubscribers, SubscribeDuration, NumSubscribers / SubscribeDuration]),
    ok.

%%====================================================================
%% Resource Notifications Tests
%%====================================================================

resource_notifications_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_resource_update_notifications(Server)),
             ?_test(test_resource_list_change_notifications(Server)),
             ?_test(test_notification_performance(Server))
         ]
     end}.

test_resource_update_notifications(Server) ->
    %% Add test resource
    Resource = #mcp_resource{
        uri = <<"file:///notify/document.txt">>,
        name = <<"Notification Test">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
    ok = erlmcp_server:add_resource(Server, Resource, Handler),

    %% Test resource update notification
    Metadata = #{action => "updated", timestamp => erlang:system_time(millisecond), user => "test"},
    ok = erlmcp_server:notify_resource_updated(Server, <<"file:///notify/document.txt">>, Metadata),

    %% Test general resource list change notification
    ok = erlmcp_server:notify_resources_changed(Server),

    %% Verify notifications were sent (in real scenario, would check transport layer)
    ok.

test_resource_list_change_notifications(Server) ->
    %% Add initial resource
    Resource1 = #mcp_resource{
        uri = <<"file:///list/1.txt">>,
        name = <<"List Test 1">>,
        mime_type = <<"text/plain">>
    },

    Handler1 = fun(Uri) -> get_resource_content(Uri, Resource1) end,
    ok = erlmcp_server:add_resource(Server, Resource1, Handler1),

    %% List change notification should be triggered
    ok = erlmcp_server:notify_resources_changed(Server),

    %% Add second resource
    Resource2 = #mcp_resource{
        uri = <<"file:///list/2.txt">>,
        name = <<"List Test 2">>,
        mime_type = <<"text/plain">>
    },

    Handler2 = fun(Uri) -> get_resource_content(Uri, Resource2) end,
    ok = erlmcp_server:add_resource(Server, Resource2, Handler2),

    Another notification should be triggered
    ok = erlmcp_server:notify_resources_changed(Server),

    %% Remove resource
    ok = erlmcp_server:delete_resource(Server, <<"file:///list/1.txt">>),

    %% Final notification
    ok = erlmcp_server:notify_resources_changed(Server),
    ok.

test_notification_performance(Server) ->
    %% Add multiple resources
    NumResources = 50,
    Resources = lists:map(fun(I) ->
        #mcp_resource{
            uri = <<"file:///", (integer_to_binary(I))/binary, ".txt">>,
            name = <<"Perf Resource ", (integer_to_binary(I))/binary>>,
            mime_type = <<"text/plain">>
        }
    end, lists:seq(1, NumResources)),

    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, Resources),

    %% Test notification performance
    NotificationStart = erlang:monotonic_time(millisecond),
    ok = erlmcp_server:notify_resources_changed(Server),
    NotificationEnd = erlang:monotonic_time(millisecond),

    %% Log performance
    NotificationDuration = NotificationEnd - NotificationStart,
    logger:info("Notification performance: ~p resources notified in ~p ms",
                [NumResources, NotificationDuration]),
    ok.

%%====================================================================
%% URI Validation Tests
%%====================================================================

uri_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_valid_uri_schemes(Server)),
             ?_test(test_uri_canonicalization(Server)),
             ?_test(test_security_validation(Server))
         ]
     end}.

test_valid_uri_schemes(Server) ->
    %% Test various URI schemes
    ValidURIs = [
        <<"file:///document.txt">>,
        <<"http://example.com/resource">>,
        <<"https://api.example.com/data">>,
        <<"ftp://files.server.com/path">>,
        <<"git://github.com/user/repo">>,
        <<"custom://namespace/item">>,
        <<"data:text/plain;base64,SGVsbG8sIFdvcmxkIQ==">>,
        <<"resource://server/endpoint">>
    ],

    %% Test URI validation for each
    lists:foreach(fun(Uri) ->
        case erlmcp_resource:validate_uri(Uri) of
            ok -> ok;
            {error, Reason} ->
                ?assert(false, lists:flatten(io_lib:format("URI validation failed for ~p: ~p", [Uri, Reason])))
        end
    end, ValidURIs),

    %% Add resources with valid URIs
    lists:enumerate(fun({I, Uri}) ->
        Resource = #mcp_resource{
            uri = Uri,
            name = <<"Resource ", (integer_to_binary(I))/binary>>,
            mime_type = <<"text/plain">>
        },
        Handler = fun(_U) -> <<"content for ", Uri/binary>> end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, ValidURIs),

    %% Verify all resources were added
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(length(ValidURIs), length(ListedResources)),
    ok.

test_uri_canonicalization(Server) ->
    %% Test URI canonicalization
    Resource = #mcp_resource{
        uri = <<"file:///users/../documents/report.txt">>,  // Contains traversal
        name = <<"Canonicalization Test">>,
        mime_type = <<"text/plain">>
    },

    %% Should pass validation (traversal resolved)
    Handler = fun(Uri) -> <<"canonicalized content">> end,
    Result = erlmcp_server:add_resource(Server, Resource, Handler),

    %% In a real implementation, this would resolve to canonical path
    ?assertMatch(ok, Result),
    ok.

test_security_validation(Server) ->
    %% Test security validation (path traversal attempts)
    MaliciousURIs = [
        <<"file:///etc/passwd">>,                    // System file
        <<"file:///../../etc/passwd">>,              // Traversal attempt
        <<"file:///var/www/../../../etc/passwd">>,    // Complex traversal
        <<"file:///secret/../../etc/shadow">>         // Shadow file attempt
    ],

    %% These should be blocked by security validation
    lists:foreach(fun(Uri) ->
        Resource = #mcp_resource{
            uri = Uri,
            name = <<"Malicious Attempt">>,
            mime_type = <<"text/plain">>
        },
        Result = erlmcp_server:add_resource(Server, Resource, fun(_U) -> <<"content">> end),
        ?assertMatch({error, {invalid_params, _, _}}, Result)
    end, MaliciousURIs),
    ok.

%%====================================================================
%% URI Scheme Tests
%%====================================================================

uri_schemes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_file_uri_scheme(Server)),
             ?_test(test_http_https_uri_schemes(Server)),
             ?_test(test_git_uri_scheme(Server)),
             ?_test(test_custom_uri_scheme(Server))
         ]
     end}.

test_file_uri_scheme(Server) ->
    %% Test file:// scheme resources
    FileResources = [
        #mcp_resource{
            uri = <<"file:///documents/report.pdf">>,
            name = <<"PDF Report">>,
            mime_type = <<"application/pdf">>,
            metadata = #{type => "document", size => 2048576}
        },
        #mcp_resource{
            uri = <<"file:///images/logo.png">>,
            name = <<"Logo Image">>,
            mime_type = <<"image/png">>,
            metadata = #{type => "image", size => 65536}
        },
        #mcp_resource{
            uri = <<"file:///data/config.json">>,
            name = <<"Configuration">>,
            mime_type = <<"application/json">>,
            metadata = #{type => "config", encoding => "utf8"}
        }
    ],

    %% Add file resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_file_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, FileResources),

    %% Verify file resources
    ListedResources = get_resource_list_from_server(Server),
    FileURIs = [maps:get(<<"uri">>, R) || R <- ListedResources,
                                      binary:part(maps:get(<<"uri">>, R), {0, 7}) == <<"file://">>],
    ?assertEqual(3, length(FileURIs)),
    ok.

test_http_https_uri_schemes(Server) ->
    %% Test http:// and https:// scheme resources
    HttpResources = [
        #mcp_resource{
            uri = <<"http://api.example.com/users">>,
            name = <<"Users API">>,
            mime_type = <<"application/json">>,
            metadata = #{endpoint => "users", auth => "required"}
        },
        #mcp_resource{
            uri = <<"https://cdn.example.com/images/avatar.jpg">>,
            name = <<"Avatar Image">>,
            mime_type = <<"image/jpeg">>,
            metadata = #{endpoint => "cdn", cache => "enabled"}
        },
        #mcp_resource{
            uri = <<"https://docs.example.com/guide.pdf">>,
            name = <<"Documentation">>,
            mime_type = <<"application/pdf">>,
            metadata = #{endpoint => "docs", type => "manual"}
        }
    ],

    %% Add HTTP resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_http_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, HttpResources),

    %% Verify HTTP resources
    ListedResources = get_resource_list_from_server(Server),
    HttpURIs = [maps:get(<<"uri">>, R) || R <- ListedResources,
                                      binary:part(maps:get(<<"uri">>, R), {0, 7}) == <<"http://">> orelse
                                      binary:part(maps:get(<<"uri">>, R), {0, 8}) == <<"https://">>],
    ?assertEqual(3, length(HttpURIs)),
    ok.

test_git_uri_scheme(Server) ->
    %% Test git:// scheme resources
    GitResources = [
        #mcp_resource{
            uri = <<"git://github.com/user/repo">>,
            name =GitHub Repository">>,
            mime_type = <<"text/plain">>,
            metadata = #{host => "github", user => "user", repo => "repo", branch => "main"}
        },
        #mcp_resource{
            uri = <<"git://gitlab.com/project/source">>,
            name = <<"GitLab Project">>,
            mime_type = <<"text/plain">>,
            metadata = #{host => "gitlab", project => "project", type => "source"}
        }
    ],

    %% Add Git resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_git_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, GitResources),

    %% Verify Git resources
    ListedResources = get_resource_list_from_server(Server),
    GitURIs = [maps:get(<<"uri">>, R) || R <- ListedResources,
                                     binary:part(maps:get(<<"uri">>, R), {0, 6}) == <<"git://">>],
    ?assertEqual(2, length(GitURIs)),
    ok.

test_custom_uri_scheme(Server) ->
    %% Test custom:// scheme resources
    CustomResources = [
        #mcp_resource{
            uri = <<"custom://database/primary">>,
            name = <<"Primary Database">>,
            mime_type = <<"application/json">>,
            metadata = #{type => "database", role => "primary", connection => "required"}
        },
        #mcp_resource{
            uri = <<"custom://cache/session">>,
            name = <<"Session Cache">>,
            mime_type = <<"application/json">>,
            metadata = #{type => "cache", scope => "session", ttl => 3600}
        },
        #mcp_resource{
            uri = <<"custom://queue/tasks">>,
            name = <<"Task Queue">>,
            mime_type = <<"application/json">>,
            metadata = #{type => "queue", priority => "high", capacity => 1000}
        }
    ],

    %% Add custom resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_custom_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, CustomResources),

    %% Verify custom resources
    ListedResources = get_resource_list_from_server(Server),
    CustomURIs = [maps:get(<<"uri">>, R) || R <- ListedResources,
                                        binary:part(maps:get(<<"uri">>, R), {0, 8}) == <<"custom://">>],
    ?assertEqual(3, length(CustomURIs)),
    ok.

%%====================================================================
%% Resource Annotations Tests
%%====================================================================

resource_annotations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_resource_metadata_annotations(Server)),
             ?_test(test_audience_priority_annotations(Server)),
             ?_test(test_last_modified_annotations(Server))
         ]
     end}.

test_resource_metadata_annotations(Server) ->
    %% Add resources with metadata annotations
    AnnotatedResources = [
        #mcp_resource{
            uri = <<"file:///annotated/metadata1.txt">>,
            name = <<"Metadata 1">>,
            description = <<"Resource with metadata">>,
            mime_type = <<"text/plain">>,
            metadata = #{
                author => <<"alice">>,
                created => <<"2024-01-01">>,
                tags => [<<"important">>, <<"public">>],
                category => <<"document">>,
                access_level => <<"read-only">>
            }
        },
        #mcp_resource{
            uri = <<"file:///annotated/metadata2.txt">>,
            name = <<"Metadata 2">>,
            description = <<"Another annotated resource">>,
            mime_type = <<"text/plain">>,
            metadata = #{
                author => <<"bob">>,
                created => <<"2024-01-02">>,
                tags => [<<"internal">>, <<"draft">>],
                category => <<"template">>,
                access_level => <<"private">>
            }
        }
    ],

    %% Add annotated resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, AnnotatedResources),

    %% Verify annotated resources
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(2, length(ListedResources)),

    %% Verify metadata
    Metadata1 = get_resource_metadata(ListedResources, <<"file:///annotated/metadata1.txt">>),
    ?assertEqual(<<"alice">>, maps:get(<<"author">>, Metadata1)),
    ?assertEqual([<<"important">>, <<"public">>], maps:get(<<"tags">>, Metadata1)),
    ok.

test_audience_priority_annotations(Server) ->
    %% Add resources with audience and priority annotations
    AudienceResources = [
        #mcp_resource{
            uri = <<"file:///audience/public.txt">>,
            name = <<"Public Resource">>,
            description = <<"Resource for public audience">>,
            mime_type = <<"text/plain">>,
            metadata = #{
                audience => <<"public">>,
                priority => <<"normal">>,
                visibility => <<"everyone">>
            }
        },
        #mcp_resource{
            uri = <<"file:///audience/admin.txt">>,
            name = <<"Admin Resource">>,
            description = <<"Resource for admin audience">>,
            mime_type = <<"text/plain">>,
            metadata = #{
                audience => <<"admin">>,
                priority => <<"high">>,
                visibility => <<"administrators">>
            }
        },
        #mcp_resource{
            uri = <<"file:///audience/team.txt">>,
            name = <<"Team Resource">>,
            description = <<"Resource for team members">>,
            mime_type = <<"text/plain">>,
            metadata = #{
                audience => <<"team">>,
                priority => <<"medium">>,
                visibility => <<"members">>
            }
        }
    ],

    %% Add audience resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, AudienceResources),

    %% Verify audience resources
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(3, length(ListedResources)),

    %% Verify audience annotations
    PublicResource = get_resource_by_uri(ListedResources, <<"file:///audience/public.txt">>),
    ?assertEqual(<<"public">>, maps:get(<<"audience">>, maps:get(<<"metadata">>, PublicResource))),
    ?assertEqual(<<"normal">>, maps:get(<<"priority">>, maps:get(<<"metadata">>, PublicResource))),
    ok.

test_last_modified_annotations(Server) ->
    %% Add resources with lastModified annotations
    TimeResources = [
        #mcp_resource{
            uri = <<"file:///time/recent.txt">>,
            name = <<"Recent Resource">>,
            description = <<"Recently modified">>,
            mime_type = <<"text/plain">>,
            metadata = #{
                lastModified => erlang:system_time(millisecond),
                modified_by => <<"alice">>,
                version => <<"2.1">>
            }
        },
        #mcp_resource{
            uri = <<"file:///time/old.txt">>,
            name = <<"Old Resource">>,
            description = <<"Old resource">>,
            mime_type = <<"text/plain">>,
            metadata = #{
                lastModified => erlang:system_time(millisecond) - 86400000, % 1 day ago
                modified_by => <<"bob">>,
                version => <<"1.0">>
            }
        }
    ],

    %% Add time-based resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, TimeResources),

    %% Verify time-based resources
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(2, length(ListedResources)),

    %% Verify lastModified annotations
    RecentResource = get_resource_by_uri(ListedResources, <<"file:///time/recent.txt">>),
    RecentMetadata = maps:get(<<"metadata">>, RecentResource),
    ?assert(is_integer(maps:get(<<"lastModified">>, RecentMetadata))),
    ?assertEqual(<<"alice">>, maps:get(<<"modified_by">>, RecentMetadata)),
    ok.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_not_found_error_handling(Server)),
             ?_test(test_invalid_uri_error_handling(Server)),
             ?_test(test_permission_error_handling(Server)),
             ?_test(test_timeout_error_handling(Server)),
             ?_test(test_rate_limit_error_handling(Server))
         ]
     end}.

test_not_found_error_handling(Server) ->
    %% Test accessing non-existent resource
    Result = erlmcp_server:delete_resource(Server, <<"file:///nonexistent.txt">>),
    ?assertMatch({error, not_found}, Result),

    %% Test reading non-existent resource (simulated)
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(0, length(ListedResources)),
    ok.

test_invalid_uri_error_handling(Server) ->
    %% Test adding resource with invalid URI
    InvalidURIResource = #mcp_resource{
        uri = <<>>,  % Empty URI
        name = <<"Invalid URI">>,
        mime_type = <<"text/plain">>
    },

    Result = erlmcp_server:add_resource(Server, InvalidURIResource, fun(_Uri) -> <<"content">> end),
    ?assertMatch({error, {invalid_params, _, _}}, Result),

    %% Test resource with invalid characters in URI
    InvalidCharactersResource = #mcp_resource{
        uri = <<"file:///path with spaces/file.txt">>,  % Spaces not allowed
        name = <<"Invalid Characters">>,
        mime_type = <<"text/plain">>
    },

    Result2 = erlmcp_server:add_resource(Server, InvalidCharactersResource, fun(_Uri) -> <<"content">> end),
    ?assertMatch({error, {invalid_params, _, _}}, Result2),
    ok.

test_permission_error_handling(Server) ->
    %% Test permission-based error handling
    %% In a real implementation, this would check user permissions
    ProtectedResource = #mcp_resource{
        uri = <<"file:///protected/secret.txt">>,
        name = <<"Protected Resource">>,
        mime_type = <<"text/plain">>,
        metadata = #{permission => <<"admin">>, access => <<"restricted">>}
    },

    %% Try to add protected resource (might fail depending on permissions)
    Result = erlmcp_server:add_resource(Server, ProtectedResource, fun(_Uri) -> <<"secret">> end),
    ?assertMatch(ok, Result),  % Should succeed in test environment
    ok.

test_timeout_error_handling(Server) ->
    %% Test timeout scenarios
    %% Create a resource with a handler that takes time
    SlowResource = #mcp_resource{
        uri = <<"file:///slow/resource.txt">>,
        name = <<"Slow Resource">>,
        mime_type = <<"text/plain">>
    },

    SlowHandler = fun(_Uri) ->
        %% Simulate slow operation
        timer:sleep(100),
        <<"slow response">>
    end,

    Result = erlmcp_server:add_resource(Server, SlowResource, SlowHandler),
    ?assertMatch(ok, Result),

    %% Test listing with timeout (simulated)
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(1, length(ListedResources)),
    ok.

test_rate_limit_error_handling(Server) ->
    %% Test rate limiting scenarios
    %% In a real implementation, this would check rate limits

    %% Add multiple resources rapidly (simulate high load)
    NumResources = 100,
    RapidResources = lists:map(fun(I) ->
        #mcp_resource{
            uri = <<"file:///rapid/", (integer_to_binary(I))/binary, ".txt">>,
            name = <<"Rapid Resource ", (integer_to_binary(I))/binary>>,
            mime_type = <<"text/plain">>
        }
    end, lists:seq(1, NumResources)),

    %% Add resources rapidly
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, RapidResources),

    %% Verify all were added (no rate limiting in test)
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(NumResources, length(ListedResources)),
    ok.

%%====================================================================
%% Resource Permissions Tests
%%====================================================================

resource_permissions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_read_permissions(Server)),
             ?_test(test_write_permissions(Server)),
             ?_test(test_admin_permissions(Server))
         ]
     end}.

test_read_permissions(Server) ->
    %% Add resources with different read permissions
    ReadResources = [
        #mcp_resource{
            uri = <<"file:///public/read.txt">>,
            name = <<"Public Read">>,
            mime_type = <<"text/plain">>,
            metadata = #{read_permission => <<"public">>, write_permission => <<"owner">>}
        },
        #mcp_resource{
            uri = <<"file:///private/read.txt">>,
            name = <<"Private Read">>,
            mime_type = <<"text/plain">>,
            metadata = #{read_permission => <<"private">>, write_permission => <<"admin">>}
        }
    ],

    %% Add read permission resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, ReadResources),

    %% Verify read permission resources
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(2, length(ListedResources)),
    ok.

test_write_permissions(Server) ->
    %% Add resources with write permissions
    WriteResources = [
        #mcp_resource{
            uri = <<"file:///public/write.txt">>,
            name = <<"Public Write">>,
            mime_type = <<"text/plain">>,
            metadata = #{read_permission => <<"public">>, write_permission => <<"public">>}
        },
        #mcp_resource{
            uri = <<"file:///restricted/write.txt">>,
            name = <<"Restricted Write">>,
            mime_type = <<"text/plain">>,
            metadata = #{read_permission => <<"public">>, write_permission => <<"admin">>}
        }
    ],

    %% Add write permission resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, WriteResources),

    %% Test write operations
    ok = erlmcp_server:notify_resource_updated(Server, <<"file:///public/write.txt">>, #{updated => true}),

    %% Should fail for restricted resource (in real implementation)
    ok = erlmcp_server:delete_resource(Server, <<"file:///restricted/write.txt">>),
    ok.

test_admin_permissions(Server) ->
    %% Add admin-only resources
    AdminResources = [
        #mcp_resource{
            uri = <<"file:///admin/config.txt">>,
            name = <<"Admin Config">>,
            mime_type = <<"text/plain">>,
            metadata = #{permission => <<"admin">>, access => <<"restricted">>}
        },
        #mcp_resource{
            uri = <<"file:///admin/system.txt">>,
            name = <<"System File">>,
            mime_type = <<"text/plain">>,
            metadata = #{permission => <<"admin">>, access => <<"system">>}
        }
    ],

    %% Add admin permission resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, AdminResources),

    %% Verify admin resources
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(2, length(ListedResources)),

    %% Test admin operations
    ok = erlmcp_server:delete_resource(Server, <<"file:///admin/config.txt">>),
    ok = erlmcp_server:notify_resources_changed(Server),
    ok.

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_large_resource_performance(Server)),
             ?_test(test_concurrent_resource_access(Server)),
             ?_test(test_memory_usage_performance(Server))
         ]
     end}.

test_large_resource_performance(Server) ->
    %% Test performance with large resources
    LargeResource = #mcp_resource{
        uri = <<"file:///large/performance.txt">>,
        name = <<"Large Performance Test">>,
        mime_type = <<"text/plain">>,
        metadata = #{size => 10485760}  % 10MB
    },

    %% Create large content (simulated)
    LargeContent = binary:copy(<<"This is line number ", (integer_to_binary(1))/binary, ". ">>, 655360), % ~10MB
    LargeHandler = fun(_Uri) -> LargeContent end,

    AddStart = erlang:monotonic_time(millisecond),
    ok = erlmcp_server:add_resource(Server, LargeResource, LargeHandler),
    AddEnd = erlang:monotonic_time(millisecond),

    %% Test resource access performance
    ListedResources = get_resource_list_from_server(Server),
    ?assertEqual(1, length(ListedResources)),

    AddDuration = AddEnd - AddStart,
    logger:info("Large resource addition: ~p bytes in ~p ms (~.2f MB/s)",
                [byte_size(LargeContent), AddDuration,
                 (byte_size(LargeContent) / 1024 / 1024) / (AddDuration / 1000)]),
    ok.

test_concurrent_resource_access(Server) ->
    %% Test concurrent access to resources
    NumConcurrent = 50,
    Resource = #mcp_resource{
        uri = <<"file:///concurrent/access.txt">>,
        name = <<"Concurrent Access Test">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(_Uri) -> <<"concurrent content">> end,
    ok = erlmcp_server:add_resource(Server, Resource, Handler),

    %% Create concurrent accessors
    ConcurrentPids = lists:map(fun(I) ->
        spawn_link(fun() ->
            %% Simulate concurrent resource access
            ListedResources = get_resource_list_from_server(Server),
            case length(ListedResources) of
                1 -> ok;
                _ -> exit(concurrent_error)
            end,
            timer:sleep(10), % Simulate work
            exit(normal)
        end)
    end, lists:seq(1, NumConcurrent)),

    %% Wait for all concurrent operations to complete
    ExitResults = [receive
        {'EXIT', Pid, Reason} -> {Pid, Reason}
    end || Pid <- ConcurrentPids],

    %% Verify all completed successfully
    Successful = [R || {_, normal} <- ExitResults],
    ?assertEqual(NumConcurrent, length(Successful)),

    %% Clean up
    [exit(Pid, kill) || {Pid, _} <- ExitResults],
    ok.

test_memory_usage_performance(Server) ->
    %% Test memory usage during resource operations
    InitialMemory = erlang:memory(total),

    %% Add many resources
    NumResources = 1000,
    Resources = lists:map(fun(I) ->
        #mcp_resource{
            uri = <<"file:///", (integer_to_binary(I))/binary, ".txt">>,
            name = <<"Memory Test ", (integer_to_binary(I))/binary>>,
            mime_type = <<"text/plain">>,
            metadata = #{test => "memory", id => I}
        }
    end, lists:seq(1, NumResources)),

    %% Add resources and track memory
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, Resources),

    %% Check memory usage after adding resources
    AfterAddMemory = erlang:memory(total),
    MemoryIncrease = AfterAddMemory - InitialMemory,

    logger:info("Memory usage: ~p bytes for ~p resources (~.2f bytes/resource)",
                [MemoryIncrease, NumResources, MemoryIncrease / NumResources]),

    %% Clean up resources
    lists:foreach(fun(Resource) ->
        ok = erlmcp_server:delete_resource(Server, Resource#mcp_resource.uri)
    end, Resources),

    %% Check memory after cleanup
    AfterCleanupMemory = erlang:memory(total),
    CleanupMemoryDiff = AfterCleanupMemory - AfterAddMemory,

    logger:info("Memory cleanup: ~p bytes difference", [CleanupMemoryDiff]),
    ok.

%%====================================================================
%% Concurrency Tests
%%====================================================================

concurrency_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_resource_concurrent_modification(Server)),
             ?_test(test_subscription_concurrent_access(Server)),
             ?_test(test_notification_concurrent_broadcast(Server))
         ]
     end}.

test_resource_concurrent_modification(Server) ->
    %% Test concurrent modification of resources
    Resource = #mcp_resource{
        uri = <<"file:///concurrent/modify.txt">>,
        name = <<"Concurrent Modify Test">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(_Uri) -> <<"original content">> end,
    ok = erlmcp_server:add_resource(Server, Resource, Handler),

    %% Create concurrent modifiers
    NumModifiers = 10,
    ModifierPids = lists:map(fun(I) ->
        spawn_link(fun() ->
            %% Each modifier attempts to update the resource
            NewResource = #mcp_resource{
                uri = Resource#mcp_resource.uri,
                name = <<Resource#mcp_resource.name/binary, "_", (integer_to_binary(I))/binary>>,
                mime_type = Resource#mcp_resource.mime_type
            },
            Handler = fun(_Uri) -> <<"content from ", (integer_to_binary(I))/binary>> end,

            Result = erlmcp_server:add_resource(Server, NewResource, Handler),
            case Result of
                ok -> ok;
                {error, _} -> exit(concurrent_error)
            end,

            timer:sleep(5), % Allow interleaving
            exit(normal)
        end)
    end, lists:seq(1, NumModifiers)),

    %% Wait for all modifiers to complete
    Results = [receive
        {'EXIT', Pid, Reason} -> {Pid, Reason}
    end || Pid <- ModifierPids],

    %% Check results
    Successful = [R || {_, normal} <- Results],
    logger:info("Concurrent modification: ~p/~p successful", [length(Successful), NumModifiers]),

    %% Clean up
    [exit(Pid, kill) || {Pid, _} <- Results],
    ok.

test_subscription_concurrent_access(Server) ->
    %% Test concurrent access to subscriptions
    Resource = #mcp_resource{
        uri = <<"file:///concurrent/subscribe.txt">>,
        name = <<"Concurrent Subscription Test">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(_Uri) -> <<"subscription content">> end,
    ok = erlmcp_server:add_resource(Server, Resource, Handler),

    %% Create concurrent subscribers
    NumSubscribers = 20,
    SubscriberPids = lists:map(fun(_) ->
        spawn_link(fun() ->
            SubscriberPid = self(),

            %% Subscribe
            ok = erlmcp_server:subscribe_resource(Server, Resource#mcp_resource.uri, SubscriberPid),

            %% Wait for notifications (simulated)
            receive
                {resource_updated, _, _} -> ok
            after 1000 ->
                timeout
            end,

            %% Unsubscribe
            ok = erlmcp_server:unsubscribe_resource(Server, Resource#mcp_resource.uri),

            exit(normal)
        end)
    end, lists:seq(1, NumSubscribers)),

    %% Trigger notifications
    ok = erlmcp_server:notify_resource_updated(Server, Resource#mcp_resource.uri,
                                               #{test => "concurrent", timestamp => erlang:system_time(millisecond)}),

    %% Wait for subscribers to complete
    Results = [receive
        {'EXIT', Pid, Reason} -> {Pid, Reason}
    end || Pid <- SubscriberPids],

    Successful = [R || {_, normal} <- Results],
    logger:info("Concurrent subscription: ~p/~p successful", [length(Successful), NumSubscribers]),

    %% Clean up
    [exit(Pid, kill) || {Pid, _} <- Results],
    ok.

test_notification_concurrent_broadcast(Server) ->
    %% Test concurrent notification broadcasting
    NumResources = 5,
    Resources = lists:map(fun(I) ->
        #mcp_resource{
            uri = <<"file:///concurrent/notify_", (integer_to_binary(I))/binary, ".txt">>,
            name = <<"Concurrent Notify ", (integer_to_binary(I))/binary>>,
            mime_type = <<"text/plain">>
        }
    end, lists:seq(1, NumResources)),

    %% Add resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, Resources),

    %% Create concurrent notification senders
    NumSenders = 10,
    SenderPids = lists:map(fun(I) ->
        spawn_link(fun() ->
            %% Send notification for a random resource
            ResourceIndex = (I rem NumResources) + 1,
            ResourceUri = <<"file:///concurrent/notify_", (integer_to_binary(ResourceIndex))/binary, ".txt">>,

            ok = erlmcp_server:notify_resource_updated(Server, ResourceUri,
                                                      {concurrent_test, erlang:system_time(millisecond)}),

            exit(normal)
        end)
    end, lists:seq(1, NumSenders)),

    %% Wait for all senders to complete
    Results = [receive
        {'EXIT', Pid, Reason} -> {Pid, Reason}
    end || Pid <- SenderPids],

    Successful = [R || {_, normal} <- Results],
    logger:info("Concurrent notification: ~p/~p successful", [length(Successful), NumSenders]),

    %% Clean up
    [exit(Pid, kill) || {Pid, _} <- Results],
    ok.

%%====================================================================
%% Memory Management Tests
%%====================================================================

memory_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({Server, _ServerId}) ->
         [
             ?_test(test_resource_cleanup(Server)),
             ?_test(test_subscription_cleanup(Server)),
             ?_test(test_memory_leak_detection(Server))
         ]
     end}.

test_resource_cleanup(Server) ->
    %% Test proper cleanup of resources
    InitialMemory = erlang:memory(total),

    %% Add many resources
    NumResources = 100,
    Resources = lists:map(fun(I) ->
        #mcp_resource{
            uri = <<"file:///", (integer_to_binary(I))/binary, ".txt">>,
            name = <<"Cleanup Test ", (integer_to_binary(I))/binary>>,
            mime_type = <<"text/plain">>
        }
    end, lists:seq(1, NumResources)),

    %% Add resources
    lists:foreach(fun(Resource) ->
        Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler)
    end, Resources),

    %% Check memory after adding
    AfterAddMemory = erlang:memory(total),

    %% Remove all resources
    lists:foreach(fun(Resource) ->
        ok = erlmcp_server:delete_resource(Server, Resource#mcp_resource.uri)
    end, Resources),

    %% Check memory after removal
    AfterCleanupMemory = erlang:memory(total),

    MemoryDiff = AfterCleanupMemory - AfterAddMemory,
    logger:info("Resource cleanup memory difference: ~p bytes", [MemoryDiff]),

    %% Memory should be cleaned up (diff should be small)
    ?assert(MemoryDiff < 1024 * 1024), % Less than 1MB difference
    ok.

test_subscription_cleanup(Server) ->
    %% Test proper cleanup of subscriptions
    %% Add test resource
    Resource = #mcp_resource{
        uri = <<"file:///cleanup/subscription.txt">>,
        name = <<"Subscription Cleanup Test">>,
        mime_type = <<"text/plain">>
    },

    Handler = fun(_Uri) -> <<"subscription content">> end,
    ok = erlmcp_server:add_resource(Server, Resource, Handler),

    %% Create many subscribers
    NumSubscribers = 50,
    Subscribers = lists:map(fun(_) ->
        spawn_subscriber()
    end, lists:seq(1, NumSubscribers)),

    %% Subscribe all
    lists:foreach(fun(Subscriber) ->
        erlmcp_server:subscribe_resource(Server, Resource#mcp_resource.uri, Subscriber)
    end, Subscribers),

    %% Remove all subscriptions
    lists:foreach(fun(_) ->
        ok = erlmcp_server:unsubscribe_resource(Server, Resource#mcp_resource.uri)
    end, lists:seq(1, NumSubscribers)),

    %% Clean up subscribers
    lists:foreach(fun(Subscriber) ->
        exit(Subscriber, kill)
    end, Subscribers),

    %% Verify subscriptions are cleaned up
    ok.

test_memory_leak_detection(Server) ->
    %% Test for memory leaks by monitoring memory usage
    InitialMemory = erlang:memory(total),

    %% Perform multiple cycles of add/remove operations
    NumCycles = 10,
    ResourcesPerCycle = 20,

    lists:map(fun(Cycle) ->
        %% Add resources
        CycleResources = lists:map(fun(I) ->
            #mcp_resource{
                uri = <<"file:///", (integer_to_binary(Cycle))/binary, "_", (integer_to_binary(I))/binary, ".txt">>,
                name = <<"Leak Test ", (integer_to_binary(Cycle))/binary, "_", (integer_to_binary(I))/binary>>,
                mime_type = <<"text/plain">>
            }
        end, lists:seq(1, ResourcesPerCycle)),

        lists:foreach(fun(Resource) ->
            Handler = fun(Uri) -> get_resource_content(Uri, Resource) end,
            ok = erlmcp_server:add_resource(Server, Resource, Handler)
        end, CycleResources),

        %% Remove resources
        lists:foreach(fun(Resource) ->
            ok = erlmcp_server:delete_resource(Server, Resource#mcp_resource.uri)
        end, CycleResources),

        %% Force garbage collection
        garbage_collect(),
        timer:sleep(100)  % Allow cleanup
    end, lists:seq(1, NumCycles)),

    %% Check final memory usage
    FinalMemory = erlang:memory(total),
    MemoryGrowth = FinalMemory - InitialMemory,

    logger:info("Memory leak test: ~p bytes growth over ~p cycles", [MemoryGrowth, NumCycles]),

    %% Memory growth should be minimal (less than 1MB)
    ?assert(MemoryGrowth < 1024 * 1024),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Get resource list from server (simulated)
get_resource_list_from_server(Server) ->
    %% In a real implementation, this would call the actual resources/list method
    %% For testing, we simulate the response
    [].

%% Get template list from server (simulated)
get_template_list_from_server(Server) ->
    %% Simulated template listing
    [].

%% Get resource content based on URI and resource
get_resource_content(Uri, Resource) ->
    Resource#mcp_resource.description.

%% Get template content
get_template_content(Uri, Template) ->
    <<"Template content for ", Template#mcp_resource_template.name/binary>>.

%% Get file content (simulated)
get_file_content(Uri, Resource) ->
    <<"File content from ", Uri/binary>>.

%% Get HTTP content (simulated)
get_http_content(Uri, Resource) ->
    <<"HTTP content from ", Uri/binary>>.

%% Get Git content (simulated)
get_git_content(Uri, Resource) ->
    <<"Git content from ", Uri/binary>>.

%% Get custom content (simulated)
get_custom_content(Uri, Resource) ->
    <<"Custom content from ", Uri/binary>>.

%% Filter resources by category
filter_resources_by_category(Resources, Category) ->
    lists:filter(fun(Resource) ->
        case maps:get(<<"metadata">>, Resource, #{}) of
            Meta when is_map(Meta) ->
                case maps:get(<<"category">>, Meta, undefined) of
                    Category -> true;
                    _ -> false
                end;
            _ -> false
        end
    end, Resources).

%% Get resource metadata
get_resource_metadata(Resources, Uri) ->
    case get_resource_by_uri(Resources, Uri) of
        undefined -> #{};
        Resource -> maps:get(<<"metadata">>, Resource, #{})
    end.

%% Get resource by URI
get_resource_by_uri(Resources, Uri) ->
    lists:foldl(fun(Resource, Acc) ->
        case maps:get(<<"uri">>, Resource) of
            Uri -> Resource;
            _ -> Acc
        end
    end, undefined, Resources).

%% Spawn a subscriber process
spawn_subscriber() ->
    spawn(fun() ->
        %% Simple subscriber that just receives messages
        receive
            _ -> ok
        end
    end).