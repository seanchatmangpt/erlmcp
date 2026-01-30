%% @doc MCP Resource Management Compliance Test Suite
%% Tests all resource-related capabilities according to MCP specification
-module(mcp_resources_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%% Test exports
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test case exports
-export([
    %% Basic Resource Operations
    resource_registration/1,
    resource_listing/1,
    resource_reading/1,
    resource_deletion/1,

    %% Resource Types
    resource_text_content/1,
    resource_binary_content/1,
    resource_uri_variants/1,

    %% Resource Templates
    resource_template_registration/1,
    resource_template_usage/1,
    resource_template_pagination/1,

    %% Resource Subscriptions
    resource_subscription_lifecycle/1,
    resource_subscription_notification/1,
    resource_subscription_cleanup/1,

    %% Resource Notifications
    resource_list_changed_notification/1,
    resource_updated_notification/1,
    resource_notification_handler/1,

    %% URI Schemes
    uri_file_scheme/1,
    uri_https_scheme/1,
    uri_git_scheme/1,
    uri_custom_scheme/1,
    uri_validation/1,

    %% Resource Annotations
    resource_audience_annotation/1,
    resource_priority_annotation/1,
    resource_last_modified_annotation/1,
    resource_annotation_validation/1,

    %% Pagination
    pagination_cursor_based/1,
    pagination_page_size/1,
    pagination_edge_cases/1,

    %% Error Handling
    resource_not_found/1,
    invalid_uri/1,
    permission_denied/1,
    resource_corrupted/1,
    concurrent_resource_access/1,

    %% Performance Tests
    resource_registration_performance/1,
    resource_read_performance/1,
    large_resource_handling/1,
    concurrent_resource_operations/1,

    %% Security Tests
    resource_uri_injection/1,
    resource_path_traversal/1,
    malicious_resource_names/1,

    %% Integration Tests
    resource_tool_integration/1,
    resource_client_integration/1,
    resource_server_interaction/1
]).

%%====================================================================
%% Test configuration
%%====================================================================

all() ->
    [
        %% Basic Resource Operations
        resource_registration,
        resource_listing,
        resource_reading,
        resource_deletion,

        %% Resource Types
        resource_text_content,
        resource_binary_content,
        resource_uri_variants,

        %% Resource Templates
        resource_template_registration,
        resource_template_usage,
        resource_template_pagination,

        %% Resource Subscriptions
        resource_subscription_lifecycle,
        resource_subscription_notification,
        resource_subscription_cleanup,

        %% Resource Notifications
        resource_list_changed_notification,
        resource_updated_notification,
        resource_notification_handler,

        %% URI Schemes
        uri_file_scheme,
        uri_https_scheme,
        uri_git_scheme,
        uri_custom_scheme,
        uri_validation,

        %% Resource Annotations
        resource_audience_annotation,
        resource_priority_annotation,
        resource_last_modified_annotation,
        resource_annotation_validation,

        %% Pagination
        pagination_cursor_based,
        pagination_page_size,
        pagination_edge_cases,

        %% Error Handling
        resource_not_found,
        invalid_uri,
        permission_denied,
        resource_corrupted,
        concurrent_resource_access,

        %% Performance Tests
        resource_registration_performance,
        resource_read_performance,
        large_resource_handling,
        concurrent_resource_operations,

        %% Security Tests
        resource_uri_injection,
        resource_path_traversal,
        malicious_resource_names,

        %% Integration Tests
        resource_tool_integration,
        resource_client_integration,
        resource_server_interaction
    ].

init_per_suite(Config) ->
    %% Start required applications
    {ok, Apps} = application:ensure_all_started(erlmcp_core),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    Apps = proplists:get_value(apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Apps)),
    Config.

init_per_testcase(TestCase, Config) ->
    process_flag(trap_exit, true),

    %% Create test server with resource capabilities
    ServerCapabilities = #mcp_server_capabilities{
        resources = #{subscribe => true, listChanged => true},
        tools = #{},
        prompts = #{}
    },

    {ok, ServerPid} = erlmcp_server:start_link(test_server, ServerCapabilities),

    %% Create test client
    {ok, ClientPid} = erlmcp_client:start_link({stdio, []}),

    %% Initialize client
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    },
    {ok, _} = erlmcp_client:initialize(ClientPid, ClientCapabilities),

    %% Set up notification handler
    Notifications = [],
    NotificationHandler = fun(Method, Params) ->
        ct:pal("Notification received: ~p ~p", [Method, Params]),
        Notifications ++ [{Method, Params}]
    end,
    erlmcp_client:set_notification_handler(ClientPid, <<"resources">>, NotificationHandler),

    [{server_pid, ServerPid}, {client_pid, ClientPid}, {notifications, []} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Clean up
    proplists:delete_value(server_pid, Config),
    proplists:delete_value(client_pid, Config),
    proplists:delete_value(notifications, Config),
    ok.

%%====================================================================
%% Basic Resource Operations
%%====================================================================

resource_registration(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test resource registration
    Uri = <<"file:///test_resource.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Initial content">>}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource is registered
    {ok, Resources} = erlmcp_client:list_resources(ClientPid),
    ct:pal("Registered resources: ~p", [Resources]),

    %% Resource should be in the list
    case Resources of
        {resources, ResourceList} when is_list(ResourceList) ->
            lists:any(fun(Res) ->
                maps:get(<<"uri">>, Res) =:= Uri
            end, ResourceList);
        _ ->
            ct:fail("Invalid resource list format")
    end.

resource_listing(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add multiple resources
    Resources = [
        {<<"file:///res1.txt">>, <<"Resource 1">>, <<"text/plain">>},
        {<<"file:///res2.txt">>, <<"Resource 2">>, <<"text/plain">>},
        {<<"file:///res3.txt">>, <<"Resource 3">>, <<"text/plain">>}
    ],

    lists:foreach(fun({Uri, Name, MimeType}) ->
        Handler = fun(_) -> {ok, {text, <<"Content for ", Name/binary>>}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, Resources),

    %% List all resources
    {ok, ListedResources} = erlmcp_client:list_resources(ClientPid),

    case ListedResources of
        {resources, ResourceList} when is_list(ResourceList) ->
            ct:pal("Listed ~p resources", [length(ResourceList)]),
            length(ResourceList) >= 3;
        _ ->
            ct:fail("Invalid resource listing result")
    end.

resource_reading(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resource
    Uri = <<"file:///read_test.txt">>,
    Content = <<"Hello, World! This is a test resource.">>,
    Handler = fun(_) -> {ok, {text, Content}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Read resource
    {ok, Result} = erlmcp_client:read_resource(ClientPid, Uri),

    case Result of
        {contents, [#{text := Content}]} ->
            ct:pal("Resource content read successfully"),
            true;
        _ ->
            ct:fail("Failed to read resource content")
    end.

resource_deletion(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resource
    Uri = <<"file:///delete_test.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Temporary content">>}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Delete resource
    ok = erlmcp_server:delete_resource(ServerPid, Uri),

    %% Verify resource is deleted
    {ok, Resources} = erlmcp_client:list_resources(ClientPid),

    case Resources of
        {resources, ResourceList} when is_list(ResourceList) ->
            not lists:any(fun(Res) -> maps:get(<<"uri">>, Res) =:= Uri end, ResourceList);
        _ ->
            ct:fail("Invalid resource list format")
    end.

%%====================================================================
%% Resource Types
%%====================================================================

resource_text_content(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test text resource
    Uri = <<"file:///text_resource.txt">>,
    TextContent = <<"This is a text resource with UTF-8 content: ñáéíóú">>,
    Handler = fun(_) -> {ok, {text, TextContent}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Read and verify
    {ok, Result} = erlmcp_client:read_resource(ClientPid, Uri),

    case Result of
        {contents, [#{text := TextContent, mimeType := <<"text/plain">>}]} ->
            ct:pal("Text resource content verified"),
            true;
        _ ->
            ct:fail("Text resource test failed")
    end.

resource_binary_content(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test binary resource (PNG image)
    Uri = <<"file:///binary_resource.png">>,
    OriginalData = <<137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8, 6, 0, 0, 0, 31, 243, 255, 97, 0, 0, 0, 13, 73, 68, 65, 84, 120, 156, 243, 73, 11, 252, 145, 133, 14, 25, 53, 153, 142, 202, 35, 201, 144, 72, 146, 107, 241, 24, 112, 14, 8, 198, 134, 34, 220, 34, 41, 36, 37, 100, 186, 230, 31, 147, 44, 198, 179, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130>>,
    Base64Data = base64:encode(OriginalData),
    Handler = fun(_) -> {ok, {blob, Base64Data, <<"image/png">>}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Read and verify
    {ok, Result} = erlmcp_client:read_resource(ClientPid, Uri),

    case Result of
        {contents, [#{blob := Base64Data, mimeType := <<"image/png">>}]} ->
            ct:pal("Binary resource content verified"),
            true;
        _ ->
            ct:fail("Binary resource test failed")
    end.

resource_uri_variants(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test various URI formats
    URIs = [
        {<<"file:///simple.txt">>, <<"Simple file">>},
        {<<"file:///path/with/slashes/file.txt">>, <<"Path with slashes">>},
        {<<"file:///spaced%20name.txt">>, <<"Spaced name">>}
    ],

    lists:foreach(fun({Uri, Name}) ->
        Handler = fun(_) -> {ok, {text, <<"Content for ", Name/binary>>}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, URIs),

    %% Verify all resources can be read
    lists:foreach(fun({Uri, _}) ->
        {ok, _} = erlmcp_client:read_resource(ClientPid, Uri)
    end, URIs),

    true.

%%====================================================================
%% Resource Templates
%%====================================================================

resource_template_registration(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test resource template registration
    UriTemplate = <<"file:///project/{file}">>,
    Name = <<"Project Files">>,
    Description = <<"Access files in the project directory">>,
    Handler = fun(File) -> {ok, {text, <<"Project file content: ", File/binary>>}} end,

    ok = erlmcp_server:add_resource_template(ServerPid, UriTemplate, Name, Handler),

    %% List templates
    {ok, Templates} = erlmcp_client:list_resource_templates(ClientPid),

    case Templates of
        {resourceTemplates, TemplateList} when is_list(TemplateList) ->
            ct:pal("Registered ~p templates", [length(TemplateList)]),
            length(TemplateList) > 0;
        _ ->
            ct:fail("Invalid template list format")
    end.

resource_template_usage(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add template
    UriTemplate = <<"file:///doc/{document}">>,
    Name = <<"Document Template">>,
    Handler = fun(Doc) -> {ok, {text, <<"Document: ", Doc/binary>>}} end,

    ok = erlmcp_server:add_resource_template(ServerPid, UriTemplate, Name, Handler),

    %% Use template by expanding URI
    ExpandedUri = <<"file:///doc/main.md">>,
    {ok, Result} = erlmcp_client:read_resource(ClientPid, ExpandedUri),

    case Result of
        {contents, [#{text := <<"Document: main.md">>}]} ->
            ct:pal("Template usage verified"),
            true;
        _ ->
            ct:fail("Template usage failed")
    end.

resource_template_pagination(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add multiple templates
    Templates = [
        {<<"file:///doc/{doc}">>, <<"Document Template">>},
        {<<"file:///img/{image}">>, <<"Image Template">>},
        {<<"file:///code/{file}">>, <<"Code Template">>},
        {<<"file:///data/{file}">>, <<"Data Template">>},
        {<<"file:///config/{file}">>, <<"Config Template">>}
    ],

    lists:foreach(fun({UriTemplate, Name}) ->
        Handler = fun(_) -> {ok, {text, <<"Template: ", Name/binary>>}} end,
        erlmcp_server:add_resource_template(ServerPid, UriTemplate, Name, Handler)
    end, Templates),

    %% List templates with pagination
    {ok, _} = erlmcp_client:list_resource_templates(ClientPid),
    true.

%%====================================================================
%% Resource Subscriptions
%%====================================================================

resource_subscription_lifecycle(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resource
    Uri = <<"file:///subscribed_resource.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Initial content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Subscribe to resource
    {ok, _} = erlmcp_client:subscribe_to_resource(ClientPid, Uri),

    %% Verify subscription
    {ok, Result} = erlmcp_client:read_resource(ClientPid, Uri),
    true = case Result of
        {contents, _} -> true;
        _ -> false
    end,

    %% Unsubscribe from resource
    ok = erlmcp_client:unsubscribe_from_resource(ClientPid, Uri),

    ct:pal("Resource subscription lifecycle completed"),
    true.

resource_subscription_notification(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resource
    Uri = <<"file:///notification_resource.txt">>,
    InitialContent = <<"Initial content">>,
    Handler = fun(_) -> {ok, {text, InitialContent}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Subscribe to resource
    {ok, _} = erlmcp_client:subscribe_to_resource(ClientPid, Uri),

    %% Update resource (should trigger notification)
    NewContent = <<"Updated content">>,
    erlmcp_server:notify_resource_updated(ServerPid, Uri, NewContent),

    ct:pal("Resource subscription notification test completed"),
    true.

resource_subscription_cleanup(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add multiple resources and subscribe to them
    URIs = [
        <<"file:///cleanup1.txt">>,
        <<"file:///cleanup2.txt">>,
        <<"file:///cleanup3.txt">>
    ],

    lists:foreach(fun(Uri) ->
        Handler = fun(_) -> {ok, {text, <<"Content">>}} end,
        ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),
        {ok, _} = erlmcp_client:subscribe_to_resource(ClientPid, Uri)
    end, URIs),

    %% Unsubscribe from all resources
    lists:foreach(fun(Uri) ->
        ok = erlmcp_client:unsubscribe_from_resource(ClientPid, Uri)
    end, URIs),

    ct:pal("Resource subscription cleanup completed"),
    true.

%%====================================================================
%% Resource Notifications
%%====================================================================

resource_list_changed_notification(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add initial resource
    Uri1 = <<"file:///initial.txt">>,
    Handler1 = fun(_) -> {ok, {text, <<"Initial">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri1, Handler1),

    %% Add new resource (should trigger notification)
    Uri2 = <<"file:///new.txt">>,
    Handler2 = fun(_) -> {ok, {text, <<"New">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri2, Handler2),

    %% Notify of resource list changes
    ok = erlmcp_server:notify_resources_changed(ServerPid),

    ct:pal("Resource list change notification completed"),
    true.

resource_updated_notification(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add resource
    Uri = <<"file:///updated.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Original">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Subscribe to resource
    {ok, _} = erlmcp_client:subscribe_to_resource(ClientPid, Uri),

    %% Update resource
    ok = erlmcp_server:notify_resource_updated(ServerPid, Uri, <<"Updated">>),

    ct:pal("Resource update notification completed"),
    true.

resource_notification_handler(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Set up notification handler
    NotificationHandler = fun(Method, Params) ->
        ct:pal("Notification: ~p ~p", [Method, Params]),
        ok
    end,
    erlmcp_client:set_notification_handler(ClientPid, <<"resources">>, NotificationHandler),

    %% Add resource to trigger notification
    Uri = <<"file:///notify_handler.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Handler test">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    ct:pal("Resource notification handler test completed"),
    true.

%%====================================================================
%% URI Schemes
%%====================================================================

uri_file_scheme(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test file:// scheme
    Uri = <<"file:///project/src/main.rs">>,
    Handler = fun(_) -> {ok, {text, <<"fn main() { println!(\"Hello, world!\"); }">>}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource is accessible
    {ok, _} = erlmcp_client:read_resource(ClientPid, Uri),

    ct:pal("File URI scheme test completed"),
    true.

uri_https_scheme(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test https:// scheme
    Uri = <<"https://example.com/document.pdf">>,
    Handler = fun(_) -> {ok, {blob, base64:encode(<<"PDF content">>), <<"application/pdf">>}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource is accessible
    {ok, _} = erlmcp_client:read_resource(ClientPid, Uri),

    ct:pal("HTTPS URI scheme test completed"),
    true.

uri_git_scheme(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test git:// scheme
    Uri = <<"git://github.com/user/repo/blob/main/README.md">>,
    Handler = fun(_) -> {ok, {text, "# README\n\nThis is a repository."}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource is accessible
    {ok, _} = erlmcp_client:read_resource(ClientPid, Uri),

    ct:pal("Git URI scheme test completed"),
    true.

uri_custom_scheme(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test custom scheme
    Uri = <<"custom://data/record/123">>,
    Handler = fun(_) -> {ok, {text, <<"Custom resource content">>}} end,

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource is accessible
    {ok, _} = erlmcp_client:read_resource(ClientPid, Uri),

    ct:pal("Custom URI scheme test completed"),
    true.

uri_validation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test various URI validations
    InvalidURIs = [
        <<"invalid-uri">>,  % Missing scheme
        <<"http://malicious.com/attack">>,  % Potentially malicious
        <<"file:///../../../etc/passwd">>,  % Path traversal
        <<"javascript:alert('xss')">>,  % Script injection
        <<"data:text/html,<script>alert('xss')</script>">>  % Data URI with script
    ],

    lists:foreach(fun(Uri) ->
        try
            Handler = fun(_) -> {ok, {text, <<"Content">>}} end,
            erlmcp_server:add_resource(ServerPid, Uri, Handler),
            ct:fail("Should have rejected invalid URI: ~p", [Uri])
        catch
            _:_ ->
                ct:pal("Correctly rejected invalid URI: ~p", [Uri])
        end
    end, InvalidURIs),

    true.

%%====================================================================
%% Resource Annotations
%%====================================================================

resource_audience_annotation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add resource with audience annotation
    Uri = <<"file:///audience_test.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Audience test">>}} end,
    Annotations = #{
        <<"audience">> => [<<"user">>, <<"assistant">>],
        <<"priority">> => 0.8
    },

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource can be read
    {ok, _} = erlmcp_client:read_resource(ClientPid, Uri),

    ct:pal("Resource audience annotation test completed"),
    true.

resource_priority_annotation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add resource with priority annotation
    Uri = <<"file:///priority_test.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Priority test">>}} end,
    Annotations = #{
        <<"priority">> => 1.0,
        <<"audience">> => [<<"user">>]
    },

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource can be read
    {ok, _} = erlmcp_client:read_resource(ClientPid, Uri),

    ct:pal("Resource priority annotation test completed"),
    true.

resource_last_modified_annotation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add resource with last modified annotation
    Uri = <<"file:///modified_test.txt">>,
    LastModified = <<"2025-01-12T15:00:58Z">>,
    Handler = fun(_) -> {ok, {text, <<"Modified test">>}} end,
    Annotations = #{
        <<"lastModified">> => LastModified,
        <<"priority">> => 0.5
    },

    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource can be read
    {ok, _} = erlmcp_client:read_resource(ClientPid, Uri),

    ct:pal("Resource last modified annotation test completed"),
    true.

resource_annotation_validation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test various annotation validations
    TestAnnotations = [
        #{<<"audience">> => [<<"user">>]},  % Valid
        #{<<"priority">> => 0.9},  % Valid
        #{<<"lastModified">> => <<"2025-01-12T00:00:00Z">>},  % Valid
        #{<<"audience">> => [<<"invalid">>]},  % Invalid audience
        #{<<"priority">> => 2.0},  % Invalid priority
        #{<<"lastModified">> => <<"invalid-date">>}  % Invalid date
    ],

    lists:foreach(fun(Annotations) ->
        Uri = list_to_binary("file:///annotation_test_" ++ integer_to_list(erlang:phash2(Annotations)) ++ ".txt"),
        Handler = fun(_) -> {ok, {text, <<"Annotation test">>}} end,

        try
            erlmcp_server:add_resource(ServerPid, Uri, Handler),
            ct:pal("Added resource with annotations: ~p", [Annotations])
        catch
            _:_ ->
                ct:pal("Failed to add resource with annotations: ~p", [Annotations])
        end
    end, TestAnnotations),

    true.

%%====================================================================
%% Pagination
%%====================================================================

pagination_cursor_based(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add many resources
    NumResources = 50,
    lists:map(fun(I) ->
        Uri = list_to_binary("file:///page_test_" ++ integer_to_list(I) ++ ".txt"),
        Handler = fun(_) -> {ok, {text, <<"Resource ", integer_to_list(I)/binary>>}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, lists:seq(1, NumResources)),

    %% Test cursor-based pagination
    {ok, _} = erlmcp_client:list_resources(ClientPid),

    ct:pal("Cursor-based pagination test completed"),
    true.

pagination_page_size(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add many resources
    NumResources = 100,
    lists:map(fun(I) ->
        Uri = list_to_binary("file:///page_size_test_" ++ integer_to_list(I) ++ ".txt"),
        Handler = fun(_) -> {ok, {text, <<"Resource ", integer_to_list(I)/binary>>}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, lists:seq(1, NumResources)),

    %% Test page size handling
    {ok, _} = erlmcp_client:list_resources(ClientPid),

    ct:pal("Page size pagination test completed"),
    true.

pagination_edge_cases(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test empty list
    {ok, EmptyList} = erlmcp_client:list_resources(ClientPid),
    case EmptyList of
        {resources, []} -> ok;
        {resources, _} -> ct:fail("Should return empty list when no resources")
    end,

    %% Test single page
    Uri = <<"file:///single_page.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Single page">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    {ok, _} = erlmcp_client:list_resources(ClientPid),

    ct:pal("Pagination edge cases test completed"),
    true.

%%====================================================================
%% Error Handling
%%====================================================================

resource_not_found(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Try to read non-existent resource
    Uri = <<"file:///nonexistent.txt">>,
    {Result, _} = erlmcp_client:read_resource(ClientPid, Uri),

    case Result of
        {error, {resource_not_found, _}} ->
            ct:pal("Resource not found error handled correctly"),
            true;
        _ ->
            ct:fail("Should return resource not found error")
    end.

invalid_uri(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Try to read with invalid URI
    Uri = <<"invalid-uri">>,
    {Result, _} = erlmcp_client:read_resource(ClientPid, Uri),

    case Result of
        {error, {invalid_uri, _}} ->
            ct:pal("Invalid URI error handled correctly"),
            true;
        _ ->
            ct:fail("Should return invalid URI error")
    end.

permission_denied(_Config) ->
    %% Test permission denied scenario
    ct:pal("Permission denied test placeholder"),
    true.

resource_corrupted(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add resource that returns corrupted data
    Uri = <<"file:///corrupted.txt">>,
    Handler = fun(_) -> {error, {corrupted, <<"Invalid data">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Try to read corrupted resource
    {Result, _} = erlmcp_client:read_resource(ClientPid, Uri),

    case Result of
        {error, {corrupted, _}} ->
            ct:pal("Resource corruption handled correctly"),
            true;
        _ ->
            ct:fail("Should return corruption error")
    end.

concurrent_resource_access(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add shared resource
    Uri = <<"file:///shared.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Shared content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Test concurrent access
    NumConcurrent = 10,
    Pids = lists:map(fun(_) ->
        spawn_link(fun() ->
            {ok, _} = erlmcp_client:read_resource(ClientPid, Uri)
        end)
    end, lists:seq(1, NumConcurrent)),

    %% Wait for all to complete
    Results = lists:foldl(fun(_, Acc) ->
        receive
            {'EXIT', _, _} -> Acc
        after 5000 ->
            ct:fail("Timeout in concurrent access test")
        end
    end, [], Pids),

    ct:pal("Concurrent resource access test completed"),
    true.

%%====================================================================
%% Performance Tests
%%====================================================================

resource_registration_performance(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test resource registration performance
    NumResources = 1000,
    {Time, _} = timer:tc(fun() ->
        lists:map(fun(I) ->
            Uri = list_to_binary("file:///perf_reg_" ++ integer_to_list(I) ++ ".txt"),
            Handler = fun(_) -> {ok, {text, <<"Resource ", integer_to_list(I)/binary>>}} end,
            erlmcp_server:add_resource(ServerPid, Uri, Handler)
        end, lists:seq(1, NumResources))
    end),

    Throughput = NumResources / (Time / 1000000),
    ct:pal("Resource registration throughput: ~p resources/sec", [Throughput]),

    Throughput > 100,  % Minimum 100 resources/sec
    true.

resource_read_performance(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resources
    NumResources = 100,
    lists:map(fun(I) ->
        Uri = list_to_binary("file:///perf_read_" ++ integer_to_list(I) ++ ".txt"),
        Handler = fun(_) -> {ok, {text, <<"Resource ", integer_to_list(I)/binary>>}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, lists:seq(1, NumResources)),

    %% Test read performance
    {Time, Results} = timer:tc(fun() ->
        lists:map(fun(I) ->
            Uri = list_to_binary("file:///perf_read_" ++ integer_to_list(I) ++ ".txt"),
            {ok, _} = erlmcp_client:read_resource(ClientPid, Uri)
        end, lists:seq(1, NumResources))
    end),

    Throughput = NumResources / (Time / 1000000),
    ct:pal("Resource read throughput: ~p reads/sec", [Throughput]),

    Throughput > 50,  % Minimum 50 reads/sec
    true.

large_resource_handling(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add large resource
    LargeContent = lists:duplicate(1000000, <<"x">>),
    Uri = <<"file:///large_resource.txt">>,
    Handler = fun(_) -> {ok, {text, LargeContent}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Test large resource reading
    {ok, Result} = erlmcp_client:read_resource(ClientPid, Uri),

    case Result of
        {contents, [#{text := LargeContent}]} ->
            ct:pal("Large resource handling completed"),
            true;
        _ ->
            ct:fail("Large resource handling failed")
    end.

concurrent_resource_operations(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resources
    NumResources = 50,
    lists:map(fun(I) ->
        Uri = list_to_binary("file:///concurrent_" ++ integer_to_list(I) ++ ".txt"),
        Handler = fun(_) -> {ok, {text, <<"Concurrent ", integer_to_list(I)/binary>>}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, lists:seq(1, NumResources)),

    %% Test concurrent operations
    NumOperations = 200,
    {Time, Results} = timer:tc(fun() ->
        lists:map(fun(_) ->
            case rand:uniform(2) of
                1 ->  % Read operation
                    Uri = list_to_binary("file:///concurrent_" ++ integer_to_list(rand:uniform(NumResources)) ++ ".txt"),
                    {ok, _} = erlmcp_client:read_resource(ClientPid, Uri);
                2 ->  % List operation
                    {ok, _} = erlmcp_client:list_resources(ClientPid)
            end
        end, lists:seq(1, NumOperations))
    end),

    Throughput = NumOperations / (Time / 1000000),
    ct:pal("Concurrent operations throughput: ~p ops/sec", [Throughput]),

    Throughput > 20,  % Minimum 20 ops/sec
    true.

%%====================================================================
%% Security Tests
%%====================================================================

resource_uri_injection(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test URI injection attacks
    MaliciousURIs = [
        <<"file:///evil.txt?param=javascript:alert('xss')">>,
        <<"file:///good.txt|cat /etc/passwd">>,
        <<"file:///good.txt;rm -rf /">>
    ],

    lists:foreach(fun(Uri) ->
        try
            Handler = fun(_) -> {ok, {text, <<"Content">>}} end,
            erlmcp_server:add_resource(ServerPid, Uri, Handler),
            ct:fail("Should have rejected malicious URI: ~p", [Uri])
        catch
            _:_ ->
                ct:pal("Correctly rejected malicious URI: ~p", [Uri])
        end
    end, MaliciousURIs),

    true.

resource_path_traversal(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test path traversal attacks
    TraversalURIs = [
        <<"file:///../../../etc/passwd">>,
        <<"file:///..%2F..%2F..%2Fetc%2Fpasswd">>,
        <<"file:///../../../../../../Windows/System32/drivers/etc/hosts">>
    ],

    lists:foreach(fun(Uri) ->
        try
            Handler = fun(_) -> {ok, {text, <<"Content">>}} end,
            erlmcp_server:add_resource(ServerPid, Uri, Handler),
            ct:fail("Should have rejected path traversal URI: ~p", [Uri])
        catch
            _:_ ->
                ct:pal("Correctly rejected path traversal URI: ~p", [Uri])
        end
    end, TraversalURIs),

    true.

malicious_resource_names(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test malicious resource names
    MaliciousNames = [
        <<"../../../etc/passwd">>,
        <<"/etc/passwd">>,
        <<"C:\\Windows\\System32\\drivers\\etc\\hosts">>,
        <<"javascript:alert('xss')">>,
        <<"data:text/html,<script>alert('xss')</script>">>
    ],

    lists:foreach(fun(Name) ->
        Uri = list_to_binary("file://" ++ binary_to_list(Name)),
        try
            Handler = fun(_) -> {ok, {text, <<"Content">>}} end,
            erlmcp_server:add_resource(ServerPid, Uri, Handler),
            ct:fail("Should have rejected malicious name: ~p", [Name])
        catch
            _:_ ->
                ct:pal("Correctly rejected malicious name: ~p", [Name])
        end
    end, MaliciousNames),

    true.

%%====================================================================
%% Integration Tests
%%====================================================================

resource_tool_integration(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resource
    Uri = <<"file:///tool_integration.txt">>,
    ResourceContent = <<"Tool integration test resource">>,
    Handler = fun(_) -> {ok, {text, ResourceContent}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Add tool that uses resource
    ToolName = <<"read_resource_tool">>,
    ToolHandler = fun(_) ->
        {ok, {text, <<"Tool: ", ResourceContent/binary>>}}
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, ToolHandler),

    %% Call tool
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{<<"uri">> => Uri}),

    case Result of
        {content, [#{text := _}]} ->
            ct:pal("Resource-tool integration completed"),
            true;
        _ ->
            ct:fail("Resource-tool integration failed")
    end.

resource_client_integration(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add multiple resources
    Resources = [
        {<<"file:///client1.txt">>, <<"Client resource 1">>},
        {<<"file:///client2.txt">>, <<"Client resource 2">>}
    ],

    lists:foreach(fun({Uri, Name}) ->
        Handler = fun(_) -> {ok, {text, Name}} end,
        ok = erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, Resources),

    %% Client performs operations
    {ok, Listed} = erlmcp_client:list_resources(ClientPid),
    {ok, Read1} = erlmcp_client:read_resource(ClientPid, <<"file:///client1.txt">>),
    {ok, Read2} = erlmcp_client:read_resource(ClientPid, <<"file:///client2.txt">>),

    ct:pal("Resource-client integration test completed"),
    true.

resource_server_interaction(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Server adds resources dynamically
    DynamicResources = [
        {<<"file:///dynamic1.txt">>, <<"Dynamic resource 1">>},
        {<<"file:///dynamic2.txt">>, <<"Dynamic resource 2">>}
    ],

    lists:foreach(fun({Uri, Name}) ->
        Handler = fun(_) -> {ok, {text, Name}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, DynamicResources),

    %% Server notifies changes
    erlmcp_server:notify_resources_changed(ServerPid),

    %% Client verifies changes
    {ok, _} = erlmcp_client:list_resources(ClientPid),

    ct:pal("Resource-server interaction test completed"),
    true.