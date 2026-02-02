%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_resource - CLI Resource Manager
%%%
%%% Implements MCP resource management functionality including
%%% resource listing, reading, and subscription management.
%%% Provides full MCP compliance for resource operations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_resource).

-behaviour(gen_server).

%% API
-export([start_link/0, list_resources/0, list_resources/1,
         get_resource/2, get_resource/3, subscribe_resource/2,
         subscribe_resource/3, unsubscribe_resource/2,
         unsubscribe_resource/3, list_subscriptions/0,
         create_resource/2, update_resource/3, delete_resource/2,
         search_resources/1, validate_resource/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(resource_state,
        {resources :: map(),                    % Available resources
         subscriptions :: map(),                 % Active subscriptions
         handlers :: map(),                      % Subscription handlers
         registry :: map(),                      % Resource registry
         metrics :: map(),                       % Resource metrics
         polling :: boolean(),                   % Polling enabled
         poll_interval :: integer(),              % Polling interval
         watchers :: list()}).                   % Resource watchers

%% Resource schema
-define(RESOURCE_SCHEMA,
        #{<<"name">> => {binary, required},
          <<"uri">> => {binary, required},
          <<"description">> => {binary, optional},
          <<"mimeType">> => {binary, optional},
          <<"file">> => {binary, optional},
          <<"contents">> => {binary, optional},
          <<"schema">> => {map, optional},
          <<"dynamic">> => {boolean, optional}}).

%% Default resource configuration
-define(DEFAULT_CONFIG,
        #{<<"poll_interval">> => 5000,
          <<"enable_polling">> => true,
          <<"cache_ttl">> => 30000,
         <<"max_subscriptions">> => 100,
         <<"resource_timeout">> => 30000}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the resource manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc List all available resources
-spec list_resources() -> {ok, list()} | {error, term()}.
list_resources() ->
    gen_server:call(?SERVER, list_resources, 5000).

%% @doc List resources with filtering
-spec list_resources(map()) -> {ok, list()} | {error, term()}.
list_resources(Filters) ->
    gen_server:call(?SERVER, {list_resources, Filters}, 5000).

%% @doc Get resource by name
-spec get_resource(binary(), binary()) -> {ok, map()} | {error, term()}.
get_resource(Uri, Selector) ->
    gen_server:call(?SERVER, {get_resource, Uri, Selector}, 5000).

%% @doc Get resource by name with timeout
-spec get_resource(binary(), binary(), integer()) -> {ok, map()} | {error, term()}.
get_resource(Uri, Selector, Timeout) ->
    gen_server:call(?SERVER, {get_resource, Uri, Selector, Timeout}, Timeout).

%% @doc Subscribe to resource updates
-spec subscribe_resource(binary(), binary()) -> ok | {error, term()}.
subscribe_resource(Uri, Selector) ->
    subscribe_resource(Uri, Selector, #{}).

%% @doc Subscribe to resource updates with options
-spec subscribe_resource(binary(), binary(), map()) -> ok | {error, term()}.
subscribe_resource(Uri, Selector, Options) ->
    gen_server:call(?SERVER, {subscribe_resource, Uri, Selector, Options}, 5000).

%% @doc Unsubscribe from resource updates
-spec unsubscribe_resource(binary(), binary()) -> ok | {error, term()}.
unsubscribe_resource(Uri, Selector) ->
    gen_server:call(?SERVER, {unsubscribe_resource, Uri, Selector}, 5000).

%% @doc Unsubscribe from resource updates with reason
-spec unsubscribe_resource(binary(), binary(), binary()) -> ok | {error, term()}.
unsubscribe_resource(Uri, Selector, Reason) ->
    gen_server:call(?SERVER, {unsubscribe_resource, Uri, Selector, Reason}, 5000).

%% @doc List all subscriptions
-spec list_subscriptions() -> {ok, list()} | {error, term()}.
list_subscriptions() ->
    gen_server:call(?SERVER, list_subscriptions, 5000).

%% @doc Create new resource
-spec create_resource(binary(), map()) -> ok | {error, term()}.
create_resource(Uri, ResourceData) ->
    gen_server:call(?SERVER, {create_resource, Uri, ResourceData}, 10000).

%% @doc Update existing resource
-spec update_resource(binary(), binary(), map()) -> ok | {error, term()}.
update_resource(Uri, Selector, ResourceData) ->
    gen_server:call(?SERVER, {update_resource, Uri, Selector, ResourceData}, 10000).

%% @doc Delete resource
-spec delete_resource(binary(), binary()) -> ok | {error, term()}.
delete_resource(Uri, Selector) ->
    gen_server:call(?SERVER, {delete_resource, Uri, Selector}, 5000).

%% @doc Search resources
-spec search_resources(binary()) -> {ok, list()} | {error, term()}.
search_resources(Query) ->
    gen_server:call(?SERVER, {search_resources, Query}, 5000).

%% @doc Validate resource
-spec validate_resource(map()) -> ok | {error, term()}.
validate_resource(Resource) ->
    gen_server:call(?SERVER, {validate_resource, Resource}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the resource manager
-spec init(term()) -> {ok, #resource_state{}} | {stop, term()}.
init(_Args) ->
    %% Create OTEL span for resource manager initialization
    erlmcp_otel:with_span("cli.resource.init",
                          #{<<"module">> => atom_to_binary(?MODULE, utf8)},
                          fun() ->
                             %% Initialize state
                             DefaultConfig = ?DEFAULT_CONFIG,

                             %% Load available resources
                             Resources = load_resources(),

                             %% Initialize subscription management
                             Subscriptions = #{},
                             Handlers = #{},

                             %% Initialize metrics
                             Metrics = init_metrics(),

                             State = #resource_state{
                                resources = Resources,
                                subscriptions = Subscriptions,
                                handlers = Handlers,
                                registry = create_registry(Resources),
                                metrics = Metrics,
                                polling = maps:get(<<"enable_polling">>, DefaultConfig, true),
                                poll_interval = maps:get(<<"poll_interval">>, DefaultConfig, 5000),
                                watchers = []
                             },

                             %% Start polling timer if enabled
                            case State#resource_state.polling of
                                true ->
                                    erlang:send_after(State#resource_state.poll_interval, self(), poll_resources);
                                false ->
                                    ok
                            end,

                             erlmcp_metrics:record("cli.resource.initialized", 1),
                             {ok, State}
                          end).

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #resource_state{}) ->
                   {reply, term(), #resource_state{}}.
handle_call(list_resources, _From, State) ->
    %% Create OTEL span for listing resources
    SpanCtx = erlmcp_otel:inject_span("cli.resource.list", #{}, undefined),

    ResourceList = maps:values(State#resource_state.resources),
    erlmcp_otel:record_event(SpanCtx, <<"resources.listed">>, #{<<"count">> => length(ResourceList)}),

    erlmcp_metrics:record("cli.resource.list.success", 1),
    {reply, {ok, ResourceList}, State};

handle_call({list_resources, Filters}, _From, State) ->
    %% Create OTEL span for filtered resource listing
    SpanCtx = erlmcp_otel:inject_span("cli.resource.list_filtered",
                                     #{<<"filters">> => Filters},
                                     undefined),

    FilteredResources = filter_resources(State#resource_state.resources, Filters),
    erlmcp_otel:record_event(SpanCtx, <<"resources.filtered">>,
                          #{<<"count">> => length(FilteredResources)}),

    erlmcp_metrics:record("cli.resource.list_filtered.success", 1),
    {reply, {ok, FilteredResources}, State};

handle_call({get_resource, Uri, Selector}, _From, State) ->
    handle_call({get_resource, Uri, Selector, maps:get(<<"resource_timeout">>, ?DEFAULT_CONFIG, 30000)}, _From, State);

handle_call({get_resource, Uri, Selector, Timeout}, _From, State) ->
    %% Create OTEL span for resource retrieval
    SpanCtx = erlmcp_otel:inject_span("cli.resource.get",
                                     #{<<"uri">> => Uri, <<"selector">> => Selector},
                                     undefined),

    try
        case find_resource(Uri, Selector, State) of
            {ok, Resource} ->
                erlmcp_otel:record_event(SpanCtx, <<"resource.retrieved">>, #{}),
                erlmcp_metrics:record("cli.resource.get.success", 1),
                {reply, {ok, Resource}, State};
            {error, not_found} ->
                erlmcp_otel:record_error(SpanCtx, {resource_not_found, Uri, Selector}),
                erlmcp_metrics:record("cli.resource.get.not_found", 1),
                {reply, {error, not_found}, State};
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {resource_get_failed, Uri, Selector, Reason}),
                erlmcp_metrics:record("cli.resource.get.error", 1),
                {reply, {error, Reason}, State}
        end
    after
        %% Ensure timeout is enforced
        case erlang:monotonic_time(millisecond) - erlang:system_time(millisecond) > Timeout of
            true ->
                erlmcp_otel:record_error(SpanCtx, {resource_timeout, Uri, Selector}),
                {reply, {error, timeout}, State};
            false ->
                ok
        end
    end;

handle_call({subscribe_resource, Uri, Selector, Options}, _From, State) ->
    %% Create OTEL span for resource subscription
    SpanCtx = erlmcp_otel:inject_span("cli.resource.subscribe",
                                     #{<<"uri">> => Uri, <<"selector">> => Selector},
                                     undefined),

    try
        %% Validate resource exists
        case find_resource(Uri, Selector, State) of
            {ok, Resource} ->
                %% Create subscription ID
                SubscriptionId = generate_subscription_id(),

                %% Create subscription handler
                Handler = create_subscription_handler(SubscriptionId, Uri, Selector, Options),

                %% Store subscription
                NewSubscriptions = maps:put(SubscriptionId, {Uri, Selector, Options, erlang:system_time(millisecond)},
                                          State#resource_state.subscriptions),

                %% Store handler
                NewHandlers = maps:put(SubscriptionId, Handler, State#resource_state.handlers),

                %% Start handler
                case start_handler(Handler, Resource) of
                    ok ->
                        erlmcp_otel:record_event(SpanCtx, <<"resource.subscribed">>, #{}),
                        erlmcp_metrics:record("cli.resource.subscribe.success", 1),
                        {reply, ok, State#resource_state{subscriptions = NewSubscriptions, handlers = NewHandlers}};
                    {error, Reason} ->
                        erlmcp_otel:record_error(SpanCtx, {subscription_start_failed, Uri, Selector, Reason}),
                        erlmcp_metrics:record("cli.resource.subscribe.error", 1),
                        {reply, {error, Reason}, State}
                end;
            {error, not_found} ->
                erlmcp_otel:record_error(SpanCtx, {resource_not_found, Uri, Selector}),
                erlmcp_metrics:record("cli.resource.subscribe.not_found", 1),
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {subscription_failed, Uri, Selector, Error, Reason}),
            erlmcp_metrics:record("cli.resource.subscribe.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({unsubscribe_resource, Uri, Selector}, _From, State) ->
    handle_call({unsubscribe_resource, Uri, Selector, <<"unsubscribed">>}, _From, State);

handle_call({unsubscribe_resource, Uri, Selector, Reason}, _From, State) ->
    %% Create OTEL span for resource unsubscription
    SpanCtx = erlmcp_otel:inject_span("cli.resource.unsubscribe",
                                     #{<<"uri">> => Uri, <<"selector">> => Selector},
                                     undefined),

    try
        case find_subscription(Uri, Selector, State) of
            {ok, SubscriptionId} ->
                %% Remove subscription
                NewSubscriptions = maps:remove(SubscriptionId, State#resource_state.subscriptions),

                %% Stop handler
                case maps:find(SubscriptionId, State#resource_state.handlers) of
                    {ok, Handler} ->
                        stop_handler(Handler),
                        NewHandlers = maps:remove(SubscriptionId, State#resource_state.handlers);
                    error ->
                        NewHandlers = State#resource_state.handlers
                end,

                %% Notify watchers
                notify_watchers(unsubscribe, Uri, Selector, Reason),

                erlmcp_otel:record_event(SpanCtx, <<"resource.unsubscribed">>, #{}),
                erlmcp_metrics:record("cli.resource.unsubscribe.success", 1),

                {reply, ok, State#resource_state{subscriptions = NewSubscriptions, handlers = NewHandlers}};
            {error, not_found} ->
                erlmcp_otel:record_error(SpanCtx, {subscription_not_found, Uri, Selector}),
                erlmcp_metrics:record("cli.resource.unsubscribe.not_found", 1),
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {unsubscription_failed, Uri, Selector, Error, Reason}),
            erlmcp_metrics:record("cli.resource.unsubscribe.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call(list_subscriptions, _From, State) ->
    Subscriptions = lists:map(fun({SubId, {Uri, Selector, Options, Timestamp}}) ->
                                   #{<<"id">> => SubId,
                                     <<"uri">> => Uri,
                                     <<"selector">> => Selector,
                                     <<"options">> => Options,
                                     <<"timestamp">> => Timestamp}
                               end, maps:to_list(State#resource_state.subscriptions)),
    {reply, {ok, Subscriptions}, State};

handle_call({create_resource, Uri, ResourceData}, _From, State) ->
    %% Create OTEL span for resource creation
    SpanCtx = erlmcp_otel:inject_span("cli.resource.create",
                                     #{<<"uri">> => Uri, <<"data_size">> => map_size(ResourceData)},
                                     undefined),

    try
        %% Validate resource data
        case validate_resource(ResourceData) of
            ok ->
                %% Create new resource
                Resource = create_resource_object(Uri, ResourceData),

                %% Add to resources
                NewResources = maps:put(Uri, Resource, State#resource_state.resources),

                %% Update registry
                NewRegistry = create_registry(NewResources),

                %% Notify watchers
                notify_watchers(create, Uri, Resource),

                erlmcp_otel:record_event(SpanCtx, <<"resource.created">>, #{}),
                erlmcp_metrics:record("cli.resource.create.success", 1),

                {reply, ok, State#resource_state{resources = NewResources, registry = NewRegistry}};
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {resource_validation_failed, Uri, Reason}),
                erlmcp_metrics:record("cli.resource.create.validation_error", 1),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {resource_create_failed, Uri, Error, Reason}),
            erlmcp_metrics:record("cli.resource.create.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({update_resource, Uri, Selector, ResourceData}, _From, State) ->
    %% Create OTEL span for resource update
    SpanCtx = erlmcp_otel:inject_span("cli.resource.update",
                                     #{<<"uri">> => Uri, <<"selector">> => Selector},
                                     undefined),

    try
        %% Validate resource data
        case validate_resource(ResourceData) of
            ok ->
                %% Find existing resource
                case find_resource(Uri, Selector, State) of
                    {ok, OldResource} ->
                        %% Update resource
                        NewResource = update_resource_object(OldResource, ResourceData),

                        %% Update resources
                        NewResources = maps:put(Uri, NewResource, State#resource_state.resources),

                        %% Update registry
                        NewRegistry = create_registry(NewResources),

                        NotifyExistingSubscriptions = NewResources /= State#resource_state.resources,

                        %% Notify watchers
                        notify_watchers(update, Uri, NewResource),

                        %% Notify subscribers of changes
                        case NotifyExistingSubscriptions of
                            true ->
                                notify_subscribers(Uri, Selector, NewResource);
                            false ->
                                ok
                        end,

                        erlmcp_otel:record_event(SpanCtx, <<"resource.updated">>, #{}),
                        erlmcp_metrics:record("cli.resource.update.success", 1),

                        {reply, ok, State#resource_state{resources = NewResources, registry = NewRegistry}};
                    {error, not_found} ->
                        erlmcp_otel:record_error(SpanCtx, {resource_not_found, Uri, Selector}),
                        erlmcp_metrics:record("cli.resource.update.not_found", 1),
                        {reply, {error, not_found}, State}
                end;
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {resource_validation_failed, Uri, Reason}),
                erlmcp_metrics:record("cli.resource.update.validation_error", 1),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {resource_update_failed, Uri, Selector, Error, Reason}),
            erlmcp_metrics:record("cli.resource.update.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({delete_resource, Uri, Selector}, _From, State) ->
    %% Create OTEL span for resource deletion
    SpanCtx = erlmcp_otel:inject_span("cli.resource.delete",
                                     #{<<"uri">> => Uri, <<"selector">> => Selector},
                                     undefined),

    try
        case find_resource(Uri, Selector, State) of
            {ok, Resource} ->
                %% Remove resource
                NewResources = maps:remove(Uri, State#resource_state.resources),

                UnsubscribeAffectedSubscriptions = maps:remove(Uri, State#resource_state.subscriptions),

                %% Stop handlers for affected subscriptions
                lists:foreach(fun(SubId) ->
                                   case maps:find(SubId, State#resource_state.handlers) of
                                       {ok, Handler} -> stop_handler(Handler);
                                       error -> ok
                                   end
                               end, maps:keys(UnsubscribeAffectedSubscriptions)),

                %% Update registry
                NewRegistry = create_registry(NewResources),

                %% Notify watchers
                notify_watchers(delete, Uri, Resource),

                erlmcp_otel:record_event(SpanCtx, <<"resource.deleted">>, #{}),
                erlmcp_metrics:record("cli.resource.delete.success", 1),

                {reply, ok, State#resource_state{resources = NewResources, registry = NewRegistry}};
            {error, not_found} ->
                erlmcp_otel:record_error(SpanCtx, {resource_not_found, Uri, Selector}),
                erlmcp_metrics:record("cli.resource.delete.not_found", 1),
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {resource_delete_failed, Uri, Selector, Error, Reason}),
            erlmcp_metrics:record("cli.resource.delete.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({search_resources, Query}, _From, State) ->
    %% Create OTEL span for resource search
    SpanCtx = erlmcp_otel:inject_span("cli.resource.search",
                                     #{<<"query">> => Query},
                                     undefined),

    try
        SearchResults = search_resources_internal(State#resource_state.resources, Query),
        erlmcp_otel:record_event(SpanCtx, <<"resources.searched">>, #{<<"count">> => length(SearchResults)}),
        erlmcp_metrics:record("cli.resource.search.success", 1),
        {reply, {ok, SearchResults}, State}
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {resource_search_failed, Query, Error, Reason}),
            erlmcp_metrics:record("cli.resource.search.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({validate_resource, Resource}, _From, State) ->
    case validate_resource_schema(Resource) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #resource_state{}) -> {noreply, #resource_state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #resource_state{}) -> {noreply, #resource_state{}}.
handle_info(poll_resources, State) ->
    %% Create OTEL span for resource polling
    SpanCtx = erlmcp_otel:inject_span("cli.resource.poll", #{}, undefined),

    try
        %% Poll all dynamic resources
        PollingResults = poll_dynamic_resources(State#resource_state.resources),

        %% Check for changes and notify subscribers
        lists:foreach(fun({Uri, Selector, NewResource}) ->
                             notify_subscribers(Uri, Selector, NewResource)
                         end, PollingResults),

        erlmcp_otel:record_event(SpanCtx, <<"resources.polled">>,
                              #{<<"polled_count">> => length(PollingResults)}),
        erlmcp_metrics:record("cli.resource.poll.success", 1)
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {resource_poll_failed, Error, Reason}),
            erlmcp_metrics:record("cli.resource.poll.error", 1)
    end,

    %% Schedule next poll
    case State#resource_state.polling of
        true ->
            erlang:send_after(State#resource_state.poll_interval, self(), poll_resources);
        false ->
            ok
    end,

    {noreply, State};

handle_info({resource_update, Uri, NewResource}, State) ->
    %% External resource update notification
    NewResources = maps:put(Uri, NewResource, State#resource_state.resources),
    NewRegistry = create_registry(NewResources),

    %% Notify watchers and subscribers
    notify_watchers(update, Uri, NewResource),
    notify_subscribers(Uri, undefined, NewResource),

    {noreply, State#resource_state{resources = NewResources, registry = NewRegistry}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #resource_state{}) -> ok.
terminate(_Reason, State) ->
    %% Create OTEL span for resource manager termination
    erlmcp_otel:with_span("cli.resource.terminate",
                          #{<<"resource_count">> => map_size(State#resource_state.resources),
                            <<"subscription_count">> => map_size(State#resource_state.subscriptions)},
                          fun() ->
                             %% Stop all subscription handlers
                            lists:foreach(fun({_, Handler}) ->
                                             stop_handler(Handler)
                                         end, maps:to_list(State#resource_state.handlers)),

                             %% Notify watchers of shutdown
                             notify_watchers(terminated, undefined, undefined),

                             %% Record final metrics
                             erlmcp_metrics:record("cli.resource.terminated", 1),

                             ok
                          end).

%% @doc Handle code changes
-spec code_change(term(), #resource_state{}, term()) -> {ok, #resource_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Load available resources
-spec load_resources() -> map().
load_resources() ->
    try
        %% Load resources from file system
        FileResources = load_file_resources(),

        %% Load resources from environment
        EnvResources = load_env_resources(),

        %% Load resources from registry
        RegistryResources = load_registry_resources(),

        %% Merge all resources
        maps:merge(maps:merge(FileResources, EnvResources), RegistryResources)
    catch
        Error:Reason ->
            lager:warning("Failed to load resources: ~p:~p", [Error, Reason]),
            #{}
    end.

%% @doc Load file resources
-spec load_file_resources() -> map().
load_file_resources() ->
    Resources = #{},

    %% Load from .erlmcp_resources directory
    ResourcesDir = filename:join(os:getenv("HOME", "."), ".erlmcp_resources"),
    case file:list_dir(ResourcesDir) of
        {ok, Files} ->
            lists:foldl(fun(File, Acc) ->
                             case filename:extension(File) of
                                 <<".json">> ->
                                     FilePath = filename:join(ResourcesDir, File),
                                     case file:read_file(FilePath) of
                                         {ok, Content} ->
                                             Resource = jsx:decode(Content, [{labels, binary}, return_maps]),
                                             maps:put(File, Resource, Acc);
                                         {error, _} -> Acc
                                     end;
                                 _ -> Acc
                             end
                         end, Resources, Files);
        {error, _} -> Resources
    end.

%% @doc Load environment resources
-spec load_env_resources() -> map().
load_env_resources() ->
    Resources = #{},

    %% Load from environment variables
    case os:getenv("ERLMCP_RESOURCES") of
        false -> Resources;
        JsonData ->
            try
                jsx:decode(list_to_binary(JsonData), [{labels, binary}, return_maps])
            catch
                _ -> Resources
            end
    end.

%% @doc Load registry resources
-spec load_registry_resources() -> map().
load_registry_resources() ->
    try
        case erlmcp_registry:find_resources() of
            {ok, Resources} when is_list(Resources) ->
                lists:foldl(fun(Resource, Acc) ->
                                 Uri = maps:get(<<"uri">>, Resource),
                                 maps:put(Uri, Resource, Acc)
                             end, #{}, Resources);
            {error, _} -> #{}
        end
    catch
        _ -> #{}
    end.

%% @doc Create resource registry
-spec create_registry(map()) -> map().
create_registry(Resources) ->
    lists:foldl(fun(Uri, Acc) ->
                     case maps:find(Uri, Resources) of
                         {ok, Resource} ->
                             %% Create searchable index
                             Index = create_resource_index(Resource),
                             maps:put(Uri, Index, Acc);
                         error -> Acc
                     end
                 end, #{}, maps:keys(Resources)).

%% @doc Create resource index
-spec create_resource_index(map()) -> map().
create_resource_index(Resource) ->
    #{<<"uri">> => maps:get(<<"uri">>, Resource),
      <<"name">> => maps:get(<<"name">>, Resource),
      <<"description">> => maps:get(<<"description">>, Resource, <<>>),
      <<"mimeType">> => maps:get(<<"mimeType">>, Resource, <<"application/json">>),
      <<"tags">> => extract_tags(Resource),
      <<"searchable_text">>> => create_searchable_text(Resource)}.

%% @doc Create searchable text
-spec create_searchable_text(map()) -> binary().
create_searchable_text(Resource) ->
    Name = maps:get(<<"name">>, Resource, <<>>),
    Description = maps:get(<<"description">>, Resource, <<>>),
    Contents = maps:get(<<"contents">>, Resource, <<>>),

    Text = <<Name/binary, " ", Description/binary, " ", Contents/binary>>,
    string:lowercase(Text).

%% @doc Filter resources
-spec filter_resources(map(), map()) -> list().
filter_resources(Resources, Filters) ->
    lists:filter(fun(Uri) ->
                     case maps:find(Uri, Resources) of
                         {ok, Resource} -> matches_filters(Resource, Filters);
                         error -> false
                     end
                 end, maps:keys(Resources)).

%% @doc Check if resource matches filters
-spec matches_filters(map(), map()) -> boolean().
matches_filters(Resource, Filters) ->
    lists:all(fun({FilterKey, FilterValue}) ->
                     case maps:find(FilterKey, Resource) of
                         {ok, ResourceValue} -> ResourceValue == FilterValue;
                         error -> false
                     end
                 end, maps:to_list(Filters)).

%% @doc Find resource
-spec find_resource(binary(), binary(), #resource_state{}) -> {ok, map()} | {error, term()}.
find_resource(Uri, Selector, State) ->
    case maps:find(Uri, State#resource_state.resources) of
        {ok, Resource} ->
            %% Apply selector if provided
            case Selector of
                undefined -> {ok, Resource};
                _ -> apply_selector(Resource, Selector)
            end;
        error ->
            {error, not_found}
    end.

%% @doc Apply selector to resource
-spec apply_selector(map(), binary()) -> {ok, map()} | {error, term()}.
apply_selector(Resource, Selector) ->
    try
        %% Simple JSONPath-like selector
        case binary:split(Selector, <<"/">>, [global]) of
            [Key] ->
                case maps:find(Key, Resource) of
                    {ok, Value} -> {ok, Value};
                    error -> {error, {key_not_found, Key}}
                end;
            Parts ->
                apply_selector_parts(Parts, Resource)
        end
    catch
        Error:Reason ->
            {error, {selector_error, Error, Reason}}
    end.

%% @doc Apply selector parts
-spec apply_selector_parts([binary()], map()) -> {ok, map()} | {error, term()}.
apply_selector_parts([Part], Map) ->
    case maps:find(Part, Map) of
        {ok, Value} -> {ok, Value};
        error -> {error, {key_not_found, Part}}
    end;
apply_selector_parts([Part|Rest], Map) ->
    case maps:find(Part, Map) of
        {ok, SubMap} when is_map(SubMap) ->
            apply_selector_parts(Rest, SubMap);
        {ok, _} ->
            {error, {invalid_path, Part}};
        error ->
            {error, {key_not_found, Part}}
    end.

%% @doc Create subscription handler
-spec create_subscription_handler(binary(), binary(), binary(), map()) -> map().
create_subscription_handler(SubscriptionId, Uri, Selector, Options) ->
    #{<<"id">> => SubscriptionId,
      <<"uri">> => Uri,
      <<"selector">> => Selector,
      <<"options">> => Options,
      <<"created">> => erlang:system_time(millisecond),
      <<"type">> => case maps:get(<<"type">>, Options, <<"webhook">>) of
                       <<"webhook">> -> webhook;
                       <<"poll">> -> poll;
                       <<"event">> -> event;
                       _ -> webhook
                   end}.

%% @doc Start subscription handler
-spec start_handler(map(), map()) -> ok | {error, term()}.
start_handler(Handler, Resource) ->
    case maps:get(<<"type">>, Handler) of
        webhook -> start_webhook_handler(Handler, Resource);
        poll -> start_poll_handler(Handler, Resource);
        event -> start_event_handler(Handler, Resource);
        _ -> {error, unsupported_handler_type}
    end.

%% @doc Start webhook handler
-spec start_webhook_handler(map(), map()) -> ok | {error, term()}.
start_webhook_handler(Handler, _Resource) ->
    try
        %% Start webhook receiver process
        {ok, Pid} = erlmcp_webhook_sup:start_child(Handler),
        ok
    catch
        Error:Reason ->
            {error, {webhook_start_failed, Error, Reason}}
    end.

%% @doc Start poll handler
-spec start_poll_handler(map(), map()) -> ok | {error, term()}.
start_poll_handler(Handler, _Resource) ->
    try
        %% Start polling process
        {ok, Pid} = erlmcp_poll_sup:start_child(Handler),
        ok
    catch
        Error:Reason ->
            {error, {poll_start_failed, Error, Reason}}
    end.

%% @doc Start event handler
-spec start_event_handler(map(), map()) -> ok | {error, term()}.
start_event_handler(Handler, _Resource) ->
    try
        %% Start event listener process
        {ok, Pid} = erlmcp_event_sup:start_child(Handler),
        ok
    catch
        Error:Reason ->
            {error, {event_start_failed, Error, Reason}}
    end.

%% @doc Stop subscription handler
-spec stop_handler(map()) -> ok.
stop_handler(Handler) ->
    case maps:get(<<"type">>, Handler) of
        webhook -> erlmcp_webhook_sup:stop_child(Handler);
        poll -> erlmcp_poll_sup:stop_child(Handler);
        event -> erlmcp_event_sup:stop_child(Handler);
        _ -> ok
    end.

%% @doc Notify subscribers
-spec notify_subscribers(binary(), binary(), map()) -> ok.
notify_subscribers(Uri, Selector, NewResource) ->
    try
        %% Send update to all subscribers
        erlmcp_observability:notify_subscribers(Uri, Selector, NewResource),
        ok
    catch
        Error:Reason ->
            lager:warning("Failed to notify subscribers: ~p:~p", [Error, Reason]),
            ok
    end.

%% @doc Notify watchers
-spec notify_watchers(atom(), binary(), map()) -> ok.
notify_watchners(Event, Uri, Resource) ->
    lists:foreach(fun(Watcher) ->
                     Watcher ! {resource_event, Event, Uri, Resource}
                 end, get_watchers()).

%% @doc Poll dynamic resources
-spec poll_dynamic_resources(map()) -> list().
poll_dynamic_resources(Resources) ->
    lists:foldl(fun(Uri, Acc) ->
                     case maps:find(Uri, Resources) of
                         {ok, Resource} ->
                             case maps:get(<<"dynamic">>, Resource, false) of
                                 true ->
                                     case poll_resource(Resource) of
                                         {ok, NewResource} -> [{Uri, undefined, NewResource} | Acc];
                                         {error, _} -> Acc
                                     end;
                                 false -> Acc
                             end;
                         error -> Acc
                     end
                 end, [], maps:keys(Resources)).

%% @doc Poll individual resource
-spec poll_resource(map()) -> {ok, map()} | {error, term()}.
poll_resource(Resource) ->
    try
        %% Implement resource-specific polling logic
        case maps:get(<<"uri">>, Resource) of
            <<"http://", _/binary>> ->
                poll_http_resource(Resource);
            <<"https://", _/binary>> ->
                poll_http_resource(Resource);
            _ ->
                poll_local_resource(Resource)
        end
    catch
        Error:Reason ->
            {error, {poll_failed, Error, Reason}}
    end.

%% @doc Poll HTTP resource
-spec poll_http_resource(map()) -> {ok, map()} | {error, term()}.
poll_http_resource(Resource) ->
    Uri = maps:get(<<"uri">>, Resource),
    try
        case httpc:request(get, {Uri, []}, [], []) of
            {ok, {{_, 200, _}, _, Body}} ->
                UpdatedResource = Resource#{<<"contents">> => list_to_binary(Body)},
                {ok, UpdatedResource};
            {ok, {{_, StatusCode, _}, _, _}} ->
                {error, {http_error, StatusCode}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Error:Reason ->
            {error, {http_poll_error, Error, Reason}}
    end.

%% @doc Poll local resource
-spec poll_local_resource(map()) -> {ok, map()} | {error, term()}.
poll_local_resource(Resource) ->
    case maps:get(<<"file">>, Resource) of
        undefined ->
            {error, no_file_specified};
        FilePath ->
            try
                case file:read_file(FilePath) of
                    {ok, Content} ->
                        UpdatedResource = Resource#{<<"contents">> => Content},
                        {ok, UpdatedResource};
                    {error, Reason} ->
                        {error, Reason}
                end
            catch
                Error:Reason ->
                    {error, {file_poll_error, Error, Reason}}
            end
    end.

%% @doc Search resources internal
-spec search_resources_internal(map(), binary()) -> list().
search_resources_internal(Resources, Query) ->
    QueryLower = string:lowercase(Query),
    lists:foldl(fun(Uri, Acc) ->
                     case maps:find(Uri, Resources) of
                         {ok, Resource} ->
                             case matches_search_query(Resource, QueryLower) of
                                 true -> [Resource | Acc];
                                 false -> Acc
                             end;
                         error -> Acc
                     end
                 end, [], maps:keys(Resources)).

%% @doc Check if resource matches search query
-spec matches_search_query(map(), binary()) -> boolean().
matches_search_query(Resource, Query) ->
    SearchableText = maps:get(<<"searchable_text">>, Resource, <<>>),
    case string:find(SearchableText, Query) of
        nomatch -> false;
        _ -> true
    end.

%% @doc Extract tags from resource
-spec extract_tags(map()) -> list().
extract_tags(Resource) ->
    Tags = maps:get(<<"tags">>, Resource, []),
    case is_list(Tags) of
        true -> Tags;
        false -> []
    end.

%% @doc Create resource object
-spec create_resource_object(binary(), map()) -> map().
create_resource_object(Uri, ResourceData) ->
    Defaults = #{<<"name">> => Uri,
                <<"uri">> => Uri,
                <<"description">> => <<>>,
                <<"mimeType">> => <<"application/json">>,
                <<"contents">> => <<>>,
                <<"dynamic">> => false,
                <<"created">> => erlang:system_time(millisecond)},
    maps:merge(Defaults, ResourceData).

%% @doc Update resource object
-spec update_resource_object(map(), map()) -> map().
update_resource_object(OldResource, Updates) ->
    maps:merge(OldResource, Updates).

%% @doc Generate subscription ID
-spec generate_subscription_id() -> binary().
generate_subscription_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).

%% @doc Find subscription
-spec find_subscription(binary(), binary(), #resource_state{}) -> {ok, binary()} | {error, term()}.
find_subscription(Uri, Selector, State) ->
    lists:foldl(fun({SubId, {SubUri, SubSelector, _, _}} = Subscription, Acc) ->
                     if SubUri == Uri, SubSelector == Selector -> {ok, SubId};
                     true -> Acc
                     end
                 end, {error, not_found}, maps:to_list(State#resource_state.subscriptions)).

%% @doc Validate resource schema
-spec validate_resource_schema(map()) -> ok | {error, term()}.
validate_resource_schema(Resource) ->
    %% Validate required fields
    case maps:find(<<"name">>, Resource) of
        {ok, Name} when is_binary(Name) ->
            ok;
        {ok, _} ->
            {error, {invalid_field, <<"name">>, must_be_binary}};
        error ->
            {error, {missing_required_field, <<"name">>}}
    end,

    case maps:find(<<"uri">>, Resource) of
        {ok, Uri} when is_binary(Uri) ->
            ok;
        {ok, _} ->
            {error, {invalid_field, <<"uri">>, must_be_binary}};
        error ->
            {error, {missing_required_field, <<"uri">>}}
    end,

    ok.

%% @doc Initialize metrics
-spec init_metrics() -> map().
init_metrics() ->
    #{"resources.count" => 0,
      "resources.get" => 0,
      "resources.create" => 0,
      "resources.update" => 0,
      "resources.delete" => 0,
      "subscriptions.count" => 0,
      "subscriptions.created" => 0,
      "subscriptions.removed" => 0,
      "searches.performed" => 0}.