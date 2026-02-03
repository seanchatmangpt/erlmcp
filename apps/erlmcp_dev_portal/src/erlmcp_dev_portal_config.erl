-module(erlmcp_dev_portal_config).

-behaviour(gen_server).

%% API exports
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions for configuration
-export([get_config/0, get_config/1, update_config/2, reload_config/0]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Initialize configuration
    Config = load_default_config(),
    store_config(Config),

    %% Set up configuration refresh timer
    {ok, _TRef} = timer:send_interval(60000, reload_config),

    {ok, Config}.

%% Get all configuration
get_config() ->
    gen_server:call(?MODULE, {get_config}).

%% Get specific configuration value
get_config(Key) ->
    gen_server:call(?MODULE, {get_config, Key}).

%% Update configuration
update_config(Key, Value) ->
    gen_server:cast(?MODULE, {update_config, Key, Value}).

%% Reload configuration from disk/storage
reload_config() ->
    gen_server:cast(?MODULE, reload_config).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

handle_call({get_config}, _From, State) ->
    {reply, {ok, State}, State};

handle_call({get_config, Key}, _From, State) ->
    Value = maps:get(Key, State, undefined),
    {reply, {ok, Value}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({update_config, Key, Value}, State) ->
    NewState = maps:put(Key, Value, State),
    store_config(NewState),
    {noreply, NewState};

handle_cast(reload_config, State) ->
    NewConfig = load_config_from_storage(),
    case NewConfig of
        {ok, Config} ->
            store_config(Config),
            {noreply, Config};
        {error, _} ->
            %% Keep current state if reload fails
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_default_config() ->
    %% Load default configuration
    #{
        portal_title => <<"erlmcp Developer Portal">>,
        portal_description => <<"Enterprise API Management Platform">>,
        portal_theme => <<"default">>,
        features => #{
            api_explorer => true,
            api_testing => true,
            documentation => true,
            analytics => true,
            community => true,
            support => true
        },
        auth => #{
            enabled => true,
            providers => [<<"developer">>],
            session_timeout => 86400
        },
        notifications => #{
            enabled => true,
            email => #{
                enabled => false,
                smtp => #{
                    server => <<"smtp.example.com">>,
                    port => 587,
                    username => undefined,
                    password => undefined
                }
            }
        },
        analytics => #{
            enabled => true,
            collection_interval => 60000,
            retention_period => 2592000 % 30 days
        },
        rate_limiting => #{
            enabled => true,
            default_limit => 100,
            burst_limit => 200
        },
        caching => #{
            enabled => true,
            ttl => 3600,
            max_size => 1000
        },
        database => #{
            type => mnesia,
            nodes => [node()],
            backup_interval => 86400
        },
        logging => #{
            level => info,
            file => <<"erlmcp_dev_portal.log">>,
            size => 10485760, % 10MB
            count => 5
        },
        monitoring => #{
            enabled => true,
            health_check_interval => 30000,
            metrics_interval => 60000
        }
    }.

load_config_from_storage() ->
    %% Load configuration from mnesia
    case mnesia:dirty_read(dev_portal_config, main) of
        [Config] -> {ok, Config};
        _ -> {error, not_found}
    end.

store_config(Config) ->
    %% Store configuration in mnesia
    Fun = fun() ->
        mnesia:write(#dev_portal_config{
            id = main,
            config = Config,
            timestamp = erlang:system_time(second)
        })
    end,
    mnesia:transaction(Fun).

%%====================================================================
%% Record Definitions
%%====================================================================

-record(dev_portal_config, {
    id :: atom(),
    config :: map(),
    timestamp :: integer()
}).