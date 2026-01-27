-module(erlmcp_transport).

%% Transport behavior definition

-type transport_state() :: term().
-type transport_opts() :: term().

%% Standard message format types
-type transport_message() ::
    {transport_data, Data :: binary()} |
    {transport_connected, Info :: map()} |
    {transport_disconnected, Reason :: term()} |
    {transport_error, Type :: atom(), Reason :: term()}.

%% Callback definitions
-callback init(Opts :: transport_opts()) ->
    {ok, State :: transport_state()} |
    {error, Reason :: term()}.

-callback send(State :: transport_state(), Data :: iodata()) ->
    ok |
    {error, Reason :: term()}.

-callback close(State :: transport_state()) ->
    ok.

%% Optional callbacks
-callback get_info(State :: term()) ->
    #{type => atom(), status => atom(), peer => term()}.

-callback handle_transport_call(Request :: term(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {error, Reason :: term()}.

-optional_callbacks([close/1, get_info/1, handle_transport_call/2]).

%% Type exports
-export_type([transport_state/0, transport_opts/0, transport_message/0]).

%% Configuration validation helper functions (exported for transport implementations)
-export([validate_config/2, merge_defaults/2]).

%% @doc Validate transport configuration against required keys.
%% Implementations can use this to standardize config validation.
-spec validate_config(Config :: map(), RequiredKeys :: [atom()]) ->
    ok | {error, {missing_keys, [atom()]}}.
validate_config(Config, RequiredKeys) when is_map(Config), is_list(RequiredKeys) ->
    MissingKeys = [Key || Key <- RequiredKeys, not maps:is_key(Key, Config)],
    case MissingKeys of
        [] -> ok;
        _ -> {error, {missing_keys, MissingKeys}}
    end.

%% @doc Merge user configuration with defaults, user config takes precedence.
-spec merge_defaults(Config :: map(), Defaults :: map()) -> map().
merge_defaults(Config, Defaults) when is_map(Config), is_map(Defaults) ->
    maps:merge(Defaults, Config).