%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Formatter Behavior - Custom output formatters
%%%
%%% Formatters transform CLI output to different formats:
%%% - CSV, XML, AVRO, Protocol Buffers
%%% - Custom JSON transformations
%%% - Domain-specific output formats
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(erlmcp_plugin_csv_formatter).
%%% -behaviour(erlmcp_plugin_formatter).
%%%
%%% -export([init/1, format/2, supports_format/0, metadata/0]).
%%%
%%% metadata() ->
%%%     #{name => <<"csv_formatter">>,
%%%       version => <<"1.0.0">>,
%%%       type => formatter,
%%%       description => <<"CSV output formatter">>}.
%%%
%%% init(Opts) ->
%%%     {ok, #{}}.
%%%
%%% format(Data, State) ->
%%%     CSV = convert_to_csv(Data),
%%%     {ok, CSV, State}.
%%%
%%% supports_format() ->
%%%     csv.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_formatter).

%% Behavior definition
-callback format(Data :: term(), State :: term()) ->
    {ok, Formatted :: binary(), NewState :: term()} | {error, Reason :: term()}.

-callback supports_format() -> atom().

%% Re-export plugin behavior
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    erlmcp_plugin:behaviour_info(callbacks) ++
    [
        {format, 2},
        {supports_format, 0}
    ];
behaviour_info(_) ->
    undefined.
