%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_observability_app - Observability Application Callback
%%%
%%% OTP application callback for the observability subsystem.
%%% Starts the observability supervision tree.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_observability_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Application Callbacks
%%====================================================================

%% @doc Start the observability application
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    ?LOG_INFO("Starting erlmcp_observability application"),

    %% Start the supervisor FIRST
    case erlmcp_observability_sup:start_link() of
        {ok, Pid} ->
            ?LOG_INFO("erlmcp_observability started successfully"),

            %% Initialize OpenTelemetry ASYNC after supervisor starts
            %% This avoids blocking application startup if OTEL fails
            case application:get_env(erlmcp_observability, otel_enabled, true) of
                true ->
                    spawn(fun() ->
                             timer:sleep(100),  % Small delay to let supervisor initialize
                             init_otel()
                          end);
                false ->
                    ok
            end,

            {ok, Pid};
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to start erlmcp_observability: ~p", [Reason]),
            Error
    end.

%% @doc Stop the observability application
-spec stop(term()) -> ok.
stop(_State) ->
    ?LOG_INFO("Stopping erlmcp_observability application"),

    %% Shutdown OpenTelemetry
    case application:get_env(erlmcp_observability, otel_enabled, true) of
        true ->
            erlmcp_otel:shutdown();
        false ->
            ok
    end,

    ok.

%% @doc Prepare for graceful shutdown
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    ?LOG_INFO("Preparing erlmcp_observability for graceful shutdown"),
    %% Flush any remaining telemetry data
    try
        case application:get_env(erlmcp_observability, otel_enabled, true) of
            true ->
                erlmcp_otel:flush();
            false ->
                ok
        end
    catch
        _:_ -> ok
    end,
    State.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Initialize OpenTelemetry with configuration from environment
-spec init_otel() -> ok.
init_otel() ->
    ServiceName =
        application:get_env(erlmcp_observability, otel_service_name, <<"erlmcp-observability">>),
    Exporters = application:get_env(erlmcp_observability, otel_exporters, [console]),
    Sampling = application:get_env(erlmcp_observability, otel_sampling, always_on),

    Config =
        #{service_name => ServiceName,
          service_version => <<"0.7.0">>,
          exporters => Exporters,
          sampling => Sampling},

    case erlmcp_otel:init(Config) of
        ok ->
            ?LOG_INFO("OpenTelemetry initialized successfully"),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to initialize OpenTelemetry: ~p", [Reason]),
            ok  % Don't fail application start if OTEL fails
    end.
