-module(erlmcp_otp_manager_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,  % Restart all on any failure (state consistency)
        intensity => 3,           % Max 3 restarts
        period => 60              % Within 60 seconds
    },

    %% Child specifications - 5 workers as requested
    ChildSpecs = [
        %% 1. Registry - Track installed OTP versions
        #{
            id => erlmcp_otp_registry,
            start => {erlmcp_otp_registry, start_link, []},
            restart => permanent,
            shutdown => 30000,
            type => worker,
            modules => [erlmcp_otp_registry]
        },

        %% 2. Cache Server - Cache OTP artifacts
        #{
            id => erlmcp_otp_cache_server,
            start => {erlmcp_otp_cache_server, start_link, []},
            restart => permanent,
            shutdown => 30000,
            type => worker,
            modules => [erlmcp_otp_cache_server]
        },

        %% 3. Fetcher - Download OTP releases
        #{
            id => erlmcp_otp_fetcher,
            start => {erlmcp_otp_fetcher, start_link, []},
            restart => permanent,
            shutdown => 30000,
            type => worker,
            modules => [erlmcp_otp_fetcher]
        },

        %% 4. Builder - Compile OTP from source
        #{
            id => erlmcp_otp_builder,
            start => {erlmcp_otp_builder, start_link, []},
            restart => permanent,
            shutdown => 30000,
            type => worker,
            modules => [erlmcp_otp_builder]
        },

        %% 5. Verifier - Validate checksums
        #{
            id => erlmcp_otp_verifier,
            start => {erlmcp_otp_verifier, start_link, []},
            restart => permanent,
            shutdown => 30000,
            type => worker,
            modules => [erlmcp_otp_verifier]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
