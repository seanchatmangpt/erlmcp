%% OTP Benchmark Runner
%%
%% This module provides the entry point for running OTP version benchmarks
%% and includes support for different OTP installations.

-module(bench_runner).
-export([run_all_benchmarks/0, run_benchmark_for_otp_version/1, setup_otp_environment/1]).

-include_lib("kernel/include/logger.hrl").

%% Main entry point - run all available benchmarks
run_all_benchmarks() ->
    io:format("OTP Version Performance Benchmark Suite~n", []),
    io:format("=========================================~n~n", []),

    %% Check available OTP installations
    AvailableVersions = get_available_otp_versions(),
    io:format("Available OTP versions: ~p~n", [AvailableVersions]),

    case lists:member(28, AvailableVersions) of
        true ->
            io:format("Using OTP 28.3.1 from ERLMCP_OTP_BIN~n", []),
            %% Use the configured OTP version
            otp_bench:main();
        false ->
            io:format("OTP 28.3.1 not found. Available: ~p~n", [AvailableVersions]),
            %% Try to run with current OTP version
            io:format("Running with current OTP version: ~p~n", [erlang:system_info(otp_release)]),
            otp_bench:main()
    end.

%% Run benchmark for specific OTP version
run_benchmark_for_otp_version(Version) ->
    io:format("Running benchmark for OTP version ~p~n", [Version]),

    case setup_otp_environment(Version) of
        success ->
            otp_bench:main();
        {error, Reason} ->
            ?LOG_ERROR("Failed to setup OTP ~p: ~p", [Version, Reason]),
            error
    end.

%% Setup OTP environment for specific version
setup_otp_version(Version) ->
    %% Check if we have OTP installations in ~/.erlmcp/
    HomeDir = os:getenv("HOME"),
    OtpDir = filename:join(HomeDir, ".erlmcp"),

    %% Pattern for OTP installations
    Pattern = filename:join(otpDir, "otp-" ++ integer_to_list(Version) ++ ".*"),

    case filelib:wildcard(Pattern) of
        [Path | _] ->
            %% Found OTP installation
            OtpBin = filename:join(Path, "bin"),
            case filelib:is_dir(OtpBin) of
                true ->
                    %% Set environment variable
                    os:putenv("ERLMCP_OTP_BIN", OtpBin),
                    io:format("Using OTP ~p from: ~s~n", [Version, OtpBin]),
                    success;
                false ->
                    {error, "OTP bin directory not found"}
            end;
        [] ->
            {error, "OTP installation not found"}
    end.

%% Get available OTP versions
get_available_otp_versions() ->
    HomeDir = os:getenv("HOME"),
    OtpDir = filename:join(HomeDir, ".erlmcp"),

    case filelib:is_dir(OtpDir) of
        true ->
            %% Find OTP installations
            Patterns = filelib:wildcard(filename:join(OtpDir, "otp-*")),
            %% Extract version numbers
            Versions = lists:foldl(fun(Path, Acc) ->
                case filename:basename(Path) of
                    "otp-" ++ Rest ->
                        case catch list_to_integer(string:tokens(Rest, ".\\-_")) of
                            [V] when V >= 26 ->
                                [V | Acc];
                            _ ->
                                Acc
                        end;
                    _ ->
                        Acc
                end
            end, [], Patterns),
            lists:usort(Versions);
        false ->
            %% No OTP installations found, use current version
            [erlang:system_info(otp_release)]
    end.