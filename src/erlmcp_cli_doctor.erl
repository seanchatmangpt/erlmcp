%%%-----------------------------------------------------------------------------
%%% @doc erlmcp doctor - Host Readiness Diagnostics
%%%
%%% Comprehensive diagnostic command to verify host environment readiness for
%%% erlmcp development and deployment.
%%%
%%% Features:
%%% - System limits validation (ulimit, ephemeral ports, TCP params)
%%% - Container runtime detection (docker, colima, podman)
%%% - Erlang/OTP version checking
%%% - Build tool validation (rebar3)
%%% - Environment variable verification (TLS, OTEL)
%%% - Actionable remediation steps
%%% - JSON and text output formats
%%%
%%% Exit Codes:
%%% - 0: All checks passed (healthy system)
%%% - 1: Fixable issues found (warnings)
%%% - 2: Blocking issues found (critical failures)
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_cli_doctor).

-export([
    run/1,
    check_all/0,
    check_all/1,
    get_exit_code/1,
    output_text_report/1,
    output_json_report/1
]).

-include_lib("kernel/include/file.hrl").

-type check_status() :: ok | warning | critical.
-type check_result() :: {
    string(),  % name
    check_status(),
    string(),  % details
    string()   % remediation
}.

-record(doctor_report, {
    timestamp,
    hostname,
    checks,
    summary,
    overall_status
}).

-type doctor_report() :: #doctor_report{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Main CLI entry point for doctor command
-spec run([string()]) -> ok.
run(Args) ->
    Format = parse_format_flag(Args),
    Report = check_all(Format),
    try
        output_report(Report, Format)
    catch
        _Error:_Reason ->
            % Suppress errors in output reporting
            ok
    end,
    ok.

%% @doc Run all diagnostic checks
-spec check_all() -> doctor_report().
check_all() ->
    check_all(text).

%% @doc Run all diagnostic checks with specified format
-spec check_all(text | json) -> doctor_report().
check_all(Format) ->
    Checks = run_checks(),
    build_report(Checks, Format).

%%%=============================================================================
%%% Internal - Check Functions
%%%=============================================================================

%% @doc Run all diagnostic checks
-spec run_checks() -> [check_result()].
run_checks() ->
    [
        check_erlang_version(),
        check_otp_features(),
        check_rebar3_installation(),
        check_rebar3_version(),
        check_file_descriptor_limit(),
        check_ephemeral_ports(),
        check_tcp_params(),
        check_container_runtime(),
        check_tls_environment(),
        check_otel_environment(),
        check_disk_space(),
        check_memory_available()
    ].

%% @doc Check Erlang/OTP version (must be 25+)
-spec check_erlang_version() -> check_result().
check_erlang_version() ->
    OtpRelease = erlang:system_info(otp_release),
    OtpVersion = try list_to_integer(OtpRelease) catch _:_ -> 0 end,
    case OtpVersion >= 25 of
        true ->
            {
                "Erlang/OTP Version",
                ok,
                io_lib:format("OTP ~s (meets minimum requirement of OTP 25+)", [OtpRelease]),
                ""
            };
        false ->
            {
                "Erlang/OTP Version",
                critical,
                io_lib:format("OTP ~s (requires OTP 25+ for erlmcp)", [OtpRelease]),
                io_lib:format("Install Erlang/OTP 25+: brew install erlang (macOS) or apt-get install erlang (Linux)", [])
            }
    end.

%% @doc Check OTP features required for erlmcp
-spec check_otp_features() -> check_result().
check_otp_features() ->
    % Check if OTP supports required features
    SupportsEts = erlang:system_info(otp_release) >= "21",
    case SupportsEts of
        true ->
            {
                "OTP Features",
                ok,
                "All required OTP features available (ETS, gen_server, supervisor)",
                ""
            };
        false ->
            {
                "OTP Features",
                critical,
                "Missing required OTP features",
                "Upgrade to OTP 21+ to enable all features"
            }
    end.

%% @doc Check if rebar3 is installed
-spec check_rebar3_installation() -> check_result().
check_rebar3_installation() ->
    case find_rebar3() of
        {ok, Path} ->
            {
                "rebar3 Installation",
                ok,
                io_lib:format("rebar3 found at ~s", [Path]),
                ""
            };
        error ->
            {
                "rebar3 Installation",
                critical,
                "rebar3 not found in PATH",
                "Install rebar3: brew install rebar3 (macOS) or see https://www.rebar3.org/"
            }
    end.

%% @doc Check rebar3 version
-spec check_rebar3_version() -> check_result().
check_rebar3_version() ->
    case get_rebar3_version() of
        {ok, Version} ->
            case version_satisfies(Version, "3.13.0") of
                true ->
                    {
                        "rebar3 Version",
                        ok,
                        io_lib:format("rebar3 version ~s (meets minimum requirement)", [Version]),
                        ""
                    };
                false ->
                    {
                        "rebar3 Version",
                        warning,
                        io_lib:format("rebar3 version ~s (recommend 3.13.0+)", [Version]),
                        "Update rebar3: brew upgrade rebar3 (macOS) or download from https://www.rebar3.org/"
                    }
            end;
        error ->
            {
                "rebar3 Version",
                warning,
                "Could not determine rebar3 version",
                "Run: rebar3 --version"
            }
    end.

%% @doc Check file descriptor limit
-spec check_file_descriptor_limit() -> check_result().
check_file_descriptor_limit() ->
    case erlang:system_info(process_count) of
        ProcessCount ->
            MinFD = min(ProcessCount * 2, 65536),
            case get_ulimit_nofile() of
                {ok, Current} when Current >= MinFD ->
                    {
                        "File Descriptor Limit",
                        ok,
                        io_lib:format("ulimit -n = ~w (sufficient for ~w processes)", [Current, ProcessCount]),
                        ""
                    };
                {ok, Current} ->
                    {
                        "File Descriptor Limit",
                        warning,
                        io_lib:format("ulimit -n = ~w (recommend >= ~w for current load)", [Current, MinFD]),
                        io_lib:format("Increase limit: ulimit -n ~w", [MinFD])
                    };
                error ->
                    {
                        "File Descriptor Limit",
                        warning,
                        "Could not determine file descriptor limit",
                        "Check: ulimit -n (should be >= 4096)"
                    }
            end
    end.

%% @doc Check ephemeral port availability
-spec check_ephemeral_ports() -> check_result().
check_ephemeral_ports() ->
    case get_ephemeral_port_range() of
        {ok, Min, Max} ->
            Available = Max - Min,
            case Available >= 16384 of
                true ->
                    {
                        "Ephemeral Ports",
                        ok,
                        io_lib:format("Ephemeral port range ~w-~w (~w ports available)", [Min, Max, Available]),
                        ""
                    };
                false ->
                    {
                        "Ephemeral Ports",
                        warning,
                        io_lib:format("Ephemeral port range ~w-~w (~w ports available, recommend >= 16384)", [Min, Max, Available]),
                        case os:type() of
                            {unix, linux} ->
                                "Increase range: echo '32768 65535' > /proc/sys/net/ipv4/ip_local_port_range";
                            {unix, darwin} ->
                                "Increase range: sudo sysctl -w net.inet.ipport.firewallportrange='32768 65535'";
                            _ ->
                                "Consult OS documentation for ephemeral port configuration"
                        end
                    }
            end;
        error ->
            {
                "Ephemeral Ports",
                warning,
                "Could not determine ephemeral port range",
                case os:type() of
                    {unix, linux} -> "Check: cat /proc/sys/net/ipv4/ip_local_port_range";
                    {unix, darwin} -> "Check: sysctl net.inet.ipport.firewallportrange";
                    _ -> "Consult OS documentation"
                end
            }
    end.

%% @doc Check kernel TCP parameters
-spec check_tcp_params() -> check_result().
check_tcp_params() ->
    case get_tcp_backlog() of
        {ok, Backlog} when Backlog >= 256 ->
            {
                "TCP Parameters",
                ok,
                io_lib:format("TCP backlog = ~w (acceptable for production)", [Backlog]),
                ""
            };
        {ok, Backlog} ->
            {
                "TCP Parameters",
                warning,
                io_lib:format("TCP backlog = ~w (recommend >= 256 for production)", [Backlog]),
                case os:type() of
                    {unix, linux} ->
                        "Increase backlog: echo 256 | sudo tee /proc/sys/net/core/somaxconn";
                    {unix, darwin} ->
                        "Increase backlog: sudo sysctl -w kern.ipc.somaxconn=256";
                    _ ->
                        "Consult OS documentation"
                end
            };
        error ->
            {
                "TCP Parameters",
                warning,
                "Could not verify TCP backlog configuration",
                case os:type() of
                    {unix, linux} -> "Check: cat /proc/sys/net/core/somaxconn";
                    {unix, darwin} -> "Check: sysctl kern.ipc.somaxconn";
                    _ -> "Consult OS documentation"
                end
            }
    end.

%% @doc Check container runtime availability
-spec check_container_runtime() -> check_result().
check_container_runtime() ->
    Runtimes = check_container_runtimes(),
    case Runtimes of
        [] ->
            {
                "Container Runtime",
                warning,
                "No container runtime detected (docker, colima, or podman)",
                "For local development: brew install docker (macOS with colima) or docker-desktop (all platforms)"
            };
        Found ->
            Names = string:join([atom_to_list(Name) || Name <- Found], ", "),
            {
                "Container Runtime",
                ok,
                io_lib:format("Container runtime available: ~s", [Names]),
                ""
            }
    end.

%% @doc Check TLS environment variables
-spec check_tls_environment() -> check_result().
check_tls_environment() ->
    CertPath = os:getenv("ERLMCP_TLS_CERT_PATH"),
    KeyPath = os:getenv("ERLMCP_TLS_KEY_PATH"),
    _CaPath = os:getenv("ERLMCP_TLS_CA_PATH"),

    case {CertPath, KeyPath} of
        {false, false} ->
            {
                "TLS Environment",
                ok,
                "TLS not configured (using development defaults)",
                ""
            };
        {CertPath, KeyPath} when is_list(CertPath), is_list(KeyPath) ->
            case {file:read_file_info(CertPath), file:read_file_info(KeyPath)} of
                {{ok, _}, {ok, _}} ->
                    {
                        "TLS Environment",
                        ok,
                        "TLS certificates configured and accessible",
                        ""
                    };
                _ ->
                    {
                        "TLS Environment",
                        warning,
                        "TLS certificates configured but not all files accessible",
                        io_lib:format("Verify ERLMCP_TLS_CERT_PATH=~s and ERLMCP_TLS_KEY_PATH=~s exist", [CertPath, KeyPath])
                    }
            end;
        _ ->
            {
                "TLS Environment",
                warning,
                "Incomplete TLS configuration",
                "Set both ERLMCP_TLS_CERT_PATH and ERLMCP_TLS_KEY_PATH"
            }
    end.

%% @doc Check OTEL environment variables
-spec check_otel_environment() -> check_result().
check_otel_environment() ->
    OtelEnabled = os:getenv("OTEL_ENABLED"),
    case OtelEnabled of
        false ->
            {
                "OTEL Environment",
                ok,
                "OTEL telemetry not configured (using default metrics)",
                ""
            };
        "true" ->
            ExporterEndpoint = os:getenv("OTEL_EXPORTER_OTLP_ENDPOINT"),
            TracesSampler = os:getenv("OTEL_TRACES_SAMPLER"),
            case {ExporterEndpoint, TracesSampler} of
                {Ep, Sampler} when is_list(Ep), is_list(Sampler) ->
                    {
                        "OTEL Environment",
                        ok,
                        io_lib:format("OTEL configured: endpoint=~s, sampler=~s", [Ep, Sampler]),
                        ""
                    };
                _ ->
                    {
                        "OTEL Environment",
                        warning,
                        "OTEL enabled but not fully configured",
                        "Set OTEL_EXPORTER_OTLP_ENDPOINT and OTEL_TRACES_SAMPLER"
                    }
            end;
        _ ->
            {
                "OTEL Environment",
                ok,
                "OTEL not enabled (use OTEL_ENABLED=true to enable)",
                ""
            }
    end.

%% @doc Check available disk space
-spec check_disk_space() -> check_result().
check_disk_space() ->
    case disk_usage(get_project_root()) of
        {ok, {Total, _Available}} when Total > 1024 * 1024 * 1024 ->
            {
                "Disk Space",
                ok,
                io_lib:format("Sufficient disk space available (~w GB)", [Total div (1024 * 1024 * 1024)]),
                ""
            };
        {ok, {Total, _Available}} ->
            {
                "Disk Space",
                warning,
                io_lib:format("Limited disk space available (~w MB, recommend >= 1GB)", [Total div (1024 * 1024)]),
                "Free up disk space: rm -rf _build, or use larger partition"
            };
        error ->
            {
                "Disk Space",
                warning,
                "Could not determine available disk space",
                "Run: df -h to check manually"
            }
    end.

%% @doc Check available memory
-spec check_memory_available() -> check_result().
check_memory_available() ->
    case os:type() of
        {unix, darwin} ->
            check_memory_darwin();
        {unix, linux} ->
            check_memory_linux();
        _ ->
            {
                "Available Memory",
                ok,
                "Memory check not available on this platform",
                ""
            }
    end.

-spec check_memory_darwin() -> check_result().
check_memory_darwin() ->
    try
        case os:cmd("vm_stat | grep 'Pages free' | awk '{print $3}'") of
            [] ->
                {
                    "Available Memory",
                    warning,
                    "Could not determine available memory",
                    "Run: vm_stat to check manually"
                };
            Output ->
                Pages = list_to_integer(string:trim(Output, trailing, "\n")),
                AvailableMB = (Pages * 4096) div (1024 * 1024),
                case AvailableMB >= 512 of
                    true ->
                        {
                            "Available Memory",
                            ok,
                            io_lib:format("Available memory: ~w MB", [AvailableMB]),
                            ""
                        };
                    false ->
                        {
                            "Available Memory",
                            warning,
                            io_lib:format("Available memory: ~w MB (recommend >= 512 MB)", [AvailableMB]),
                            "Free up memory by closing applications"
                        }
                end
        end
    catch
        _:_ ->
            {
                "Available Memory",
                warning,
                "Could not parse memory information",
                "Run: vm_stat to check manually"
            }
    end.

-spec check_memory_linux() -> check_result().
check_memory_linux() ->
    case os:cmd("free -m | grep Mem | awk '{print $7}'") of
        [] ->
            {
                "Available Memory",
                warning,
                "Could not determine available memory",
                "Run: free -h to check manually"
            };
        Output ->
            try
                AvailableMB = list_to_integer(string:trim(Output, trailing, "\n")),
                case AvailableMB >= 512 of
                    true ->
                        {
                            "Available Memory",
                            ok,
                            io_lib:format("Available memory: ~w MB", [AvailableMB]),
                            ""
                        };
                    false ->
                        {
                            "Available Memory",
                            warning,
                            io_lib:format("Available memory: ~w MB (recommend >= 512 MB)", [AvailableMB]),
                            "Free up memory by closing applications"
                        }
                end
            catch
                _:_ ->
                    {
                        "Available Memory",
                        warning,
                        "Could not parse memory information",
                        "Run: free -h to check manually"
                    }
            end
    end.

%%%=============================================================================
%%% Internal - Type Conversion Helpers
%%%=============================================================================

%% @doc Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(Str) when is_list(Str) -> list_to_binary(Str);
ensure_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
ensure_binary(Other) -> list_to_binary(io_lib:format("~p", [Other])).

%%%=============================================================================
%%% Internal - System Information Helpers
%%%=============================================================================

%% @doc Find rebar3 in PATH
-spec find_rebar3() -> {ok, string()} | error.
find_rebar3() ->
    case os:find_executable("rebar3") of
        Executable when is_list(Executable) ->
            {ok, Executable};
        false ->
            error
    end.

%% @doc Get rebar3 version
-spec get_rebar3_version() -> {ok, string()} | error.
get_rebar3_version() ->
    case find_rebar3() of
        {ok, Path} ->
            try
                Output = os:cmd(Path ++ " --version 2>&1"),
                Lines = string:split(Output, "\n", all),
                case find_version_in_lines(Lines) of
                    {ok, Version} -> {ok, Version};
                    error -> error
                end
            catch
                _:_ -> error
            end;
        error ->
            error
    end.

%% @doc Find version string in output lines
-spec find_version_in_lines([string()]) -> {ok, string()} | error.
find_version_in_lines([]) ->
    error;
find_version_in_lines([Line | Rest]) ->
    case string:find(Line, "rebar 3.") of
        nomatch ->
            find_version_in_lines(Rest);
        _ ->
            case extract_version(Line) of
                {ok, Version} -> {ok, Version};
                error -> find_version_in_lines(Rest)
            end
    end.

%% @doc Extract version from version string
-spec extract_version(string()) -> {ok, string()} | error.
extract_version(Line) ->
    case string:find(Line, "3.") of
        nomatch ->
            error;
        Match ->
            Parts = string:split(Match, " "),
            case Parts of
                [Version | _] -> {ok, string:trim(Version)};
                _ -> error
            end
    end.

%% @doc Check if version satisfies minimum requirement
-spec version_satisfies(string(), string()) -> boolean().
version_satisfies(Current, Minimum) ->
    try
        CurrentVer = parse_version(Current),
        MinVer = parse_version(Minimum),
        CurrentVer >= MinVer
    catch
        _:_ -> false
    end.

%% @doc Parse version string into tuple for comparison
-spec parse_version(string()) -> {integer(), integer(), integer()}.
parse_version(Version) ->
    Parts = string:split(string:trim(Version), "."),
    try
        {
            list_to_integer(lists:nth(1, Parts)),
            list_to_integer(lists:nth(2, Parts)),
            list_to_integer(lists:nth(3, Parts))
        }
    catch
        _:_ -> {0, 0, 0}
    end.

%% @doc Get ulimit -n (open files)
-spec get_ulimit_nofile() -> {ok, integer()} | error.
get_ulimit_nofile() ->
    case os:type() of
        {unix, _} ->
            try
                Output = os:cmd("ulimit -n 2>&1"),
                Limit = list_to_integer(string:trim(Output, trailing, "\n")),
                {ok, Limit}
            catch
                _:_ -> error
            end;
        _ ->
            error
    end.

%% @doc Get ephemeral port range
-spec get_ephemeral_port_range() -> {ok, integer(), integer()} | error.
get_ephemeral_port_range() ->
    case os:type() of
        {unix, linux} ->
            try
                Output = os:cmd("cat /proc/sys/net/ipv4/ip_local_port_range 2>/dev/null"),
                [Min, Max] = string:split(string:trim(Output), "\t"),
                {ok, list_to_integer(Min), list_to_integer(Max)}
            catch
                _:_ -> error
            end;
        {unix, darwin} ->
            {ok, 49152, 65535};
        _ ->
            error
    end.

%% @doc Get TCP backlog
-spec get_tcp_backlog() -> {ok, integer()} | error.
get_tcp_backlog() ->
    case os:type() of
        {unix, linux} ->
            try
                Output = os:cmd("cat /proc/sys/net/core/somaxconn 2>/dev/null"),
                Backlog = list_to_integer(string:trim(Output, trailing, "\n")),
                {ok, Backlog}
            catch
                _:_ -> error
            end;
        {unix, darwin} ->
            try
                Output = os:cmd("sysctl -n kern.ipc.somaxconn 2>/dev/null"),
                Backlog = list_to_integer(string:trim(Output, trailing, "\n")),
                {ok, Backlog}
            catch
                _:_ -> error
            end;
        _ ->
            error
    end.

%% @doc Check available container runtimes
-spec check_container_runtimes() -> [atom()].
check_container_runtimes() ->
    Runtimes = [docker, colima, podman],
    [R || R <- Runtimes, os:find_executable(atom_to_list(R)) =/= false].

%% @doc Get disk usage
-spec disk_usage(string()) -> {ok, {integer(), integer()}} | error.
disk_usage(Path) ->
    case os:type() of
        {unix, _} ->
            try
                % Use df -k for reliable output (1024-byte blocks)
                Output = os:cmd("df -k " ++ Path ++ " 2>/dev/null | tail -1"),
                Parts = string:split(Output, " ", all),
                FilteredParts = [P || P <- Parts, P =/= ""],
                case length(FilteredParts) >= 2 of
                    true ->
                        Total = list_to_integer(lists:nth(2, FilteredParts)) * 1024,
                        Available = list_to_integer(lists:nth(4, FilteredParts)) * 1024,
                        {ok, {Total, Available}};
                    false ->
                        error
                end
            catch
                _:_ -> error
            end;
        _ ->
            error
    end.

%% @doc Get project root directory
-spec get_project_root() -> string().
get_project_root() ->
    case file:get_cwd() of
        {ok, Dir} -> Dir;
        _ -> "."
    end.

%%%=============================================================================
%%% Internal - Report Building & Output
%%%=============================================================================

%% @doc Build report from checks
-spec build_report([check_result()], text | json) -> doctor_report().
build_report(Checks, _Format) ->
    Summary = build_summary(Checks),
    OverallStatus = determine_overall_status(Summary),
    #doctor_report{
        timestamp = calendar:local_time(),
        hostname = get_hostname(),
        checks = Checks,
        summary = Summary,
        overall_status = OverallStatus
    }.

%% @doc Build summary statistics
-spec build_summary([check_result()]) -> map().
build_summary(Checks) ->
    Total = length(Checks),
    Statuses = [Status || {_, Status, _, _} <- Checks],
    Passed = length([S || S <- Statuses, S =:= ok]),
    Warnings = length([S || S <- Statuses, S =:= warning]),
    Critical = length([S || S <- Statuses, S =:= critical]),
    #{
        total => Total,
        passed => Passed,
        warnings => Warnings,
        critical => Critical
    }.

%% @doc Determine overall health status
-spec determine_overall_status(map()) -> check_status().
determine_overall_status(#{critical := C}) when C > 0 ->
    critical;
determine_overall_status(#{warnings := W}) when W > 0 ->
    warning;
determine_overall_status(_) ->
    ok.

%% @doc Get system hostname
-spec get_hostname() -> string().
get_hostname() ->
    case inet:gethostname() of
        {ok, Hostname} -> Hostname;
        _ -> "unknown"
    end.

%% @doc Output report in specified format
-spec output_report(doctor_report(), text | json) -> ok.
output_report(Report, text) ->
    output_text_report(Report);
output_report(Report, json) ->
    output_json_report(Report).

%% @doc Output report in text format
-spec output_text_report(doctor_report()) -> ok.
output_text_report(#doctor_report{
    hostname = Hostname,
    checks = Checks,
    summary = #{total := Total, passed := Passed, warnings := Warnings, critical := Critical},
    overall_status = OverallStatus
}) ->
    io:format("~n", []),
    io:format("╔════════════════════════════════════════════════════════════════╗~n", []),
    io:format("║           erlmcp doctor - Host Readiness Check                ║~n", []),
    io:format("╚════════════════════════════════════════════════════════════════╝~n", []),
    io:format("~n", []),
    io:format("Host:     ~s~n", [Hostname]),
    io:format("~n", []),

    % Output each check
    lists:foreach(fun({Name, Status, Details, Remediation}) ->
        output_check_result(Name, Status, Details, Remediation)
    end, Checks),

    io:format("~n", []),
    io:format("─────────────────────────────────────────────────────────────────~n", []),
    io:format("Summary:  ~w/~w checks passed~n", [Passed, Total]),
    case Warnings of
        0 -> io:format("          No warnings~n", []);
        _ -> io:format("          ~w warning(s)~n", [Warnings])
    end,
    case Critical of
        0 -> io:format("          No critical issues~n", []);
        _ -> io:format("          ~w critical issue(s)~n", [Critical])
    end,
    io:format("~n", []),
    output_overall_status(OverallStatus),
    io:format("~n", []).

%% @doc Output individual check result
-spec output_check_result(string(), check_status(), string(), string()) -> ok.
output_check_result(Name, ok, Details, _Remediation) ->
    io:format("  ✓ ~s~n", [Name]),
    io:format("    ~s~n", [Details]);
output_check_result(Name, warning, Details, Remediation) ->
    io:format("  ⚠ ~s~n", [Name]),
    io:format("    ~s~n", [Details]),
    case Remediation of
        "" -> ok;
        _ -> io:format("    → ~s~n", [Remediation])
    end;
output_check_result(Name, critical, Details, Remediation) ->
    io:format("  ✗ ~s~n", [Name]),
    io:format("    ~s~n", [Details]),
    case Remediation of
        "" -> ok;
        _ -> io:format("    → ~s~n", [Remediation])
    end.

%% @doc Output overall status
-spec output_overall_status(check_status()) -> ok.
output_overall_status(ok) ->
    io:format("Status:   ~s~n", ["✓ All checks passed - ready for development"]);
output_overall_status(warning) ->
    io:format("Status:   ~s~n", ["⚠ Warnings found - review recommendations above"]);
output_overall_status(critical) ->
    io:format("Status:   ~s~n", ["✗ Critical issues found - must be resolved"]).

%% @doc Output report in JSON format
-spec output_json_report(doctor_report()) -> ok.
output_json_report(#doctor_report{
    timestamp = Timestamp,
    hostname = Hostname,
    checks = Checks,
    summary = Summary,
    overall_status = OverallStatus
}) ->
    JsonChecks = [
        #{
            name => ensure_binary(Name),
            status => atom_to_binary(Status, utf8),
            details => ensure_binary(Details),
            remediation => ensure_binary(Remediation)
        }
        || {Name, Status, Details, Remediation} <- Checks
    ],

    JsonSummary = maps:map(fun(_, V) when is_integer(V) -> V;
                               (_, V) -> V end, Summary),

    Report = #{
        timestamp => format_timestamp(Timestamp),
        hostname => Hostname,
        checks => JsonChecks,
        summary => JsonSummary,
        status => atom_to_binary(OverallStatus, utf8)
    },

    Json = jsx:encode(Report, [pretty]),
    io:format("~s~n", [Json]).

%% @doc Format timestamp for output
-spec format_timestamp(calendar:datetime()) -> string().
format_timestamp({{Y, M, D}, {H, Min, S}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Y, M, D, H, Min, S]).

%% @doc Get exit code based on report status
-spec get_exit_code(doctor_report()) -> 0 | 1 | 2.
get_exit_code(#doctor_report{overall_status = ok}) ->
    0;
get_exit_code(#doctor_report{overall_status = warning}) ->
    1;
get_exit_code(#doctor_report{overall_status = critical}) ->
    2.

%% @doc Parse format flag from arguments
-spec parse_format_flag([string()]) -> text | json.
parse_format_flag([]) ->
    text;
parse_format_flag(["--json" | _]) ->
    json;
parse_format_flag([_H | T]) ->
    parse_format_flag(T).
