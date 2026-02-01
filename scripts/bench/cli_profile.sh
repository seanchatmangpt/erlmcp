#!/usr/bin/env bash
# CLI Profiling Script - Uses fprof for detailed analysis
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=========================================="
echo "ERLMCP CLI PROFILING (fprof)"
echo "=========================================="
echo ""

cd "$ROOT_DIR"

# Ensure compiled
echo "Ensuring project is compiled..."
TERM=dumb rebar3 compile > /dev/null 2>&1

echo ""
echo "Running profiler..."
echo ""

# Run with profiling enabled
rebar3 shell --config config/sys.config --eval "
    %% Profile startup
    io:format(\"Profiling CLI startup...~n\"),
    erlmcp_cli_startup_bench:profile_startup(),
    
    io:format(\"~nProfile written to: /tmp/erlmcp_cli_startup_profile.txt~n\"),
    io:format(\"~nTop functions by time:~n\"),
    io:format(\"-----------------------------------~n\"),
    
    %% Parse and display top functions
    {ok, Content} = file:read_file(\"/tmp/erlmcp_cli_startup_profile.txt\"),
    Lines = binary:split(Content, <<\"\\n\">>, [global]),
    
    %% Find totals section
    io:format(\"See /tmp/erlmcp_cli_startup_profile.txt for full details~n\"),
    
    init:stop().
" --sname cli_profiler

echo ""
echo "=========================================="
echo "Profiling complete!"
echo "Report: /tmp/erlmcp_cli_startup_profile.txt"
echo "=========================================="
echo ""
echo "To view the full report:"
echo "  less /tmp/erlmcp_cli_startup_profile.txt"
echo ""
echo "To find bottlenecks, look for:"
echo "  - High ACC (accumulated time)"
echo "  - High OWN (function's own time)"
echo "  - Frequent calls (CNT)"
