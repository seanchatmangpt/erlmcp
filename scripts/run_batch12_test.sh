#!/bin/bash
# Run Batch 12 Large Payload Tests

set -e

cd /Users/sac/erlmcp

echo "=== Running Batch 12 Large Payload Tests ==="
echo

# Start Erlang node with test code
erl -pa _build/default/lib/erlmcp_core/ebin \
    -pa _build/default/lib/erlmcp_transports/ebin \
    -pa _build/default/lib/gproc/ebin \
    -pa _build/default/lib/jsx/ebin \
    -pa _build/default/lib/ranch/ebin \
    -pa _build/default/lib/gun/ebin \
    -pa _build/test/lib/erlmcp_core/ebin \
    -pa /tmp \
    -eval "
        error_logger:tty(false),
        try
            eunit:test(erlmcp_roundtrip_batch12_large_payload_tests, [verbose])
        catch
            Type:Error ->
                io:format('Test failed: ~p:~p~n', [Type, Error]),
                halt(1)
        after
            halt(0)
        end
    " -noshell
