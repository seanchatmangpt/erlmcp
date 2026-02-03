#!/bin/bash
# erlmcp startup script
echo "Starting erlmcp on GCE VM..."
exec erlmcp --config=/etc/erlmcp/config.yaml
