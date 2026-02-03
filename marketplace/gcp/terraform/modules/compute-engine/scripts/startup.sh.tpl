#!/bin/bash
# erlmcp startup script
echo "Starting erlmcp on compute engine..."
exec erlmcp --config=/etc/erlmcp/config.yaml
