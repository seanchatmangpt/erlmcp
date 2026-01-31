#!/bin/bash
# Test script for consensus POC

rebar3 shell --eval "erlmcp_consensus_poc:run_demo()."
