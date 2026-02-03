#!/usr/bin/env bash
# Marketplace Latency Measurement - Measure response latency

set -euo pipefail

echo "=== Marketplace Latency Measurement ==="
echo "Measuring ERLMCP response latency"
echo ""

PROJECT_ID=$(gcloud config get-value project)
INSTANCE_NAME="erlmcp-marketplace-test"
ZONE="us-central1-a"

# Get instance external IP
EXTERNAL_IP=$(gcloud compute instances describe "${INSTANCE_NAME}" --zone="${ZONE}" --format='get(networkInterfaces[0].accessConfigs[0].natIP)')

if [[ -z "${EXTERNAL_IP}" ]]; then
    echo "ERROR: Could not get external IP"
    exit 1
fi

echo "Target endpoint: http://${EXTERNAL_IP}:8080"
echo ""

echo "Test 1: DNS Lookup Time"
echo "-----------------------"
DNS_TIMES=()
for i in {1..10}; do
    start=$(date +%s%N)
    nslookup "${EXTERNAL_IP}" >/dev/null 2>&1
    end=$(date +%s%N)
    dns_time=$(( (end - start) / 1000000 ))
    DNS_TIMES+=(${dns_time})
done

echo "DNS lookup times (ms): ${DNS_TIMES[@]}"
echo "Average: $(echo "${DNS_TIMES[@]}" | awk '{sum+=$1; count++} END {print sum/count}') ms"

echo ""
echo "Test 2: TCP Connection Time"
echo "--------------------------"
TCP_TIMES=()
for i in {1..10}; do
    start=$(date +%s%N)
    (echo >/dev/tcp/"${EXTERNAL_IP}"/8080) >/dev/null 2>&1
    end=$(date +%s%N)
    tcp_time=$(( (end - start) / 1000000 ))
    TCP_TIMES+=(${tcp_time})
done

echo "TCP connection times (ms): ${TCP_TIMES[@]}"
echo "Average: $(echo "${TCP_TIMES[@]}" | awk '{sum+=$1; count++} END {print sum/count}') ms"

echo ""
echo "Test 3: HTTP Response Time (TTFB)"
echo "---------------------------------"
TTFB_TIMES=()
for i in {1..20}; do
    ttfb=$(curl -o /dev/null -s -w "%{time_starttransfer}\n" "http://${EXTERNAL_IP}:8080/")
    TTFB_TIMES+=(${ttfb}))
done

echo "Time to First Byte (seconds): ${TTFB_TIMES[@]}"
echo "Average: $(echo "${TTFB_TIMES[@]}" | awk '{sum+=$1; count++} END {print sum/count}') s"

echo ""
echo "Test 4: Total Request Time"
echo "-------------------------"
TOTAL_TIMES=()
for i in {1..20}; do
    total=$(curl -o /dev/null -s -w "%{time_total}\n" "http://${EXTERNAL_IP}:8080/")
    TOTAL_TIMES+=(${total}))
done

echo "Total request time (seconds): ${TOTAL_TIMES[@]}"
echo "Average: $(echo "${TOTAL_TIMES[@]}" | awk '{sum+=$1; count++} END {print sum/count}') s"

echo ""
echo "Test 5: Latency Percentiles"
echo "--------------------------"
echo "TTFB percentiles:"
echo "${TTFB_TIMES[@]}" | tr ' ' '\n' | sort -n | awk '
BEGIN {count=0}
{times[count++]=$1}
END {
    print "  50th percentile (median): " times[int(count*0.5)]
    print "  95th percentile: " times[int(count*0.95)]
    print "  99th percentile: " times[int(count*0.99)]
}'

echo ""
echo "Test 6: Network Latency (ping)"
echo "-----------------------------"
if command -v ping >/dev/null 2>&1; then
    echo "Pinging ${EXTERNAL_IP} (10 packets)..."
    ping -c 10 "${EXTERNAL_IP}" || echo "Ping failed"
else
    echo "ping command not available"
fi

echo ""
echo "=== Latency Measurement Complete ==="
