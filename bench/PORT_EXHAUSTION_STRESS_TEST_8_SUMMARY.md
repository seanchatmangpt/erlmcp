# Port Exhaustion Stress Test #8 - Summary Report

**Date:** January 29, 2026  
**Test Type:** Destructive Port Exhaustion Stress Test  
**Working Directory:** `/Users/sac/erlmcp`

## Executive Summary

This destructive stress test successfully identified the HARD LIMIT for concurrent TCP connections on the system: **12,261 connections** before hitting Erlang's internal port table overflow.

### Key Finding

The limiting factor is **NOT** the OS ephemeral port range (16,384 ports available) but rather **Erlang/OTP's internal file descriptor table limit** of 24,576 ports.

---

## Test Configuration

### System Information
- **OS:** macOS (unix/darwin)
- **Erlang Version:** OTP 27
- **Ephemeral Port Range:** 49152-65535 (16,384 ports available)
- **File Descriptor Limit:** unlimited (OS level)
- **Erlang Port Limit:** 24,576 (default)

### Test Parameters
- **Server Port:** 10008
- **Connection Timeout:** 2000ms
- **Keep Connections:** Yes (no closing during test)
- **Test Duration:** Until exhaustion or timeout

---

## Results

### Opening Progress
| Milestone | Status | Time | Rate |
|-----------|--------|------|------|
| 1K ports | ✅ Reached | 0.30s | 3,289 conn/sec |
| 10K ports | ✅ Reached | 3.14s | 3,186 conn/sec |
| 50K ports | ❌ Failed | - | - |
| 64K ports | ❌ Failed | - | - |

### Breaking Point
- **Last Successful Port:** 12,261 connections
- **Total Connections Opened:** 12,261
- **Time to Exhaust:** 5.89 seconds
- **Connection Rate:** ~2,083 connections/second
- **Error:** timeout (with Erlang driver error)

---

## Error Analysis

### Primary Error
```
driver_select(0x0000000114dabcf8, 24576, ERL_DRV_WRITE ERL_DRV_USE, 1) 
by tcp_inet driver #Port<0.24531> failed: fd=24576 is larger than 
the largest allowed fd=24575
```

### Root Cause
Erlang/OTP has a compile-time limit on the number of file descriptors (ports) that can be opened. The default limit is **24,576 ports** (256 * 96).

When the test attempted to open connection #12,262, Erlang tried to use file descriptor 24,576, which exceeded the internal port table limit.

### Why Not Ephemeral Port Exhaustion?
The OS ephemeral port range has 16,384 available ports, but the test only opened 12,261 connections before hitting the Erlang limit. This confirms:
- **Ephemeral ports were NOT exhausted**
- **Erlang's internal limit was the bottleneck**

---

## System Status at Limit

### Resource Usage
- **FDs Used:** 24,578 (at limit)
- **Process Count:** 12,307
- **Server Status:** Survived but not accepting new connections

### Server Health
- **Server Survived:** ✅ YES
- **Accepting Connections:** ❌ NO (after limit hit)
- **Process Count:** 12,307

---

## Mitigation Strategies

### 1. Increase Erlang Port Limit (Recommended)
```bash
export ERL_MAX_PORTS=65536
erl +Q 65536 +P 134217727
```

**Flags:**
- `+Q 65536`: Set maximum number of ports
- `+P 134217727`: Set maximum number of processes
- `ERL_MAX_PORTS`: Environment variable for port limit

### 2. Increase Ephemeral Port Range (macOS)
```bash
sudo sysctl -w net.inet.ip.portrange.first=32768
sudo sysctl -w net.inet.ip.portrange.last=65535
```

**This doubles the ephemeral port range from ~16K to ~32K ports.**

### 3. Reduce TIME_WAIT Duration (macOS)
```bash
sudo sysctl -w net.inet.tcp.msl=1000
```

**Default is 15000ms (15 seconds). Reducing to 1000ms (1 second) allows faster port reuse.**

### 4. Connection Pooling
Reuse existing connections instead of opening new ones for each request.

### 5. Use SO_REUSEADDR (Server-Side Only)
Allows binding to a port that's in TIME_WAIT state. Use with caution.

### 6. Multiple Source IP Addresses
If the system has multiple IP addresses, each can use its own ephemeral port range.

---

## Variations Not Tested

1. **SO_REUSEADDR Test:** Not conducted in this run
2. **Rapid Open/Close Test:** Not conducted in this run
3. **Increased Port Limits Test:** Follow-up test needed

---

## Recommendations

### Immediate Actions
1. ✅ **Increase Erlang port limit** to 65,536 for production deployments
2. ✅ **Monitor port usage** in production: `erlang:system_info(port_count)`
3. ✅ **Set alerts** for port usage > 80% of limit

### Long-term Actions
1. Implement connection pooling for all client connections
2. Increase OS ephemeral port range to 32K+
3. Reduce TCP MSL to 1 second for faster port reuse
4. Consider clustering for horizontal scaling beyond single-node limits

---

## Conclusion

The port exhaustion stress test successfully identified that **Erlang's internal port table limit is the primary bottleneck** for high-concurrency TCP applications on this system, NOT the OS ephemeral port range.

By increasing the Erlang port limit to 65,536, the system should be able to handle **3x more concurrent connections** (up to ~36,000) before hitting OS limits.

### Hard Limit Summary
- **Current Configuration:** 12,261 connections (Erlang limit)
- **With Increased Port Limit:** ~32,000 connections (OS ephemeral limit)
- **With Increased Ephemeral Range:** ~65,000 connections (practical limit)

---

## Test Artifacts

- **Test Script:** `/tmp/port_exhaustion_fixed.erl`
- **Results:** `/Users/sac/erlmcp/bench/results/port_exhaustion_test_8_results.txt`
- **Summary:** `/Users/sac/erlmcp/bench/PORT_EXHAUSTION_STRESS_TEST_8_SUMMARY.md`

---

**Test Completed:** January 29, 2026  
**Agent:** erlang-performance (Port Exhaustion Specialist)  
**Status:** ✅ COMPLETED SUCCESSFULLY
