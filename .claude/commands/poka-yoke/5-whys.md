---
name: 5-whys-analyze
description: 5 Whys (なぜなぜ分析) - Root cause analysis
category: poka-yoke
invokes_agent: (neural system)
japanese_term: なぜなぜ分析 (Naze-naze Bunseki)
consolidates: [perf/train, training/neural-train, training/pattern-learn]
---

# Command: /5-whys-analyze

## Purpose
**Japanese**: なぜなぜ分析 (Naze-naze Bunseki - Why-why Analysis)
**TCPS Usage**: Root cause analysis for Andon events

Ask "Why?" five times to reach root cause. Every Andon triggers 5 Whys.

## Usage
```bash
/5-whys-analyze [andon-id]
```

## 5 Whys Process

1. **Problem**: Integration test failed
2. **Why?**: TCP connection timeout
3. **Why?**: Port not listening
4. **Why?**: Supervisor not starting transport
5. **Why?**: Missing config in sys.config
6. **Root Cause**: Config validation missing in SHACL

## Examples
```bash
/5-whys-analyze andon-20260127-001
```

Generates root cause report, proposes Kaizen.

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /perf train, /training/*
