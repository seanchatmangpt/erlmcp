# Forward Deployment Context

This repository is part of the **Chatman Ecosystem**, a portfolio built to make forward deployment repeatable, governed, and evidence-bearing.

Sean Chatman is publicly documenting the case for **The 2,001st Forward-Deployed Agentic Architect** while building the **operating system for forward deployment**.

## Local role

Within that portfolio, `erlmcp` is a fault-tolerant Model Context Protocol transport and supervision surface. It connects AI programs to bounded tools, resources, and customer-system capabilities while preserving isolation, failure transparency, concurrency, and recoverable service behavior.

```text
agent request → protocol parsing and routing → capability admission
→ tool intent → authority boundary → execution result or typed failure
→ receipt → replay or supervised recovery
```

Forward deployment depends on reliable integration more than persuasive model output. Protocol objects expose capabilities; they do not grant ambient authority to use them.

```text
A = μ(O*)
R = receipt(A)
```

## Boundaries

- This file does not replace the repository’s protocol conformance, OTP supervision design, license, or exact maturity status.
- Connector discovery is not equivalent to mounted or executable capability.
- Transport success is not equivalent to successful tool consequence.
- Hooks and handlers may manufacture intents; consequential actions require explicit authority.
- Every success, refusal, timeout, unsupported capability, and failure should remain distinguishable and receiptable.

The canonical portfolio narrative is maintained in `seanchatmangpt/chatman-ecosystem`.
