---
name: Quality Gate Failure
about: Report quality gate failures in CI/CD
title: '[QUALITY] '
labels: quality, ci-cd
assignees: ''

---

## Quality Gate Failure
Which quality gate failed?

- [ ] Compilation
- [ ] EUnit Tests
- [ ] Common Test
- [ ] Coverage (<80%)
- [ ] Dialyzer (type checking)
- [ ] Xref (undefined functions)
- [ ] Code formatting
- [ ] Security scan
- [ ] Performance regression
- [ ] Other: _________

## Error Details
```
Paste error output here
```

## Impact
What does this block?

- [ ] Release deployment
- [ ] PR merge
- [ ] Development workflow
- [ ] Other: _________

## Resolution Plan
How should this be fixed?

**Immediate Actions**:
-
-

**Long-term Fixes**:
-
-

## Root Cause Analysis
What caused this failure?

- [ ] Code defect
- [ ] Test issue
- [ ] Configuration problem
- [ ] Dependency issue
- [ ] Environment issue
- [ ] Other: _________

## Additional Context
Any other context about the quality gate failure.

## Quality Metrics
**Current State**:
- Compilation: ___ errors
- Tests: ___% pass rate
- Coverage: ___%
- Dialyzer: ___ warnings
- Xref: ___ issues

**Required State**:
- Compilation: 0 errors
- Tests: 100% pass rate
- Coverage: 80%+
- Dialyzer: 0 errors (blocking)
- Xref: 0 issues
