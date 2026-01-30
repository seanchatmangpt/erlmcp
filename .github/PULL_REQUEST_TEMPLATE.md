---
name: Pull Request
about: Standard PR template for erlmcp
---

## Summary
<!-- Describe the changes in 1-2 sentences -->

This PR implements _________ for the purpose of _________.

## Type of Change
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Code refactoring
- [ ] Test improvement
- [ ] CI/CD update

## Related Issues
Fixes #
Relates to #
Blocks #

## Changes Made
<!-- List the main changes -->

### Core
-
-

### Tests
-
-

### Documentation
-
-

### CI/CD
-
-

## Quality Gates

### Compilation
- [ ] 0 errors
- [ ] 0 warnings (or justified)

### Tests
- [ ] EUnit: 100% pass rate
- [ ] Common Test: 100% pass rate
- [ ] Coverage: 80%+ (current: ____%)

### Type Safety
- [ ] Dialyzer: 0 errors
- [ ] Xref: 0 undefined functions
- [ ] Specs: Complete on public APIs

### Code Quality
- [ ] Formatted with `rebar3 format`
- [ ] No hardcoded secrets
- [ ] Chicago School TDD (real processes, no mocks)

### Performance (if applicable)
- [ ] Benchmarks run
- [ ] <10% regression
- [ ] Metrology-compliant metrics

## Test Plan
<!-- Describe how you tested these changes -->

### Unit Tests
- [ ] All existing tests pass
- [ ] New tests added for new functionality
- [ ] Edge cases covered

### Integration Tests
- [ ] Integration tests pass
- [ ] Real process testing (Chicago TDD)
- [ ] Transport-specific tests

### Manual Testing
- [ ] Tested in local environment
- [ ] Tested with [specific transport/client]
- [ ] Manual verification checklist

### Performance Testing
- [ ] Benchmarks run
- [ ] No regressions detected
- [ ] Performance report attached

## Breaking Changes
<!-- List any breaking changes -->

### API Changes
-
-

### Configuration Changes
-
-

### Migration Required
-
-

## Documentation
- [ ] Code documented with specs and types
- [ ] Public API documentation updated
- [ ] Architecture docs updated (if applicable)
- [ ] CHANGELOG.md updated

## Checklist
- [ ] My code follows the style guidelines of this project
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes
- [ ] Any dependent changes have been merged and published

## Screenshots (if applicable)
<!-- Add screenshots for UI/UX changes -->

## Additional Notes
<!-- Any additional information for reviewers -->

## Reviewers
@reviewer1 @reviewer2

## Deployment Notes
<!-- Any special deployment considerations -->

---

**Quality Gate Results:**

```bash
# Compilation
TERM=dumb rebar3 compile
# Result: ✅ 0 errors

# Tests
rebar3 eunit
# Result: ✅ 100% pass rate

# Coverage
rebar3 cover
# Result: ✅ 80%+ (actual: ____%)

# Dialyzer
rebar3 dialyzer
# Result: ✅ 0 errors

# Xref
rebar3 xref
# Result: ✅ 0 undefined functions
```
