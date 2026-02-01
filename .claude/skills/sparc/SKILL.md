# SPARC Methodology

SPARC = **S**pecification, **P**seudocode, **A**rchitecture, **R**efinement, **C**ompletion

## Workflow

### 1. Specification
- Define functional requirements
- Identify edge cases
- Document invariants
- Output: Spec document

### 2. Pseudocode
- Algorithm design (language-agnostic)
- Data flow diagrams
- Interface definitions
- Output: Pseudocode document

### 3. Architecture
- Supervision tree design
- Module organization
- Protocol definitions
- Output: Architecture document

### 4. Refinement
- TDD implementation (tests first)
- Chicago School: real processes, no mocks
- Iterative refinement
- Output: Working code + tests

### 5. Completion
- Quality gates (compile, test, coverage)
- Documentation
- Receipt generation
- Output: Verified feature

## Commands

Use via `/sparc` skill or SPARC orchestrator agent.

## Quality Gates

At each phase:
- [ ] Reviews completed
- [ ] Tests passing
- [ ] Coverage >= 80%
- [ ] Documentation updated
- [ ] Receipts generated

## References

- `CLAUDE.md` for SPARC workflow details
- `.claude/agents/sparc-orchestrator.md`
