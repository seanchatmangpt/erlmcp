#!/usr/bin/env python3
"""
SPARC Workflow Automation Script
Aids in following the SPARC methodology for structured development
"""

import os
import sys
import json
import argparse
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional

class SPARCWorkflow:
    def __init__(self, project_path: str, phase: str = None):
        self.project_path = Path(project_path)
        self.phase = phase
        self.docs_dir = self.project_path / "docs"
        self.src_dir = self.project_path / "src"
        self.tests_dir = self.project_path / "tests"

        # Create directories if they don't exist
        self.docs_dir.mkdir(exist_ok=True)
        self.src_dir.mkdir(exist_ok=True)
        self.tests_dir.mkdir(exist_ok=True)

        # Phase-specific directories
        self.phase_dirs = {
            'spec': self.docs_dir / "specification",
            'pseudo': self.docs_dir / "pseudocode",
            'arch': self.docs_dir / "architecture",
            'refine': self.src_dir,
            'complete': self.docs_dir / "completion"
        }

        for phase_dir in self.phase_dirs.values():
            phase_dir.mkdir(exist_ok=True)

    def initialize_sparc_project(self):
        """Initialize a new SPARC project structure"""
        print("🚀 Initializing SPARC project structure...")

        # Create phase directories
        for phase, path in self.phase_dirs.items():
            path.mkdir(exist_ok=True)
            print(f"  Created: {path}")

        # Create initial specification document
        spec_doc = self.phase_dirs['spec'] / "requirements.md"
        if not spec_doc.exists():
            spec_doc.write_text(self._get_requirements_template())
            print(f"  Created: {spec_doc}")

        # Create project README
        readme_path = self.project_path / "README.md"
        if not readme_path.exists():
            readme_path.write_text(self._get_readme_template())
            print(f"  Created: {readme_path}")

        print("✅ SPARC project initialized successfully!")

    def run_phase(self):
        """Run specific SPARC phase"""
        if not self.phase:
            print("❌ Please specify a phase: spec, pseudo, arch, refine, complete")
            return

        phase_handlers = {
            'spec': self._specification_phase,
            'pseudo': self._pseudocode_phase,
            'arch': self._architecture_phase,
            'refine': self._refinement_phase,
            'complete': self._completion_phase
        }

        if self.phase in phase_handlers:
            print(f"🎯 Running {self.phase.upper()} phase...")
            phase_handlers[self.phase]()
        else:
            print(f"❌ Unknown phase: {self.phase}")

    def _specification_phase(self):
        """Guide through Specification phase"""
        print("\n📋 SPECIFICATION PHASE")
        print("=" * 50)

        spec_file = self.phase_dirs['spec'] / "requirements.md"
        if not spec_file.exists():
            spec_file.write_text(self._get_requirements_template())

        print("\n📝 Tasks to complete:")
        print("  1. Review and complete requirements.md")
        print("  2. Write user stories")
        print("  3. Define acceptance criteria")
        print("  4. Specify data models")
        print("  5. Document API endpoints")

        print("\n📋 Current requirements:")
        try:
            content = spec_file.read_text()
            print(content)
        except Exception as e:
            print(f"Error reading requirements: {e}")

        print("\n✅ Phase complete when:")
        print("  - All user stories are defined")
        print("  - Acceptance criteria are clear")
        print("  - Data models are specified")
        print("  - API endpoints are documented")

    def _pseudocode_phase(self):
        """Guide through Pseudocode phase"""
        print("\n🔧 PSEUDOCODE PHASE")
        print("=" * 50)

        pseudo_file = self.phase_dirs['pseudo'] / "algorithms.md"
        if not pseudo_file.exists():
            pseudo_file.write_text(self._get_pseudocode_template())

        print("\n📝 Tasks to complete:")
        print("  1. Write algorithmic descriptions")
        print("  2. Define data flow")
        print("  3. Specify error handling")
        print("  4. Plan edge cases")
        print("  5. Define success/failure conditions")

        print("\n📋 Current algorithms:")
        try:
            content = pseudo_file.read_text()
            print(content)
        except Exception as e:
            print(f"Error reading algorithms: {e}")

        print("\n✅ Phase complete when:")
        print("  - All major operations are described")
        print("  - Error handling is specified")
        print("  - Edge cases are considered")
        print("  - Logic flows are clear")

    def _architecture_phase(self):
        """Guide through Architecture phase"""
        print("\n🏗️ ARCHITECTURE PHASE")
        print("=" * 50)

        arch_file = self.phase_dirs['arch'] / "design.md"
        if not arch_file.exists():
            arch_file.write_text(self._get_architecture_template())

        print("\n📝 Tasks to complete:")
        print("  1. Design system layers")
        print("  2. Create component diagrams")
        print("  3. Plan database schema")
        print("  4. Design API structure")
        print("  5. Plan for scalability")

        print("\n📋 Current architecture:")
        try:
            content = arch_file.read_text()
            print(content)
        except Exception as e:
            print(f"Error reading architecture: {e}")

        print("\n✅ Phase complete when:")
        print("  - System layers are defined")
        print("  - Components are identified")
        print("  - Database schema is designed")
        print("  - API structure is planned")

    def _refinement_phase(self):
        """Guide through Refinement phase"""
        print("\n💻 REFINEMENT PHASE")
        print("=" * 50)

        print("\n📝 Tasks to complete:")
        print("  1. Write tests first (TDD)")
        print("  2. Implement incrementally")
        print("  3. Refactor for clarity")
        print("  4. Address technical debt")
        print("  5. Run tests frequently")

        # Check test files
        test_files = list(self.tests_dir.glob("*.py"))
        if test_files:
            print(f"\n📋 Found {len(test_files)} test files:")
            for test_file in test_files:
                print(f"  - {test_file}")
        else:
            print("\n📝 No test files found. Create tests first:")
            print("  - Write unit tests for core functionality")
            print("  - Write integration tests")
            print("  - Write edge case tests")

        # Check source files
        source_files = list(self.src_dir.glob("*.py"))
        if source_files:
            print(f"\n📋 Found {len(source_files)} source files:")
            for source_file in source_files:
                print(f"  - {source_file}")
        else:
            print("\n📝 No source files found. Implement after tests.")

        print("\n✅ Phase complete when:")
        print("  - All tests pass")
        print("  - Code is well-structured")
        print("  - Technical debt is minimal")
        print("  - Performance is adequate")

    def _completion_phase(self):
        """Guide through Completion phase"""
        print("\n✅ COMPLETION PHASE")
        print("=" * 50)

        print("\n📝 Tasks to complete:")
        print("  1. Integration testing")
        print("  2. Performance optimization")
        print("  3. Documentation completion")
        print("  4. Security review")
        print("  5. Deployment preparation")

        # Check documentation
        doc_files = list(self.docs_dir.rglob("*.md"))
        print(f"\n📋 Found {len(doc_files)} documentation files:")
        for doc_file in doc_files[:10]:  # Show first 10
            print(f"  - {doc_file.relative_to(self.project_path)}")

        # Check test coverage
        if (self.project_path / "coverage.xml").exists():
            print("\n📊 Coverage report found")

        print("\n✅ Phase complete when:")
        print("  - All requirements are met")
        print("  - Performance is adequate")
        print("  - Documentation is complete")
        print("  - System is deployable")

    def generate_report(self):
        """Generate SPARC progress report"""
        print("\n📊 SPARC Progress Report")
        print("=" * 50)

        report = {
            'generated_at': datetime.now().isoformat(),
            'project_path': str(self.project_path),
            'phases': {}
        }

        for phase, phase_dir in self.phase_dirs.items():
            files = list(phase_dir.rglob("*.md")) + list(phase_dir.rglob("*.py"))
            report['phases'][phase] = {
                'directory': str(phase_dir),
                'file_count': len(files),
                'files': [str(f.relative_to(self.project_path)) for f in files]
            }

        # Save report
        report_file = self.project_path / "sparc_report.json"
        report_file.write_text(json.dumps(report, indent=2))

        print("📄 Report saved to:", report_file)

        # Print summary
        print("\n📈 Phase Summary:")
        for phase, data in report['phases'].items():
            status = "✅" if data['file_count'] > 0 else "⏸️"
            print(f"  {status} {phase.upper()}: {data['file_count']} files")

    def _get_requirements_template(self):
        """Get specification template"""
        return """# 📋 Project Requirements Specification

## Project Overview
[Describe the project and its purpose]

## User Stories

### Primary User Stories
- [ ] As a [user type], I want [goal] so that [benefit]
- [ ] As a [user type], I want [goal] so that [benefit]
- [ ] As a [user type], I want [goal] so that [benefit]

### Secondary User Stories
- [ ] As a [user type], I want [goal] so that [benefit]
- [ ] As a [user type], I want [goal] so that [benefit]

## Acceptance Criteria

### Must-Have (Critical)
- [ ] Acceptance criteria 1
- [ ] Acceptance criteria 2
- [ ] Acceptance criteria 3

### Should-Have (Important)
- [ ] Acceptance criteria 4
- [ ] Acceptance criteria 5

### Nice-to-Have (Optional)
- [ ] Acceptance criteria 6

## Data Models

### Primary Entities
[Define your main entities and their relationships]

### Secondary Entities
[Define supporting entities]

## API Endpoints

### REST Endpoints
- [ ] METHOD /endpoint - Description
- [ ] METHOD /endpoint - Description
- [ ] METHOD /endpoint - Description

### WebSocket Endpoints (if applicable)
- [ ] ws://endpoint - Description

## Constraints

### Technical Constraints
- [ ] Technology stack restrictions
- [ ] Performance requirements
- [ ] Scalability requirements

### Business Constraints
- [ ] Regulatory requirements
- [ ] Security requirements
- [ ] Compliance requirements

## Success Metrics
[Define how to measure success]

## Non-Functional Requirements
- Performance: [Define performance targets]
- Security: [Define security requirements]
- Usability: [Define usability requirements]
- Reliability: [Define reliability requirements]
"""

    def _get_pseudocode_template(self):
        """Get pseudocode template"""
        return """# 🔧 Algorithm Descriptions

## Core Operations

### Operation 1: [Function Name]
```
FUNCTION [function_name](parameters):
    # Purpose: [Describe what the function does]
    # Input: [Describe input parameters]
    # Output: [Describe return value]

    # Algorithm steps:
    # 1. [Step 1]
    # 2. [Step 2]
    # 3. [Step 3]

    # Error handling:
    # - [Error type]: [Handle strategy]
    # - [Error type]: [Handle strategy]

    RETURN result
```

### Operation 2: [Function Name]
```
FUNCTION [function_name](parameters):
    # Purpose: [Describe what the function does]
    # Input: [Describe input parameters]
    # Output: [Describe return value]

    # Algorithm steps:
    # 1. [Step 1]
    # 2. [Step 2]
    # 3. [Step 3]

    # Edge cases:
    # - [Case 1]: [Handle strategy]
    # - [Case 2]: [Handle strategy]

    RETURN result
```

## Data Flow

### [Process Name] Flow
```
Start
├─ Step 1: [Description]
├─ Step 2: [Description]
├─ Decision: [Condition]
│   ├─ Yes: [Action]
│   └─ No: [Action]
└─ End: [Result]
```

## Error Handling Patterns

### Pattern 1: [Error Type]
```
TRY:
    # Risky operation
CATCH [Error Type]:
    # Recovery strategy
FINALLY:
    # Cleanup
```

### Pattern 2: [Error Type]
```
IF [condition]:
    # Handle gracefully
ELSE:
    # Throw appropriate error
```

## Success/Failure Conditions

### Success Conditions
- [Condition 1]
- [Condition 2]
- [Condition 3]

### Failure Conditions
- [Condition 1]
- [Condition 2]
- [Condition 3]
"""

    def _get_architecture_template(self):
        """Get architecture template"""
        return """# 🏗️ System Architecture

## System Overview
[Describe the overall system architecture]

## System Layers

### Presentation Layer
- [ ] API endpoints (REST/GraphQL)
- [ ] WebSocket handlers
- [ ] Request/response models

### Business Logic Layer
- [ ] Service classes
- [ ] Business rules
- [ ] Workflow orchestration

### Data Access Layer
- [ ] Repository pattern
- [ ] Database connections
- [ ] Cache management

### Infrastructure Layer
- [ ] Database configuration
- [ ] External services
- [ ] Message queues

## Component Diagram

```
[Component diagram or ASCII art]
```

## Database Schema

### Tables
- [ ] table_name: [description of columns and relationships]
- [ ] table_name: [description of columns and relationships]

### Indexes
- [ ] table(column) for [reason]
- [ ] table(column) for [reason]

## API Design

### REST Endpoints
```yaml
- method: GET
  path: /endpoint
  description: Description
  request: {schema}
  response: {schema}
  auth: required
  rate_limit: 100/hour
```

### Data Models
```python
class ModelName:
    field1: type  # Description
    field2: type  # Description
```

## Scalability Considerations

### Horizontal Scaling
- [ ] Load balancing strategy
- [ ] Database replication
- [ ] Caching strategy

### Performance Optimization
- [ ] Query optimization
- [ ] Indexing strategy
- [ ] Connection pooling

## Security Architecture

### Authentication
- [ ] Method: [JWT/OAuth/API Key]
- [ ] Implementation details

### Authorization
- [ ] Role-based access control
- [ ] Permission levels

### Data Protection
- [ ] Encryption at rest
- [ ] Encryption in transit
- [ ] Data masking

## Deployment Architecture

### Production Environment
- [ ] Infrastructure: [Cloud/On-premise]
- [ ] Containerization: [Docker/Kubernetes]
- [ ] Monitoring: [Prometheus/Grafana]

### Staging Environment
- [ ] Configuration: [Describe setup]
- [ ] Testing: [Describe process]
"""

    def _get_readme_template(self):
        """Get README template"""
        return """# Project Name

## Overview
[Project description]

## SPARC Development Methodology

This project follows the SPARC methodology:

- **S**pecification: Requirements and user stories
- **P**seudocode: Algorithm descriptions
- **A**rchitecture: System design
- **R**efinement: Implementation with tests
- **C**ompletion: Integration and deployment

## Quick Start

1. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

2. Run tests:
   ```bash
   pytest
   ```

3. Run development server:
   ```bash
   python -m uvicorn main:app --reload
   ```

## Project Structure

```
project/
├── docs/                 # Documentation
│   ├── specification/   # Phase 1: Specification
│   ├── pseudocode/      # Phase 2: Pseudocode
│   ├── architecture/    # Phase 3: Architecture
│   └── completion/      # Phase 5: Completion
├── src/                 # Source code (Phase 4: Refinement)
├── tests/               # Test files
└── scripts/             # Utility scripts
```

## Development Workflow

1. Specification Phase: Define requirements
2. Pseudocode Phase: Design algorithms
3. Architecture Phase: Design system structure
4. Refinement Phase: Implement with tests
5. Completion Phase: Integration and deployment

## Running Tests

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=src

# Run specific test file
pytest tests/test_file.py
```

## Documentation

- [API Documentation](docs/api.md)
- [Architecture Overview](docs/architecture/README.md)
- [Requirements](docs/specification/requirements.md)

## Contributing

1. Follow SPARC methodology
2. Write tests before code
3. Document your changes
4. Run tests before committing

## License

[License information]
"""

def main():
    parser = argparse.ArgumentParser(description='SPARC Workflow Automation')
    parser.add_argument('action', choices=['init', 'phase', 'report'],
                       help='Action to perform')
    parser.add_argument('project_path', nargs='?', default='.',
                       help='Project path (default: current directory)')
    parser.add_argument('--phase', choices=['spec', 'pseudo', 'arch', 'refine', 'complete'],
                       help='Phase to run (for phase action)')

    args = parser.parse_args()

    workflow = SPARCWorkflow(args.project_path, args.phase)

    if args.action == 'init':
        workflow.initialize_sparc_project()
    elif args.action == 'phase':
        workflow.run_phase()
    elif args.action == 'report':
        workflow.generate_report()

if __name__ == '__main__':
    main()