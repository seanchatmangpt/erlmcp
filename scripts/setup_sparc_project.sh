#!/bin/bash

# SPARC Project Setup Script
# Creates a new project with SPARC methodology structure

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if project name is provided
if [ -z "$1" ]; then
    print_error "Project name is required!"
    echo "Usage: $0 <project_name> [language]"
    echo "Languages: python, javascript, go, java"
    exit 1
fi

PROJECT_NAME="$1"
LANGUAGE="${2:-python}"
PROJECT_DIR="./$PROJECT_NAME"

print_status "Setting up SPARC project: $PROJECT_NAME"

# Create project directory
if [ -d "$PROJECT_DIR" ]; then
    print_warning "Project directory already exists. Creating anyway..."
fi

mkdir -p "$PROJECT_DIR"
cd "$PROJECT_DIR"

# Create SPARC project structure
print_status "Creating SPARC project structure..."

# Phase directories
mkdir -p docs/{specification,pseudocode,architecture,completion}
mkdir -p src
mkdir -p tests
mkdir -p scripts
mkdir -p examples

# Create README
print_status "Creating README..."
cat > README.md << 'EOF'
# Project Name

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
   # Follow language-specific instructions
   ```

2. Run tests:
   ```bash
   # Follow language-specific instructions
   ```

3. Run development server:
   ```bash
   # Follow language-specific instructions
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
├── scripts/             # Utility scripts
└── examples/            # Example usage
```

## Development Workflow

1. Specification Phase: Define requirements
2. Pseudocode Phase: Design algorithms
3. Architecture Phase: Design system structure
4. Refinement Phase: Implement with tests
5. Completion Phase: Integration and deployment

## Running Tests

```bash
# Follow language-specific instructions
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

EOF

# Create requirements based on language
print_status "Creating language-specific setup..."

case $LANGUAGE in
    python)
        # Python setup
        cat > requirements.txt << 'EOF'
# Core dependencies
fastapi>=0.100.0
uvicorn>=0.20.0
pytest>=7.0.0
pytest-asyncio>=0.21.0
pytest-cov>=4.0.0

# Documentation
mkdocs>=1.5.0
mkdocs-material>=9.0.0

# Quality
black>=23.0.0
flake8>=6.0.0
mypy>=1.0.0

# Database (example)
sqlalchemy>=2.0.0
alembic>=1.12.0
EOF

        cat > setup.py << 'EOF'
from setuptools import setup, find_packages

setup(
    name="$PROJECT_NAME",
    version="0.1.0",
    description="SPARC project",
    packages=find_packages(),
    install_requires=open("requirements.txt").read().splitlines(),
    python_requires=">=3.8",
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
    ],
)
EOF

        # Create main.py example
        cat > src/main.py << 'EOF'
"""
Main application entry point
"""
from fastapi import FastAPI

app = FastAPI()

@app.get("/")
def read_root():
    return {"message": "Welcome to SPARC project"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
EOF

        # Create test example
        cat > tests/test_main.py << 'EOF'
"""
Test main application
"""
import pytest
from fastapi.testclient import TestClient
from src.main import app

client = TestClient(app)

def test_read_root():
    response = client.get("/")
    assert response.status_code == 200
    assert response.json() == {"message": "Welcome to SPARC project"}
EOF

        cat > .gitignore << 'EOF'
# Python
__pycache__/
*.py[cod]
*$py.class
*.so
.Python
build/
develop-eggs/
dist/
downloads/
eggs/
.eggs/
lib/
lib64/
parts/
sdist/
var/
wheels/
*.egg-info/
.installed.cfg
*.egg

# Virtual environments
venv/
env/
ENV/

# IDE
.vscode/
.idea/
*.swp
*.swo

# Logs
*.log

# Testing
.coverage
htmlcov/
.pytest_cache/

# OS
.DS_Store
Thumbs.db
EOF
        ;;

    javascript)
        # JavaScript setup
        cat > package.json << 'EOF'
{
  "name": "$PROJECT_NAME",
  "version": "1.0.0",
  "description": "SPARC project",
  "main": "src/index.js",
  "scripts": {
    "test": "jest",
    "test:coverage": "jest --coverage",
    "start": "node src/index.js",
    "dev": "nodemon src/index.js"
  },
  "dependencies": {
    "express": "^4.18.0"
  },
  "devDependencies": {
    "jest": "^29.5.0",
    "nodemon": "^3.0.0",
    "supertest": "^6.3.0"
  },
  "keywords": ["sparc", "development"],
  "author": "",
  "license": "MIT"
}
EOF

        cat > src/index.js << 'EOF'
/**
 * Main application entry point
 */
const express = require('express');
const app = express();

app.get('/', (req, res) => {
  res.json({ message: 'Welcome to SPARC project' });
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});

module.exports = app;
EOF

        cat > tests/test.js << 'EOF'
/**
 * Test main application
 */
const request = require('supertest');
const app = require('../src/index.js');

describe('GET /', () => {
  it('should return welcome message', async () => {
    const response = await request(app).get('/');
    expect(response.statusCode).toBe(200);
    expect(response.body).toEqual({ message: 'Welcome to SPARC project' });
  });
});
EOF

        cat > .gitignore << 'EOF'
# Dependencies
node_modules/

# Logs
npm-debug.log*
yarn-debug.log*
yarn-error.log*

# Runtime data
pids
*.pid
*.seed
*.pid.lock

# Coverage directory used by tools like istanbul
coverage/

# IDE
.vscode/
.idea/
*.swp
*.swo

# OS
.DS_Store
Thumbs.db

# Optional npm cache directory
.npm

# Optional eslint cache
.eslintcache
EOF
        ;;

    go)
        # Go setup
        cat > go.mod << 'EOF'
module $PROJECT_NAME

go 1.21

require (
)
EOF

        cat > src/main.go << 'EOF'
package main

import (
	"encoding/json"
	"net/http"

	"github.com/gorilla/mux"
)

func main() {
	r := mux.NewRouter()

	r.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{
			"message": "Welcome to SPARC project",
		})
	})

	http.ListenAndServe(":8080", r)
}
EOF

        cat > tests/main_test.go << 'EOF'
package main

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/gorilla/mux"
)

func TestRootHandler(t *testing.T) {
	r := mux.NewRouter()
	r.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.Write([]byte("{\"message\":\"Welcome to SPARC project\"}"))
	})

	req := httptest.NewRequest("GET", "/", nil)
	w := httptest.NewRecorder()

	r.ServeHTTP(w, req)

	if status := w.Code; status != http.StatusOK {
		t.Errorf("handler returned wrong status code: got %v want %v", status, http.StatusOK)
	}
}
EOF

        cat > .gitignore << 'EOF'
# Binaries for programs and plugins
*.exe
*.exe~
*.dll
*.so
*.dylib

# Test binary, built with \`go test -c\`
*.test

# Output of the go coverage tool
*.out

# Dependency directories
vendor/

# Go workspace file
go.work

# IDE
.vscode/
.idea/

# OS
.DS_Store
Thumbs.db
EOF
        ;;

    java)
        # Java setup
        mkdir -p src/main/java/com/example/sparc
        mkdir -p src/test/java/com/example/sparc
        mkdir -p src/main/resources

        cat > pom.xml << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.example</groupId>
    <artifactId>$PROJECT_NAME</artifactId>
    <version>1.0.0</version>

    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
            <version>2.7.0</version>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-test</artifactId>
            <version>2.7.0</version>
            <scope>test</scope>
        </dependency>
    </dependencies>
</project>
EOF

        cat > src/main/java/com/example/sparc/Application.java << 'EOF'
package com.example.sparc;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
@RestController
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

    @GetMapping("/")
    public String welcome() {
        return "Welcome to SPARC project";
    }
}
EOF

        cat > src/test/java/com/example/sparc/ApplicationTest.java << 'EOF'
package com.example.sparc;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
public class ApplicationTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    public void testWelcome() throws Exception {
        mockMvc.perform(get("/"))
               .andExpect(status().isOk())
               .andExpect(content().string("Welcome to SPARC project"));
    }
}
EOF

        cat > .gitignore << 'EOF'
# Compiled class file
*.class

# Log file
*.log

# BlueJ files
*.ctxt

# Mobile Tools for Java (J2ME)
.mtj.tmp/

# Package Files #
*.jar
*.war
*.nar
*.ear
*.zip
*.tar.gz
*.rar

# virtual machine crash logs, see http://www.java.com/en/download/help/log.html
hs_err_pid*
replay_pid*

# IDE
.idea/
.vscode/

# OS
.DS_Store
Thumbs.db
EOF
        ;;

    *)
        print_error "Unsupported language: $LANGUAGE"
        print_status "Supported languages: python, javascript, go, java"
        exit 1
        ;;
esac

# Copy SPARC workflow script
print_status "Copying SPARC workflow script..."
cp ../scripts/sparc_workflow.py scripts/

# Create initial specification document
print_status "Creating initial specification document..."
cat > docs/specification/requirements.md << 'EOF'
# 📋 Project Requirements Specification

## Project Overview
[Describe the project and its purpose]

## User Stories

### Primary User Stories
- [ ] As a [user type], I want [goal] so that [benefit]
- [ ] As a [user type], I want [goal] so that [benefit]

### Secondary User Stories
- [ ] As a [user type], I want [goal] so that [benefit]

## Acceptance Criteria

### Must-Have (Critical)
- [ ] Acceptance criteria 1
- [ ] Acceptance criteria 2

### Should-Have (Important)
- [ ] Acceptance criteria 3

### Nice-to-Have (Optional)
- [ ] Acceptance criteria 4

## Data Models

### Primary Entities
[Define your main entities and their relationships]

## API Endpoints

### REST Endpoints
- [ ] METHOD /endpoint - Description
- [ ] METHOD /endpoint - Description

## Success Metrics
[Define how to measure success]

## Constraints

### Technical Constraints
- [ ] Technology stack restrictions
- [ ] Performance requirements
EOF

# Create initial pseudocode document
print_status "Creating initial pseudocode document..."
cat > docs/pseudocode/algorithms.md << 'EOF'
# 🔧 Algorithm Descriptions

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

    RETURN result
```

## Data Flow

### [Process Name] Flow
```
Start
├─ Step 1: [Description]
├─ Step 2: [Description]
└─ End: [Result]
```

## Error Handling Patterns

### Pattern 1: [Error Type]
```
TRY:
    # Risky operation
CATCH [Error Type]:
    # Recovery strategy
```
EOF

# Create initial architecture document
print_status "Creating initial architecture document..."
cat > docs/architecture/design.md << 'EOF'
# 🏗️ System Architecture

## System Overview
[Describe the overall system architecture]

## System Layers

### Presentation Layer
- [ ] API endpoints
- [ ] Request/response models

### Business Logic Layer
- [ ] Service classes
- [ ] Business rules

### Data Access Layer
- [ ] Repository pattern
- [ ] Database connections

### Infrastructure Layer
- [ ] External services
- [ ] Configuration

## Database Schema

### Tables
- [ ] table_name: [description of columns and relationships]

## API Design

### REST Endpoints
```yaml
- method: GET
  path: /endpoint
  description: Description
  request: {schema}
  response: {schema}
```

## Security Considerations

### Authentication
- [ ] Method: [JWT/OAuth/API Key]

### Authorization
- [ ] Role-based access control
```

# Initialize git repository
print_status "Initializing git repository..."
git init
git add .
git commit -m "Initial SPARC project setup"

print_success "SPARC project setup complete!"
print_success "Project created at: $(pwd)"
print_status ""
print_status "Next steps:"
print_status "1. Review and complete docs/specification/requirements.md"
print_status "2. Follow SPARC workflow:"
print_status "   - Specification: Define requirements"
print_status "   - Pseudocode: Design algorithms"
print_status "   - Architecture: Design system structure"
print_status "   - Refinement: Implement with tests"
print_status "   - Completion: Integration and deployment"
print_status "3. Run: python scripts/sparc_workflow.py --help"
print_status "4. Start development!"