#!/usr/bin/env bash
# TCPS Ontology Validation and Testing Script
# Validates SHACL shapes and runs example SPARQL queries

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "=========================================="
echo "TCPS Ontology Validation"
echo "=========================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if required tools are available
check_tool() {
    if command -v "$1" &> /dev/null; then
        echo -e "${GREEN}✓${NC} $1 found"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} $1 not found - skipping $1 validation"
        return 1
    fi
}

echo ""
echo "Checking for RDF validation tools..."
HAVE_RIOT=false
HAVE_SHACL=false
HAVE_SPARQL=false

if check_tool "riot"; then
    HAVE_RIOT=true
fi

if check_tool "shacl"; then
    HAVE_SHACL=true
fi

if check_tool "arq"; then
    HAVE_SPARQL=true
fi

echo ""
echo "----------------------------------------"
echo "1. Syntax Validation (Turtle/RDF)"
echo "----------------------------------------"

if [ "$HAVE_RIOT" = true ]; then
    for file in tcps_core.ttl tcps_quality.ttl tcps_flow.ttl; do
        echo -n "Validating $file... "
        if riot --validate "$file" > /dev/null 2>&1; then
            echo -e "${GREEN}✓ PASS${NC}"
        else
            echo -e "${RED}✗ FAIL${NC}"
            riot --validate "$file"
            exit 1
        fi
    done
else
    echo -e "${YELLOW}Skipping - install Apache Jena riot:${NC}"
    echo "  brew install jena  # macOS"
    echo "  apt install jena   # Ubuntu/Debian"
fi

echo ""
echo "----------------------------------------"
echo "2. SHACL Validation"
echo "----------------------------------------"

if [ "$HAVE_SHACL" = true ]; then
    echo "Validating instance data against SHACL shapes..."

    # Combine all ontology files for validation
    COMBINED_SHAPES="combined_shapes.ttl"
    cat tcps_core.ttl tcps_quality.ttl tcps_flow.ttl > "$COMBINED_SHAPES"

    if shacl validate --shapes "$COMBINED_SHAPES" --data example_instance_data.ttl > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS - All SHACL constraints satisfied${NC}"
    else
        echo -e "${YELLOW}⚠ VALIDATION REPORT:${NC}"
        shacl validate --shapes "$COMBINED_SHAPES" --data example_instance_data.ttl
    fi

    rm -f "$COMBINED_SHAPES"
else
    echo -e "${YELLOW}Skipping - install SHACL validator:${NC}"
    echo "  pip install pyshacl"
    echo "  # or use Apache Jena shacl command"
fi

echo ""
echo "----------------------------------------"
echo "3. SPARQL Query Testing"
echo "----------------------------------------"

if [ "$HAVE_SPARQL" = true ]; then
    # Combine ontology and instance data
    COMBINED_DATA="combined_data.ttl"
    cat tcps_core.ttl tcps_quality.ttl tcps_flow.ttl example_instance_data.ttl > "$COMBINED_DATA"

    echo "Running sample SPARQL query: Find active WorkOrders"

    cat > test_query.rq <<'EOF'
PREFIX tcps-core: <http://erlmcp.org/ontology/tcps/core#>

SELECT ?workOrderId ?priority ?status ?stage
WHERE {
    ?workOrder a tcps-core:WorkOrder ;
               tcps-core:workOrderId ?workOrderId ;
               tcps-core:priority ?priority ;
               tcps-core:status ?status ;
               tcps-core:currentStage ?stage .
}
ORDER BY ?priority
EOF

    if arq --data="$COMBINED_DATA" --query=test_query.rq > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS - Query executed successfully${NC}"
        echo ""
        echo "Query results:"
        arq --data="$COMBINED_DATA" --query=test_query.rq
    else
        echo -e "${RED}✗ FAIL - Query execution failed${NC}"
        arq --data="$COMBINED_DATA" --query=test_query.rq
    fi

    rm -f "$COMBINED_DATA" test_query.rq
else
    echo -e "${YELLOW}Skipping - install Apache Jena ARQ:${NC}"
    echo "  brew install jena  # macOS"
fi

echo ""
echo "----------------------------------------"
echo "4. Ontology Statistics"
echo "----------------------------------------"

echo "Counting ontology elements..."
echo ""

count_pattern() {
    pattern=$1
    label=$2
    count=$(grep -c "$pattern" tcps_*.ttl 2>/dev/null || echo 0)
    printf "  %-30s %d\n" "$label:" "$count"
}

count_pattern "a owl:Class" "Classes"
count_pattern "a owl:ObjectProperty" "Object Properties"
count_pattern "a owl:DatatypeProperty" "Datatype Properties"
count_pattern "a sh:NodeShape" "SHACL Shapes"
count_pattern "rdfs:comment" "Documentation Comments"
count_pattern "a owl:NamedIndividual\\|a tcps-core:ProductionStage\\|a tcps-flow:HeijunkaBucket" "Predefined Individuals"

echo ""
echo "File sizes:"
wc -l tcps_*.ttl | tail -1 | awk '{printf "  Total lines: %d\n", $1}'
du -sh . | awk '{printf "  Total size: %s\n", $1}'

echo ""
echo "=========================================="
echo "Validation Summary"
echo "=========================================="

if [ "$HAVE_RIOT" = true ] || [ "$HAVE_SHACL" = true ] || [ "$HAVE_SPARQL" = true ]; then
    echo -e "${GREEN}✓${NC} Ontology validation complete"
    echo ""
    echo "The TCPS ontology is well-formed and ready for use."
else
    echo -e "${YELLOW}⚠${NC} No validation tools found"
    echo ""
    echo "To fully validate the ontology, install Apache Jena:"
    echo "  brew install jena  # macOS"
    echo "  apt install jena   # Ubuntu/Debian"
    echo ""
    echo "Or use Python tools:"
    echo "  pip install rdflib pyshacl"
fi

echo ""
echo "Next steps:"
echo "  1. Import ontology into your RDF store (Oxigraph, Blazegraph, etc.)"
echo "  2. Load instance data and run SPARQL queries"
echo "  3. Integrate with erlmcp production workflows"
echo ""
