#!/usr/bin/env python3
"""
TCPS Ontology Usage Example

Demonstrates how to:
1. Load TCPS ontology into RDFLib
2. Create instance data programmatically
3. Run SPARQL queries
4. Validate data with SHACL
"""

from rdflib import Graph, Namespace, Literal, URIRef
from rdflib.namespace import RDF, RDFS, XSD
from datetime import datetime
import json


# Define namespaces
TCPS_CORE = Namespace("http://erlmcp.org/ontology/tcps/core#")
TCPS_QUALITY = Namespace("http://erlmcp.org/ontology/tcps/quality#")
TCPS_FLOW = Namespace("http://erlmcp.org/ontology/tcps/flow#")
EX = Namespace("http://example.org/erlmcp/production#")


def load_ontology():
    """Load all TCPS ontology files into a single graph."""
    g = Graph()

    print("Loading TCPS ontology...")
    g.parse("tcps_core.ttl", format="turtle")
    g.parse("tcps_quality.ttl", format="turtle")
    g.parse("tcps_flow.ttl", format="turtle")

    print(f"✓ Loaded {len(g)} triples")
    return g


def create_work_order(g, wo_id, sku_uri, quantity, priority):
    """Create a new WorkOrder instance."""
    wo_uri = EX[f"wo-{wo_id}"]

    g.add((wo_uri, RDF.type, TCPS_CORE.WorkOrder))
    g.add((wo_uri, TCPS_CORE.workOrderId, Literal(f"WO-{wo_id}")))
    g.add((wo_uri, TCPS_CORE.produceSKU, sku_uri))
    g.add((wo_uri, TCPS_CORE.quantity, Literal(quantity, datatype=XSD.positiveInteger)))
    g.add((wo_uri, TCPS_CORE.priority, Literal(priority)))
    g.add((wo_uri, TCPS_CORE.status, Literal("PENDING")))
    g.add((wo_uri, TCPS_CORE.createdAt, Literal(datetime.now(), datatype=XSD.dateTime)))

    print(f"✓ Created WorkOrder: WO-{wo_id}")
    return wo_uri


def create_receipt(g, receipt_id, wo_uri, stage_uri, quality_status, metrics):
    """Create a Receipt (evidence of completion)."""
    receipt_uri = EX[f"receipt-{receipt_id}"]

    g.add((receipt_uri, RDF.type, TCPS_CORE.Receipt))
    g.add((receipt_uri, TCPS_CORE.receiptId, Literal(receipt_id)))
    g.add((receipt_uri, TCPS_CORE.forWorkOrder, wo_uri))
    g.add((receipt_uri, TCPS_CORE.atStage, stage_uri))
    g.add((receipt_uri, TCPS_CORE.completedAt, Literal(datetime.now(), datatype=XSD.dateTime)))
    g.add((receipt_uri, TCPS_CORE.qualityStatus, Literal(quality_status)))
    g.add((receipt_uri, TCPS_CORE.metrics, Literal(json.dumps(metrics))))

    print(f"✓ Created Receipt: {receipt_id}")
    return receipt_uri


def create_andon_event(g, andon_id, severity, wo_uri, stage_uri, reason):
    """Create an Andon quality alert."""
    andon_uri = EX[f"andon-{andon_id}"]

    g.add((andon_uri, RDF.type, TCPS_CORE.AndonEvent))
    g.add((andon_uri, TCPS_CORE.andonId, Literal(andon_id)))
    g.add((andon_uri, TCPS_CORE.severity, Literal(severity)))
    g.add((andon_uri, TCPS_CORE.triggeredAt, Literal(datetime.now(), datatype=XSD.dateTime)))
    g.add((andon_uri, TCPS_CORE.affectsWorkOrder, wo_uri))
    g.add((andon_uri, TCPS_CORE.affectsStage, stage_uri))
    g.add((andon_uri, TCPS_CORE.reason, Literal(reason)))
    g.add((andon_uri, TCPS_CORE.isResolved, Literal(False, datatype=XSD.boolean)))

    print(f"✓ Created Andon Event: {andon_id} (severity: {severity})")
    return andon_uri


def query_active_work_orders(g):
    """Query for all active WorkOrders."""
    query = """
        PREFIX tcps-core: <http://erlmcp.org/ontology/tcps/core#>

        SELECT ?workOrderId ?priority ?status ?stage
        WHERE {
            ?wo a tcps-core:WorkOrder ;
                tcps-core:workOrderId ?workOrderId ;
                tcps-core:priority ?priority ;
                tcps-core:status ?status .
            OPTIONAL { ?wo tcps-core:currentStage ?stage }
        }
        ORDER BY ?priority
    """

    print("\nQuerying active WorkOrders...")
    results = g.query(query)

    for row in results:
        print(f"  - {row.workOrderId}: {row.status} (priority: {row.priority})")

    return results


def query_quality_metrics(g, wo_uri):
    """Query quality metrics for a specific WorkOrder."""
    query = """
        PREFIX tcps-quality: <http://erlmcp.org/ontology/tcps/quality#>

        SELECT ?metricName ?category ?measuredValue ?targetValue ?meetsThreshold
        WHERE {
            ?metric a tcps-quality:QualityMetric ;
                    tcps-quality:metricName ?metricName ;
                    tcps-quality:metricCategory ?category ;
                    tcps-quality:measuredValue ?measuredValue ;
                    tcps-quality:targetValue ?targetValue ;
                    tcps-quality:meetsThreshold ?meetsThreshold ;
                    tcps-quality:forWorkOrder <%s> .
        }
    """ % wo_uri

    print(f"\nQuerying quality metrics for {wo_uri}...")
    results = g.query(query)

    for row in results:
        status = "✓ PASS" if row.meetsThreshold else "✗ FAIL"
        print(f"  {status} {row.metricName}: {row.measuredValue}/{row.targetValue}")

    return results


def query_andon_events(g):
    """Query unresolved Andon events."""
    query = """
        PREFIX tcps-core: <http://erlmcp.org/ontology/tcps/core#>

        SELECT ?andonId ?severity ?reason ?isResolved
        WHERE {
            ?andon a tcps-core:AndonEvent ;
                   tcps-core:andonId ?andonId ;
                   tcps-core:severity ?severity ;
                   tcps-core:reason ?reason ;
                   tcps-core:isResolved ?isResolved .
            FILTER(?isResolved = false)
        }
        ORDER BY ?severity
    """

    print("\nQuerying unresolved Andon events...")
    results = g.query(query)

    for row in results:
        print(f"  [!] {row.andonId} ({row.severity}): {row.reason}")

    return results


def validate_with_shacl(g):
    """Validate graph against SHACL shapes (requires pyshacl)."""
    try:
        from pyshacl import validate

        # Load SHACL shapes
        shapes_graph = Graph()
        shapes_graph.parse("tcps_quality.ttl", format="turtle")
        shapes_graph.parse("tcps_flow.ttl", format="turtle")

        print("\nValidating with SHACL...")
        conforms, results_graph, results_text = validate(
            g,
            shacl_graph=shapes_graph,
            inference="rdfs",
            abort_on_first=False,
        )

        if conforms:
            print("✓ SHACL validation PASSED")
        else:
            print("✗ SHACL validation FAILED:")
            print(results_text)

        return conforms

    except ImportError:
        print("⚠ pyshacl not installed - skipping SHACL validation")
        print("  Install with: pip install pyshacl")
        return None


def export_to_file(g, filename):
    """Export graph to Turtle file."""
    g.serialize(destination=filename, format="turtle")
    print(f"\n✓ Exported graph to {filename}")


def main():
    """Main demonstration."""
    print("=" * 60)
    print("TCPS Ontology Usage Example")
    print("=" * 60)
    print()

    # Load ontology
    g = load_ontology()

    print("\n" + "-" * 60)
    print("Creating Instance Data")
    print("-" * 60)

    # Create SKU
    sku_uri = EX["erlmcp-server-v1.0.0"]
    g.add((sku_uri, RDF.type, TCPS_CORE.SKU))
    g.add((sku_uri, TCPS_CORE.skuIdentifier, Literal("erlmcp:mcp-server")))
    g.add((sku_uri, TCPS_CORE.skuVersion, Literal("1.0.0")))
    g.add((sku_uri, TCPS_CORE.inventoryLevel, Literal(5, datatype=XSD.nonNegativeInteger)))
    print("✓ Created SKU: erlmcp:mcp-server v1.0.0")

    # Create WorkOrder
    wo_uri = create_work_order(g, "2026-100", sku_uri, 3, "HIGH")

    # Create Receipt
    create_receipt(
        g,
        "receipt-100-test",
        wo_uri,
        TCPS_CORE.TestStage,
        "PASSED",
        {"coverage": 0.85, "tests_passed": 142, "tests_failed": 0},
    )

    # Create Quality Metric
    metric_uri = EX["metric-100-coverage"]
    g.add((metric_uri, RDF.type, TCPS_QUALITY.QualityMetric))
    g.add((metric_uri, TCPS_QUALITY.metricId, Literal("METRIC-100-COVERAGE")))
    g.add((metric_uri, TCPS_QUALITY.metricName, Literal("Test Coverage")))
    g.add((metric_uri, TCPS_QUALITY.metricCategory, Literal("CODE_COVERAGE")))
    g.add((metric_uri, TCPS_QUALITY.measuredValue, Literal(0.85, datatype=XSD.decimal)))
    g.add((metric_uri, TCPS_QUALITY.targetValue, Literal(0.80, datatype=XSD.decimal)))
    g.add((metric_uri, TCPS_QUALITY.meetsThreshold, Literal(True, datatype=XSD.boolean)))
    g.add((metric_uri, TCPS_QUALITY.forWorkOrder, wo_uri))
    g.add((metric_uri, TCPS_QUALITY.atStage, TCPS_CORE.TestStage))
    print("✓ Created Quality Metric: Test Coverage")

    # Create Andon Event
    create_andon_event(
        g,
        "ANDON-2026-050",
        "MEDIUM",
        wo_uri,
        TCPS_CORE.TestStage,
        "Performance test degradation detected",
    )

    print("\n" + "-" * 60)
    print("Running SPARQL Queries")
    print("-" * 60)

    # Run queries
    query_active_work_orders(g)
    query_quality_metrics(g, wo_uri)
    query_andon_events(g)

    print("\n" + "-" * 60)
    print("Validation")
    print("-" * 60)

    # Validate with SHACL
    validate_with_shacl(g)

    # Export graph
    export_to_file(g, "generated_instance_data.ttl")

    print("\n" + "=" * 60)
    print("Example Complete!")
    print("=" * 60)
    print(f"\nGenerated graph contains {len(g)} triples")
    print("\nNext steps:")
    print("  1. Load into RDF store: oxigraph, blazegraph, stardog")
    print("  2. Query via SPARQL endpoint")
    print("  3. Integrate with erlmcp production workflows")
    print()


if __name__ == "__main__":
    main()
