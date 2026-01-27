#!/usr/bin/env groovy

/**
 * TCPS Production Pipeline for Jenkins
 * Toyota Code Production System - Lean Six Sigma Quality
 */

pipeline {
    agent any

    environment {
        ERLANG_VERSION = '27.1'
        REBAR3_VERSION = '3.23.0'
        TCPS_QUALITY_GATE_COVERAGE = '80'
        TCPS_QUALITY_GATE_PASS_RATE = '80'
        WORK_ORDER_ID = "WO-${new Date().format('yyyyMMdd-HHmmss')}-${env.GIT_COMMIT?.take(7)}"
        TAKT_TIME_START = "${System.currentTimeMillis() / 1000 as long}"
    }

    options {
        buildDiscarder(logRotator(numToKeepStr: '30'))
        timestamps()
        timeout(time: 60, unit: 'MINUTES')
        disableConcurrentBuilds()
    }

    stages {
        // ====================================================================
        // STAGE 1: Pull Signal - Create Work Order
        // ====================================================================
        stage('Pull Signal') {
            steps {
                script {
                    echo "📋 Creating TCPS Work Order: ${env.WORK_ORDER_ID}"

                    def workOrderTtl = """
@prefix tcps: <http://purl.org/tcps#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<urn:tcps:work-order:${env.WORK_ORDER_ID}> a tcps:WorkOrder ;
    tcps:orderID "${env.WORK_ORDER_ID}" ;
    tcps:priority "normal" ;
    tcps:createdAt "${new Date().format("yyyy-MM-dd'T'HH:mm:ss'Z'")}"^^xsd:dateTime ;
    tcps:assignedTo <urn:tcps:worker:jenkins> ;
    tcps:produces <urn:tcps:sku:erlmcp:${env.GIT_COMMIT?.take(7)}> ;
    tcps:status "in_progress" .
"""

                    writeFile file: 'work_order.ttl', text: workOrderTtl
                    archiveArtifacts artifacts: 'work_order.ttl', fingerprint: true
                }
            }
        }

        // ====================================================================
        // STAGE 2: SHACL Validation - Jidoka
        // ====================================================================
        stage('SHACL Validation') {
            steps {
                script {
                    echo "🔍 Running SHACL Validation (Jidoka)"

                    try {
                        sh '''
                            pip3 install --user rdflib pyshacl pytest
                            cd tests/shacl
                            python3 -m pytest test_tcps_validation.py -v --tb=short
                        '''

                        def receipt = """
{
  "stage": "shacl_validation",
  "work_order": "${env.WORK_ORDER_ID}",
  "timestamp": "${new Date().format("yyyy-MM-dd'T'HH:mm:ss'Z'")}",
  "passed": true,
  "operator": "jenkins"
}
"""
                        writeFile file: 'shacl_receipt.json', text: receipt

                    } catch (Exception e) {
                        echo "❌ SHACL Validation FAILED"
                        triggerAndon('shacl_violation', 'SHACL validation failed')
                        throw e
                    }
                }
            }
            post {
                always {
                    archiveArtifacts artifacts: 'shacl_receipt.json', allowEmptyArchive: true
                }
            }
        }

        // ====================================================================
        // STAGE 3: Compile
        // ====================================================================
        stage('Compile') {
            steps {
                script {
                    echo "🔨 Compiling erlmcp"

                    try {
                        sh 'rebar3 compile'

                        // Run Dialyzer
                        def dialyzerPassed = true
                        try {
                            sh 'rebar3 dialyzer'
                        } catch (Exception e) {
                            echo "⚠️ Dialyzer warnings detected"
                            dialyzerPassed = false
                        }

                        def receipt = """
{
  "stage": "compile",
  "work_order": "${env.WORK_ORDER_ID}",
  "timestamp": "${new Date().format("yyyy-MM-dd'T'HH:mm:ss'Z'")}",
  "compile_passed": true,
  "dialyzer_passed": ${dialyzerPassed},
  "operator": "rebar3"
}
"""
                        writeFile file: 'compile_receipt.json', text: receipt

                    } catch (Exception e) {
                        echo "❌ Compilation FAILED"
                        triggerAndon('compilation_error', 'Compilation failed')
                        throw e
                    }
                }
            }
            post {
                always {
                    archiveArtifacts artifacts: 'compile_receipt.json', allowEmptyArchive: true
                }
            }
        }

        // ====================================================================
        // STAGE 4: Test - Quality Gate
        // ====================================================================
        stage('Test') {
            steps {
                script {
                    echo "🧪 Running tests with quality gates"

                    try {
                        sh 'rebar3 as test eunit --verbose'
                        sh 'rebar3 cover --verbose'

                        // Extract coverage (simplified)
                        def coverageOutput = sh(
                            script: 'rebar3 cover | grep -oP "Total: \\K[0-9]+" || echo "0"',
                            returnStdout: true
                        ).trim().toInteger()

                        echo "📊 Coverage: ${coverageOutput}%"

                        def qualityGatePassed = coverageOutput >= env.TCPS_QUALITY_GATE_COVERAGE.toInteger()

                        if (!qualityGatePassed) {
                            echo "❌ Quality gate FAILED: Coverage ${coverageOutput}% < ${env.TCPS_QUALITY_GATE_COVERAGE}%"
                            triggerAndon('test_failure', "Quality gate not met: ${coverageOutput}% coverage")
                            error("Quality gate failure")
                        }

                        def receipt = """
{
  "stage": "test",
  "work_order": "${env.WORK_ORDER_ID}",
  "timestamp": "${new Date().format("yyyy-MM-dd'T'HH:mm:ss'Z'")}",
  "tests_passed": true,
  "coverage_percent": ${coverageOutput},
  "quality_gate_passed": ${qualityGatePassed},
  "min_coverage_required": ${env.TCPS_QUALITY_GATE_COVERAGE}
}
"""
                        writeFile file: 'test_receipt.json', text: receipt

                    } catch (Exception e) {
                        echo "❌ Tests FAILED"
                        triggerAndon('test_failure', 'Tests failed')
                        throw e
                    }
                }
            }
            post {
                always {
                    archiveArtifacts artifacts: 'test_receipt.json,_build/test/cover/**', allowEmptyArchive: true
                    junit allowEmptyResults: true, testResults: '_build/test/logs/*/TEST-*.xml'
                }
            }
        }

        // ====================================================================
        // STAGE 5: Deterministic Build
        // ====================================================================
        stage('Deterministic Build') {
            steps {
                script {
                    echo "🔍 Verifying deterministic build"

                    try {
                        sh '''
                            # Build 1
                            rebar3 clean
                            rebar3 as prod compile
                            find _build/prod -name "*.beam" -type f -exec sha256sum {} \\; | sort > build1.sha256

                            # Build 2
                            rebar3 clean
                            rebar3 as prod compile
                            find _build/prod -name "*.beam" -type f -exec sha256sum {} \\; | sort > build2.sha256

                            # Compare
                            if ! diff build1.sha256 build2.sha256; then
                                echo "❌ Build is NOT deterministic"
                                exit 1
                            fi

                            echo "✅ Build is deterministic"
                        '''

                    } catch (Exception e) {
                        echo "❌ Non-deterministic build detected"
                        triggerAndon('non_deterministic_build', 'Build is not deterministic')
                        throw e
                    }
                }
            }
            post {
                always {
                    archiveArtifacts artifacts: 'build1.sha256,build2.sha256', allowEmptyArchive: true
                }
            }
        }

        // ====================================================================
        // STAGE 6: Release
        // ====================================================================
        stage('Release') {
            when {
                anyOf {
                    branch 'main'
                    branch pattern: 'release/.*', comparator: 'REGEXP'
                }
            }
            steps {
                script {
                    echo "📦 Creating production release"

                    sh 'rebar3 as prod tar'

                    def releaseTar = sh(
                        script: 'find _build/prod/rel -name "*.tar.gz" | head -1',
                        returnStdout: true
                    ).trim()

                    def releaseSha = sh(
                        script: "sha256sum ${releaseTar} | cut -d' ' -f1",
                        returnStdout: true
                    ).trim()

                    echo "✅ Release: ${releaseTar}"
                    echo "SHA256: ${releaseSha}"

                    def receipt = """
{
  "stage": "release",
  "work_order": "${env.WORK_ORDER_ID}",
  "timestamp": "${new Date().format("yyyy-MM-dd'T'HH:mm:ss'Z'")}",
  "release_file": "${releaseTar}",
  "sha256": "${releaseSha}"
}
"""
                    writeFile file: 'release_receipt.json', text: receipt
                }
            }
            post {
                always {
                    archiveArtifacts artifacts: '_build/prod/rel/*.tar.gz,release_receipt.json', allowEmptyArchive: true
                }
            }
        }
    }

    // ========================================================================
    // POST-BUILD: Kaizen Report
    // ========================================================================
    post {
        always {
            script {
                def taktTimeEnd = System.currentTimeMillis() / 1000 as long
                def taktTimeDuration = taktTimeEnd - env.TAKT_TIME_START.toLong()

                echo "📊 Generating Kaizen Report"
                echo "⏱️ Takt Time: ${taktTimeDuration} seconds"

                def kaizenReport = """
# TCPS Kaizen Report

**Work Order**: ${env.WORK_ORDER_ID}
**Build**: ${env.BUILD_NUMBER}
**Commit**: ${env.GIT_COMMIT}
**Branch**: ${env.GIT_BRANCH}
**Status**: ${currentBuild.result ?: 'SUCCESS'}

## Metrics

- **Takt Time**: ${taktTimeDuration} seconds
- **Quality Gates**: See receipts for details

## Receipts

${findFiles(glob: '*_receipt.json').collect { readFile(it.path) }.join('\\n\\n')}

## Continuous Improvement

- Monitor takt time trends over builds
- Analyze Andon triggers for root cause
- Optimize slow stages
- Update standard work procedures
"""

                writeFile file: 'kaizen_report.md', text: kaizenReport
                archiveArtifacts artifacts: 'kaizen_report.md,*.ttl', allowEmptyArchive: true
            }
        }

        success {
            echo "✅ TCPS Pipeline COMPLETED SUCCESSFULLY"
        }

        failure {
            echo "❌ TCPS Pipeline FAILED - Review Andon events"
        }
    }
}

/**
 * Helper function to trigger Andon (stop-the-line) events
 */
def triggerAndon(String type, String description) {
    def andonTtl = """
@prefix tcps: <http://purl.org/tcps#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<urn:tcps:andon:${System.currentTimeMillis()}> a tcps:AndonEvent ;
    tcps:severity "critical" ;
    tcps:type "${type}" ;
    tcps:triggeredAt "${new Date().format("yyyy-MM-dd'T'HH:mm:ss'Z'")}"^^xsd:dateTime ;
    tcps:description "${description}" ;
    tcps:affectsWorkOrder <urn:tcps:work-order:${env.WORK_ORDER_ID}> .
"""

    writeFile file: "andon_${type}.ttl", text: andonTtl
    echo "🚨 ANDON TRIGGERED: ${type}"
    echo "Description: ${description}"

    // Optional: Send notifications (Slack, email, etc.)
    // notifySlack(type, description)
}
