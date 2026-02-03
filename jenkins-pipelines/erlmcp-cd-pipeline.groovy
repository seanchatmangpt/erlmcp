@Library('erlmcp-pipeline-library@master') _

pipeline {
    agent {
        label 'erlmcp-builder'
    }

    environment {
        GIT_REPO = 'https://github.com/${GIT_ORG}/erlmcp.git'
        REGISTRY = 'ghcr.io/${GIT_ORG}/erlmcp'
        OTP_VERSION = '28.3.1'
        ERLMCP_VERSION = '3.0.0'
        AWS_ACCOUNT_ID = credentials('aws-account-id')
        AWS_REGION = 'us-east-1'
        KUBE_CONFIG = credentials('kube-config-prod')
    }

    options {
        timeout(time: 60, unit: 'MINUTES')
        buildDiscarder(logRotator(numToKeepStr: '10'))
        disableConcurrentBuilds()
        retry(3)
    }

    stages {
        // Stage 1: Source Checkout and Code Quality
        stage('Source & Quality') {
            parallel {
                stage('Checkout') {
                    steps {
                        git url: env.GIT_REPO, branch: env.BRANCH_NAME
                    }
                }

                stage('Quality Gate') {
                    steps {
                        script {
                            def qualityReport = qualityGate()
                            if (qualityReport.score < 80) {
                                error("Quality score ${qualityReport.score} < 80")
                            }
                        }
                    }
                }
            }
        }

        // Stage 2: Build and Test
        stage('Build & Test') {
            parallel {
                stage('Compile') {
                    steps {
                        withErlang(version: env.OTP_VERSION) {
                            sh 'rebar3 compile'
                        }
                    }
                }

                stage('Unit Tests') {
                    steps {
                        withErlang(version: env.OTP_VERSION) {
                            sh 'rebar3 eunit'
                            junit allowEmptyResults: true, testResultsPattern: '**/eunit*.xml'
                        }
                    }
                }

                stage('Integration Tests') {
                    steps {
                        withErlang(version: env.OTP_VERSION) {
                            sh 'rebar3 ct --suite=test/integration'
                            junit allowEmptyResults: true, testResultsPattern: '**/ct*.xml'
                        }
                    }
                }

                stage('Property Tests') {
                    steps {
                        withErlang(version: env.OTP_VERSION) {
                            sh 'rebar3 proper -c'
                        }
                    }
                }

                stage('Static Analysis') {
                    steps {
                        withErlang(version: env.OTP_VERSION) {
                            sh 'rebar3 dialyzer'
                            sh 'rebar3 xref'
                        }
                    }
                }
            }
        }

        // Stage 3: Security Scanning
        stage('Security Scan') {
            steps {
                parallel {
                    stage('Container Scan') {
                        script {
                            buildDockerImages()
                            dockerScan()
                        }
                    }

                    stage('Dependency Scan') {
                        script {
                            dependencyScan()
                        }
                    }

                    stage('SAST Scan') {
                        script {
                            sastScan()
                        }
                    }
                }
            }
        }

        // Stage 4: Build Artifacts
        stage('Build Artifacts') {
            steps {
                script {
                    // Build release
                    withErlang(version: env.OTP_VERSION) {
                        sh 'rebar3 as prod release'
                    }

                    // Build Docker images
                    buildDockerImages()

                    // Package artifacts
                    packageArtifacts()
                }
            }
        }

        // Stage 5: Deploy to Environment
        stage('Deploy') {
            parallel {
                stage('Deploy to Dev') {
                    when {
                        expression { env.BRANCH_NAME == 'develop' }
                    }
                    steps {
                        script {
                            deployToEnvironment('dev')
                            smokeTest('dev')
                        }
                    }
                }

                stage('Deploy to Staging') {
                    when {
                        expression { env.BRANCH_NAME == 'main' || env.TAG_NAME != null }
                    }
                    steps {
                        script {
                            deployToEnvironment('staging')
                            runIntegrationTests('staging')
                        }
                    }
                }

                stage('Deploy to Prod') {
                    when {
                        expression { env.TAG_NAME != null }
                    }
                    steps {
                        script {
                            // Manual approval required for prod
                            timeout(time: 24, unit: 'HOURS') {
                                input 'Deploy to Production?'
                            }

                            deployToEnvironment('prod')
                            runProductionTests()
                        }
                    }
                }
            }
        }

        // Stage 6: Performance Validation
        stage('Performance') {
            steps {
                script {
                    performanceValidation()
                    benchmarkRelease()
                }
            }
        }

        // Stage 7: Compliance
        stage('Compliance') {
            steps {
                script {
                    runComplianceChecks()
                    generateComplianceReport()
                }
            }
        }
    }

    post {
        always {
            archiveArtifacts artifacts: '**/*.log,**/*.xml,**/coverage/**,**/*.tar.gz,**/*.md', fingerprint: true
            publishHTML([
                allowMissing: false,
                alwaysLinkToLastBuild: true,
                keepAll: true,
                reportDir: 'reports',
                reportFiles: '*.html',
                reportName: 'Erlang MCP Build Report'
            ])
        }

        success {
            script {
                // Update feature flags for successful deployment
                updateFeatureFlags()

                // Notify success
                notifySlack('SUCCESS')

                // Create receipt
                createDeploymentReceipt()
            }
        }

        failure {
            script {
                // Rollback deployment
                if (env.DEPLOYMENT_ID) {
                    rollbackDeployment(env.DEPLOYMENT_ID)
                }

                // Notify failure
                notifySlack('FAILURE')

                // Trigger incident response
                triggerIncidentResponse()
            }
        }

        unstable {
            script {
                notifySlack('UNSTABLE')
            }
        }

        cleanup {
            cleanWs()
        }
    }
}

// Helper Functions
def qualityGate() {
    return sh(script: '''
        ./scripts/quality/analyze.sh
        echo '{"score": $(cat quality_score.json | jq .score)}'
    ''', returnStdout: true)
}

def buildDockerImages() {
    def registryImage = "${env.REGISTRY}-registry:${env.BUILD_ID}"
    def serverImage = "${env.REGISTRY}-server:${env.BUILD_ID}"
    def transportImage = "${env.REGISTRY}-transports:${env.BUILD_ID}"

    sh """
        docker build -f docker/Dockerfile.registry -t ${registryImage} .
        docker build -f docker/Dockerfile.server -t ${serverImage} .
        docker build -f docker/Dockerfile.transports -t ${transportImage} .

        # Tag with version
        docker tag ${registryImage} ${env.REGISTRY}-registry:${env.ERLMCP_VERSION}
        docker tag ${serverImage} ${env.REGISTRY}-server:${env.ERLMCP_VERSION}
        docker tag ${transportImage} ${env.REGISTRY}-transports:${env.ERLMCP_VERSION}
    """
}

def dockerScan() {
    sh '''
        docker run --rm -v /var/run/docker.sock:/var/run/docker.sock \
            aquasec/trivy image --exit-code 1 --severity CRITICAL,HIGH \
            $(docker images | grep erlmcp | awk '{print $1":"$2}')
    '''
}

def dependencyScan() {
    sh '''
        ./scripts/security/dependency-scan.sh
    '''
}

def sastScan() {
    sh '''
        ./scripts/security/sast-scan.sh
    '''
}

def packageArtifacts() {
    sh '''
        mkdir -p artifacts

        # Create tarball
        tar -czf artifacts/erlmcp-${ERLMCP_VERSION}-${BUILD_ID}.tar.gz -C _build/prod/rel erlmcp

        # Create checksums
        sha256sum artifacts/*.tar.gz > artifacts/checksums.sha256
    '''
}

def deployToEnvironment(envName) {
    def deploymentId = UUID.randomUUID().toString()

    script {
        env.DEPLOYMENT_ID = deploymentId

        // Choose deployment strategy
        def strategy = envName == 'prod' ? 'blue-green' : 'rolling'

        sh """
            ./scripts/deploy/deploy-${envName}.sh \
                --strategy ${strategy} \
                --deployment-id ${deploymentId} \
                --image-version ${ERLMCP_VERSION}
        """

        // Update deployment status
        updateDeploymentStatus(deploymentId, 'deployed', envName)
    }
}

def smokeTest(envName) {
    def url = getEnvironmentUrl(envName)

    sh """
        ./scripts/test/smoke-test.sh ${url}
    """
}

def runIntegrationTests(envName) {
    def url = getEnvironmentUrl(envName)

    sh """
        ./scripts/test/integration-test.sh ${url}
    """
}

def runProductionTests() {
    sh '''
        ./scripts/test/prod-validation.sh
    '''
}

def performanceValidation() {
    sh '''
        ./scripts/performance/validate.sh
    '''
}

def benchmarkRelease() {
    sh '''
        ./scripts/benchmark/run-release-benchmark.sh
    '''
}

def runComplianceChecks() {
    sh '''
        ./scripts/compliance/check-all.sh
    '''
}

def generateComplianceReport() {
    sh '''
        ./scripts/compliance/generate-report.sh
    '''
}

def updateFeatureFlags() {
    sh '''
        ./scripts/feature-flags/update-prod-flags.sh
    '''
}

def rollbackDeployment(deploymentId) {
    sh """
        ./scripts/deploy/rollback.sh --deployment-id ${deploymentId}
    """
}

def notifySlack(status) {
    slackSend(
        channel: '#ci-cd',
        color: status == 'SUCCESS' ? 'good' : (status == 'FAILURE' ? 'danger' : 'warning'),
        message: "${env.JOB_NAME} - #${env.BUILD_NUMBER} - ${status}\n${env.BUILD_URL}"
    )
}

def createDeploymentReceipt() {
    sh '''
        ./scripts/deployment/create-receipt.sh
    '''
}

def triggerIncidentResponse() {
    sh '''
        ./scripts/incident/trigger.sh
    '''
}

def getEnvironmentUrl(envName) {
    return sh(script: "./scripts/deploy/get-url.sh ${envName}", returnStdout: true).trim()
}

def updateDeploymentStatus(deploymentId, status, envName) {
    sh """
        ./scripts/deployment/update-status.sh \
            --deployment-id ${deploymentId} \
            --status ${status} \
            --environment ${envName}
    """
}