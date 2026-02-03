@Library('erlmcp-pipeline-library@master') _

pipeline {
    agent {
        label 'erlmcp-builder'
    }

    environment {
        GIT_REPO = 'https://github.com/${GIT_ORG}/erlmcp.git'
        REGISTRY = 'ghcr.io/${GIT_ORG}/erlmcp'
        OTP_VERSION = '28.3.1'
        FEATURE_BRANCH = env.BRANCH_NAME
        DEPLOYMENT_ENV = 'dev' // Default to dev, can be overridden
    }

    stages {
        // Stage 1: Feature Branch Checkout
        stage('Checkout Feature Branch') {
            steps {
                git url: env.GIT_REPO, branch: env.FEATURE_BRANCH
            }
        }

        // Stage 2: Feature Validation
        stage('Validate Feature') {
            steps {
                script {
                    // Check feature flag configuration
                    def featureFlagCheck = checkFeatureFlags()
                    if (!featureFlagCheck.valid) {
                        error("Feature flags not properly configured: ${featureFlagCheck.errors}")
                    }

                    // Check business rules
                    def businessRulesCheck = validateBusinessRules()
                    if (!businessRulesCheck.valid) {
                        error("Business rules validation failed: ${businessRulesCheck.errors}")
                    }
                }
            }
        }

        // Stage 3: Build Feature
        stage('Build Feature') {
            steps {
                withErlang(version: env.OTP_VERSION) {
                    sh 'rebar3 compile'
                }
            }
        }

        // Stage 4: Test Feature
        stage('Test Feature') {
            parallel {
                stage('Feature Unit Tests') {
                    steps {
                        withErlang(version: env.OTP_VERSION) {
                            sh 'rebar3 eunit'
                            junit allowEmptyResults: true, testResultsPattern: '**/eunit*.xml'
                        }
                    }
                }

                stage('Feature Integration Tests') {
                    steps {
                        withErlang(version: env.OTP_VERSION) {
                            sh 'rebar3 ct --suite=test/feature'
                            junit allowEmptyResults: true, testResultsPattern: '**/ct*.xml'
                        }
                    }
                }

                stage('Performance Tests') {
                    steps {
                        script {
                            def performanceReport = runFeaturePerformanceTests()
                            if (performanceReport.regression > 10) {
                                error("Performance regression ${performanceReport.regression}% > 10%")
                            }
                        }
                    }
                }
            }
        }

        // Stage 5: Deploy to Feature Environment
        stage('Deploy Feature') {
            steps {
                script {
                    def deploymentStrategy = 'feature-flag' // Default strategy

                    if (env.DEPLOYMENT_STRATEGY) {
                        deploymentStrategy = env.DEPLOYMENT_STRATEGY
                    }

                    // Deploy with feature flag
                    deployWithFeatureFlag(deploymentStrategy)
                }
            }
        }

        // Stage 6: Feature Validation
        stage('Validate Feature Deployment') {
            steps {
                script {
                    // Run feature-specific tests
                    runFeatureTests()

                    // Validate feature behavior
                    validateFeatureBehavior()

                    // Update feature flag status
                    updateFeatureFlagStatus('active')
                }
            }
        }

        // Stage 7: Feature Monitoring
        stage('Monitor Feature') {
            steps {
                script {
                    // Start monitoring
                    startFeatureMonitoring()

                    // Wait for user feedback
                    timeout(time: 48, unit: 'HOURS') {
                        input 'Proceed with feature promotion?'
                    }
                }
            }
        }

        // Stage 8: Feature Promotion or Rollback
        stage('Feature Decision') {
            parallel {
                stage('Promote to Production') {
                    when {
                        expression { env.PROMOTE_TO_PROD == 'true' }
                    }
                    steps {
                        script {
                            promoteToProduction()
                            updateFeatureFlagStatus('production')
                        }
                    }
                }

                stage('Rollback Feature') {
                    when {
                        expression { env.PROMOTE_TO_PROD != 'true' }
                    }
                    steps {
                        script {
                            rollbackFeature()
                            updateFeatureFlagStatus('inactive')
                        }
                    }
                }
            }
        }
    }

    post {
        always {
            archiveArtifacts artifacts: '**/*.log,**/*.xml,**/coverage/**,**/*.md', fingerprint: true
        }

        success {
            script {
                notifyFeatureDeployment('SUCCESS')
                cleanupFeatureDeployment()
            }
        }

        failure {
            script {
                notifyFeatureDeployment('FAILURE')
                cleanupFeatureDeployment()
            }
        }

        cleanup {
            cleanWs()
        }
    }
}

// Helper Functions for Feature Deployment
def checkFeatureFlags() {
    return sh(script: '''
        ./scripts/feature-flags/validate-branch.sh ${FEATURE_BRANCH}
    ''', returnStdout: true)
}

def validateBusinessRules() {
    return sh(script: '''
        ./scripts/business/validate-feature.sh ${FEATURE_BRANCH}
    ''', returnStdout: true)
}

def runFeaturePerformanceTests() {
    return sh(script: '''
        ./scripts/performance/feature-test.sh ${FEATURE_BRANCH}
        echo '{"regression": $(cat performance_report.json | jq .regression)}'
    ''', returnStdout: true)
}

def deployWithFeatureFlag(strategy) {
    def featureName = env.FEATURE_BRANCH.replace('/', '-').replace('_', '-')
    def deploymentId = UUID.randomUUID().toString()

    sh """
        ./scripts/deploy/deploy-feature.sh \
            --feature-name ${featureName} \
            --strategy ${strategy} \
            --environment ${DEPLOYMENT_ENV} \
            --deployment-id ${deploymentId}
    """
}

def runFeatureTests() {
    sh '''
        ./scripts/test/feature-validation.sh
    '''
}

def validateFeatureBehavior() {
    sh '''
        ./scripts/test/feature-behavior.sh
    '''
}

def promoteToProduction() {
    sh '''
        ./scripts/deploy/promote-feature.sh
    '''
}

def rollbackFeature() {
    sh '''
        ./scripts/deploy/rollback-feature.sh
    '''
}

def startFeatureMonitoring() {
    sh '''
        ./scripts/monitoring/feature-monitoring.sh start
    '''
}

def updateFeatureFlagStatus(status) {
    sh """
        ./scripts/feature-flags/update-status.sh \
            --feature-name ${FEATURE_BRANCH} \
            --status ${status}
    """
}

def notifyFeatureDeployment(status) {
    slackSend(
        channel: '#feature-deployments',
        color: status == 'SUCCESS' ? 'good' : 'danger',
        message: "Feature branch ${FEATURE_BRANCH} deployment ${status}\n${env.BUILD_URL}"
    )
}

def cleanupFeatureDeployment() {
    sh '''
        ./scripts/deploy/cleanup-feature.sh
    '''
}