# erlmcp Enterprise Integrations Makefile
# Build and management for the enterprise integration suite

.PHONY: all compile eunit ct clean distclean test docs xref dialyzer

# Default target
all: compile test

# Compile the application
compile:
	@rebar3 compile

# Run EUnit tests
eunit:
	@rebar3 eunit

# Run Common Test suites
ct:
	@rebar3 ct

# Run all tests
test: compile eunit ct

# Build documentation
docs:
	@rebar3 docs

# Run Xref analysis
xref:
	@rebar3 xref

# Run Dialyzer
dialyzer:
	@rebar3 dialyzer

# Clean build artifacts
clean:
	@rebar3 clean

# Clean everything including deps
distclean: clean
	@rm -rf _build rebar.lock

# Generate release
release: compile
	@rebar3 release

# Install in development mode
install: compile
	@rebar3 install

# Check application dependencies
deps:
	@rebar3 deps

# Test specific adapter
test-adapter:
	@rebar3 eunit --module=erlmcp_identity_adapter
	@rebar3 eunit --module=erlmcp_monitoring_adapter
	@rebar3 eunit --module=erlmcp_logging_adapter
	@rebar3 eunit --module=erlmcp_bi_adapter

# Integration tests
integration-test:
	@rebar3 ct --suite=enterprise_integrations_SUITE

# Performance tests
performance-test:
	@./scripts/performance_test.sh

# Load testing
load-test:
	@./scripts/load_test.sh

# Stress testing
stress-test:
	@./scripts/stress_test.sh

# Coverage report
coverage:
	@rebar3 cover

# Check application version
version:
	@cat VERSION

# Update dependencies
update-deps:
	@rebar3 upgrade

# Format code
format:
	@rebar3 fmt

# Check code formatting
check-format:
	@rebar3 fmt --check

# Lint code
lint:
	@rebar3 lint

# Security scan
security-scan:
	@./scripts/security_scan.sh

# Generate API documentation
api-docs:
	@./scripts/generate_api_docs.sh

# Run all checks
check: lint xref dialyzer test

# Deploy to staging
deploy-staging:
	@./scripts/deploy.sh staging

# Deploy to production
deploy-prod:
	@./scripts/deploy.sh production

# Start application in development mode
dev:
	@rebar3 shell

# Run in production mode
prod:
	@./_build/default/bin/erlmcp_enterprise_integrations console

# Monitor application
monitor:
	@./scripts/monitor.sh

# Health check
health:
	@curl -f http://localhost:8080/api/integrations/health || exit 1

# Log level
log-level:
	@if [ -n "$(LEVEL)" ]; then \
		erl -pa _build/default/lib/*/ebin \
			-eval "application:set_env(erlmcp_enterprise_integrations, log_level, $(LEVEL))" \
			-eval "init:stop()"; \
	else \
		echo "Usage: make log-level LEVEL=debug"; \
	fi

# Configuration validation
validate-config:
	@./scripts/validate_config.sh

# Backup configuration
backup-config:
	@cp config/enterprise_integration.config config/enterprise_integration.config.backup
	@echo "Configuration backed up"

# Restore configuration
restore-config:
	@cp config/enterprise_integration.config.backup config/enterprise_integration.config
	@echo "Configuration restored"

# Generate test reports
test-report:
	@rebar3 cover --verbose

# Benchmark
benchmark:
	@./scripts/benchmark.sh

# Profile
profile:
	@./scripts/profile.sh

# Clean old releases
clean-old-releases:
	@find releases -type d -name "*-*" -not -name "*$(shell cat VERSION)" -exec rm -rf {} +

# Update application version
bump-version:
	@if [ -n "$(NEW_VERSION)" ]; then \
		echo "$(NEW_VERSION)" > VERSION; \
		sed -i 's/{version, .*/{version, "$(NEW_VERSION)"},/' src/erlmcp_enterprise_integrations.app.src; \
	else \
		echo "Usage: make bump-version NEW_VERSION=1.2.3"; \
	fi

# Create git tag
create-tag:
	@if [ -n "$(TAG)" ]; then \
		git add VERSION src/erlmcp_enterprise_integrations.app.src; \
		git commit -m "Bump version to $(TAG)"; \
		git tag -a v$(TAG) -m "Version $(TAG)"; \
	else \
		echo "Usage: make create-tag TAG=1.2.3"; \
	fi

# Release checklist
release-checklist:
	@echo "=== Enterprise Integration Release Checklist ==="
	@echo "1. Run all tests: make test"
	@echo "2. Run linting: make lint"
	@echo "3. Run dialyzer: make dialyzer"
	@echo "4. Update version: make bump-version NEW_VERSION=x.y.z"
	@echo "5. Create tag: make create-tag TAG=x.y.z"
	@echo "6. Build release: make release"
	@echo "7. Test release: ./_build/default/bin/erlmcp_enterprise_integrations console"
	@echo "8. Deploy: make deploy-staging"
	@echo "9. Monitor: make monitor"
	@echo "10. Tag production: make deploy-prod"

# Development helpers
dev-help:
	@echo "=== Development Helpers ==="
	@echo "make dev         - Start in development mode"
	@echo "make test        - Run all tests"
	@echo "make test-adapter - Test specific adapters"
	@echo "make lint        - Lint code"
	@echo "make format      - Format code"
	@echo "make check       - Run all checks"

# Monitoring helpers
monitor-help:
	@echo "=== Monitoring Helpers ==="
	@echo "make health      - Check application health"
	@echo "make log-level   - Set log level (debug/info/warning/error)"
	@echo "make monitor     - Monitor application"

# Deployment helpers
deploy-help:
	@echo "=== Deployment Helpers ==="
	@echo "make deploy-staging - Deploy to staging"
	@echo "make deploy-prod   - Deploy to production"
	@echo "make backup-config - Backup configuration"
	@echo "make restore-config - Restore configuration"