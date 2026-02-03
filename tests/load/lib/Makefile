# Makefile for erlmcp Load Testing
# Docker-only execution per DOCKER-ONLY CONSTITUTION

.PHONY: all test clean help validate report

# Configuration
K6_IMAGE := grafana/k6:latest
BASE_URL := http://erlmcp:8080
REPORT_DIR := reports
DOCKER_NETWORK := erlmcp-network

# Default target
all: test

# Run all load tests
test: validate
	@echo "Running all load tests..."
	docker run --rm \
		--network $(DOCKER_NETWORK) \
		-e BASE_URL=$(BASE_URL) \
		-v $(PWD):/tests \
		-w /tests \
		$(K6_IMAGE) \
		run --summary-export=$(REPORT_DIR)/summary.json ramp-up-test.js

# Run ramp-up test
ramp-up:
	@echo "Running ramp-up test..."
	docker run --rm \
		--network $(DOCKER_NETWORK) \
		-e BASE_URL=$(BASE_URL) \
		-v $(PWD):/tests \
		-w /tests \
		$(K6_IMAGE) \
		run --out json=$(REPORT_DIR)/ramp-up.json ramp-up-test.js

# Run sustained load test
sustained:
	@echo "Running sustained load test..."
	docker run --rm \
		--network $(DOCKER_NETWORK) \
		-e BASE_URL=$(BASE_URL) \
		-v $(PWD):/tests \
		-w /tests \
		$(K6_IMAGE) \
		run --out json=$(REPORT_DIR)/sustained.json sustained-load-test.js

# Run spike test
spike:
	@echo "Running spike test..."
	docker run --rm \
		--network $(DOCKER_NETWORK) \
		-e BASE_URL=$(BASE_URL) \
		-v $(PWD):/tests \
		-w /tests \
		$(K6_IMAGE) \
		run --out json=$(REPORT_DIR)/spike.json spike-test.js

# Run failover test
failover:
	@echo "Running failover test..."
	docker run --rm \
		--network $(DOCKER_NETWORK) \
		-e BASE_URL=$(BASE_URL) \
		-v $(PWD):/tests \
		-w /tests \
		$(K6_IMAGE) \
		run --out json=$(REPORT_DIR)/failover.json failover-test.js

# Validate test scripts
validate:
	@echo "Validating test scripts..."
	docker run --rm \
		-v $(PWD):/tests \
		-w /tests \
		$(K6_IMAGE) \
		validate ramp-up-test.js sustained-load-test.js spike-test.js failover-test.js

# Generate HTML report
report:
	@echo "Generating HTML reports..."
	docker run --rm \
		-v $(PWD):/tests \
		-w /tests \
		$(K6_IMAGE) \
		run --out json=$(REPORT_DIR)/all.json ramp-up-test.js

# Clean report directory
clean:
	@echo "Cleaning reports..."
	rm -rf $(REPORT_DIR)/*.json $(REPORT_DIR)/*.html
	mkdir -p $(REPORT_DIR)

# Start monitoring stack (Prometheus + Grafana)
monitoring:
	@echo "Starting monitoring stack..."
	docker compose -f docker-compose.load.yml --profile load up -d

# Stop monitoring stack
monitoring-stop:
	@echo "Stopping monitoring stack..."
	docker compose -f docker-compose.load.yml --profile load down

# Health check
health:
	@echo "Checking system health..."
	docker run --rm \
		--network $(DOCKER_NETWORK) \
		curlimages/curl:latest \
		curl -f $(BASE_URL)/health || exit 1

# Help
help:
	@echo "Available targets:"
	@echo "  all          - Run all tests (default)"
	@echo "  ramp-up      - Run ramp-up test"
	@echo "  sustained    - Run sustained load test"
	@echo "  spike        - Run spike test"
	@echo "  failover     - Run failover test"
	@echo "  validate     - Validate test scripts"
	@echo "  report       - Generate HTML reports"
	@echo "  clean        - Clean report directory"
	@echo "  monitoring   - Start monitoring stack"
	@echo "  monitoring-stop - Stop monitoring stack"
	@echo "  health       - Check system health"
	@echo "  help         - Show this help"
