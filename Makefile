.PHONY: all compile test ct eunit lint dialyze clean distclean release help \
        setup direnv asdf deps deps-tree quality info coverage taiea-compile taiea-test \
        release-dev release-prod tar show-release \
        workspace-build workspace-test workspace-lint workspace-check workspace-clean workspace-release \
        check build

SHELL := /bin/bash

# Colors for output
BLUE := \033[0;34m
GREEN := \033[0;32m
RED := \033[0;31m
NC := \033[0m # No Color

# Default target
all: compile test lint

# ============================================================================
# BUILD TARGETS
# ============================================================================

compile:
	@echo "$(BLUE)Compiling workspace...$(NC)"
	rebar3 compile

# ============================================================================
# TEST TARGETS
# ============================================================================

test: eunit ct
	@echo "$(GREEN)All tests passed!$(NC)"

eunit:
	@echo "$(BLUE)Running EUnit tests...$(NC)"
	rebar3 eunit

ct:
	@echo "$(BLUE)Running Common Test suite...$(NC)"
	rebar3 ct

ct-verbose:
	@echo "$(BLUE)Running Common Test (verbose)...$(NC)"
	rebar3 ct -v

coverage:
	@echo "$(BLUE)Generating coverage report...$(NC)"
	rebar3 cover
	@echo "$(GREEN)Coverage report: _build/test/cover/index.html$(NC)"

# ============================================================================
# QUALITY TARGETS
# ============================================================================

lint:
	@echo "$(BLUE)Running linting...$(NC)"
	rebar3 lint

dialyze:
	@echo "$(BLUE)Running Dialyzer (type checker)...$(NC)"
	rebar3 dialyze

quality: lint dialyze
	@echo "$(GREEN)Quality checks passed!$(NC)"

# ============================================================================
# DOCUMENTATION
# ============================================================================

docs:
	@echo "$(BLUE)Generating documentation...$(NC)"
	rebar3 edoc
	@echo "$(GREEN)Documentation: doc/$(NC)"

# ============================================================================
# PROJECT-SPECIFIC TARGETS
# ============================================================================

taiea-test:
	@echo "$(BLUE)Testing TAIEA project...$(NC)"
	rebar3 -p taiea eunit
	rebar3 -p taiea ct

taiea-compile:
	@echo "$(BLUE)Compiling TAIEA...$(NC)"
	rebar3 -p taiea compile

# ============================================================================
# RELEASE TARGETS
# ============================================================================

release-dev:
	@echo "$(BLUE)Building development release...$(NC)"
	rebar3 release

release-prod:
	@echo "$(BLUE)Building production release...$(NC)"
	rebar3 as prod release

tar:
	@echo "$(BLUE)Creating release tarball...$(NC)"
	rebar3 as prod tar

show-release:
	@echo "$(GREEN)Release artifacts:$(NC)"
	@ls -lh _build/prod/rel/erlmcp/ 2>/dev/null || echo "No prod release found"
	@ls -lh _build/prod/*.tar.gz 2>/dev/null || echo "No tarball found"

# ============================================================================
# DEPENDENCY MANAGEMENT
# ============================================================================

deps:
	@echo "$(BLUE)Fetching dependencies...$(NC)"
	rebar3 get-deps

deps-tree:
	@echo "$(BLUE)Dependency tree:$(NC)"
	rebar3 tree

# ============================================================================
# ENVIRONMENT & SETUP
# ============================================================================

direnv:
	@echo "$(BLUE)Loading direnv...$(NC)"
	direnv allow

asdf:
	@echo "$(BLUE)Installing Erlang/Elixir via asdf...$(NC)"
	asdf install
	@echo "$(GREEN)Done! Use: direnv allow$(NC)"

setup: deps asdf direnv
	@echo "$(GREEN)Workspace setup complete!$(NC)"

# ============================================================================
# CLEANUP TARGETS
# ============================================================================

clean:
	@echo "$(BLUE)Cleaning build artifacts...$(NC)"
	rebar3 clean

distclean: clean
	@echo "$(BLUE)Removing all generated files...$(NC)"
	rm -rf _build/ doc/ ct_logs/ cover/ .rebar3/
	@echo "$(GREEN)Workspace clean!$(NC)"

# ============================================================================
# HELP & INFO
# ============================================================================

info:
	@echo "$(BLUE)Workspace Information:$(NC)"
	@echo "  Location: /Users/sac/erlmcp"
	@echo "  Erlang: $$(erl -noshell -eval 'erlang:display(erlang:system_info(otp_release))' -s init stop 2>/dev/null || echo 'unknown')"
	@echo "  Projects: taiea, vendor/erlmcp"
	@echo ""
	@echo "$(BLUE)Commands:$(NC)"
	@make help | grep -E '^\s+(make [a-z-]+|rebar3)'

help:
	@echo "$(BLUE)erlmcp Workspace - Build Targets$(NC)"
	@echo ""
	@echo "$(GREEN)Primary Targets:$(NC)"
	@echo "  make all              Compile + test + lint (default)"
	@echo "  make compile          Compile workspace"
	@echo "  make test             Run all tests (eunit + ct)"
	@echo "  make quality          Lint + Dialyzer"
	@echo ""
	@echo "$(GREEN)Testing:$(NC)"
	@echo "  make eunit            Run EUnit tests"
	@echo "  make ct               Run Common Test suite"
	@echo "  make ct-verbose       Run CT with verbose output"
	@echo "  make coverage         Generate coverage report"
	@echo ""
	@echo "$(GREEN)Code Quality:$(NC)"
	@echo "  make lint             Run linting"
	@echo "  make dialyze          Run Dialyzer (type checker)"
	@echo ""
	@echo "$(GREEN)Documentation:$(NC)"
	@echo "  make docs             Generate edoc documentation"
	@echo ""
	@echo "$(GREEN)Release:$(NC)"
	@echo "  make release-dev      Development release"
	@echo "  make release-prod     Production release"
	@echo "  make tar              Create tarball"
	@echo "  make show-release     Show release artifacts"
	@echo ""
	@echo "$(GREEN)Project Targets:$(NC)"
	@echo "  make taiea-compile    Compile TAIEA only"
	@echo "  make taiea-test       Test TAIEA only"
	@echo ""
	@echo "$(GREEN)Setup:$(NC)"
	@echo "  make setup            Complete workspace setup"
	@echo "  make direnv           Load direnv"
	@echo "  make asdf             Install Erlang/Elixir"
	@echo "  make deps             Fetch dependencies"
	@echo "  make deps-tree        Show dependency tree"
	@echo ""
	@echo "$(GREEN)Cleanup:$(NC)"
	@echo "  make clean            Remove build artifacts"
	@echo "  make distclean        Remove all generated files"
	@echo ""
	@echo "$(GREEN)Info:$(NC)"
	@echo "  make info             Show workspace information"
	@echo "  make help             This message"
