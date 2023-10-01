.PHONY: test fmt lint

all: test fmt lint

test:
	@echo "Running tests..."
	@cargo test --verbose

fmt:
	@echo "Checking formatting..."
	@cargo fmt --all

lint:
	@echo "Linting with clippy..."
	@cargo clippy --all-targets -- -D warnings
