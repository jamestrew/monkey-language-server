.PHONY: test fmt lint

all: test fmt lint build

test:
	@echo "Running tests..."
	@cargo test --verbose

fmt:
	@echo "Checking formatting..."
	@cargo fmt --all

lint:
	@echo "Linting with clippy..."
	@cargo clippy --all-targets -- -D warnings

build:
	@echo "Building release binary..."
	@cargo build --release
