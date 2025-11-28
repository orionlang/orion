.PHONY: bootstrap self-hosted test clean help

help:
	@echo "Orion Compiler Build System"
	@echo ""
	@echo "Targets:"
	@echo "  bootstrap      - Build the bootstrap compiler (Zig)"
	@echo "  self-hosted    - Build the self-hosted compiler (Orion) using bootstrap"
	@echo "  test           - Run tests against bootstrap compiler"
	@echo "  all            - Build bootstrap (default)"
	@echo "  clean          - Clean build artifacts"

bootstrap:
	@echo "Building bootstrap compiler..."
	cd bootstrap && zig build

test:
	@echo "Running bootstrap compiler tests..."
	cd bootstrap && zig build test --summary all

self-hosted: bootstrap
	@echo "Building self-hosted compiler..."
	@echo "NOTE: Self-hosted compiler not yet implemented"

all: bootstrap

clean:
	@echo "Cleaning build artifacts..."
	rm -rf bootstrap/.zig-cache bootstrap/zig-out
	rm -rf .zig-cache zig-out
