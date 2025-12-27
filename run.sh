#!/bin/bash
set -e

# Use system clang for proper macOS framework linking
export LEAN_CC=/usr/bin/clang

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Building demo..."
lake build demo

echo ""
echo "Running demo..."
.lake/build/bin/demo
