#!/bin/bash
# run-tests.sh - Run Tootsville Lisp tests with timeout protection
#
# Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2024 The
# Corporation for Inter-World Tourism and Adventuring (CIWTA.org); © 2025
# Interworldly Adventuring, LLC
#
# This program is Free Software: you can redistribute it and/or
# modify it under the terms of the GNU Affero General Public License
# as published by the Free Software Foundation; either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program. If not, see
# <https://www.gnu.org/licenses/>.

set -e

# Default timeout in seconds (5 minutes)
DEFAULT_TIMEOUT=300

# Parse command line arguments
TIMEOUT=${1:-$DEFAULT_TIMEOUT}
VERBOSE=${2:-false}

echo "Tootsville Test Runner"
echo "====================="
echo "Timeout: ${TIMEOUT} seconds"
echo "Verbose: ${VERBOSE}"
echo ""

# Check if SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL (Steel Bank Common Lisp) is not installed or not in PATH"
    echo "Please install SBCL to run the tests"
    exit 1
fi

# Check if we're in the right directory
if [ ! -f "run-tests.lisp" ]; then
    echo "Error: run-tests.lisp not found in current directory"
    echo "Please run this script from the lib/tootsville.net directory"
    exit 1
fi

# Function to cleanup on exit
cleanup() {
    echo ""
    echo "Cleaning up..."
    # Kill any remaining SBCL processes
    pkill -f "sbcl.*run-tests" 2>/dev/null || true
}

# Set up trap to cleanup on script exit
trap cleanup EXIT

echo "Starting test execution..."
echo ""

# Run the tests with timeout
if timeout ${TIMEOUT} sbcl --script run-tests.lisp ${TIMEOUT}; then
    echo ""
    echo "✅ All tests completed successfully"
    exit 0
else
    EXIT_CODE=$?
    echo ""
    if [ $EXIT_CODE -eq 124 ]; then
        echo "❌ Tests timed out after ${TIMEOUT} seconds"
        echo "Consider increasing the timeout or investigating slow tests"
    elif [ $EXIT_CODE -eq 1 ]; then
        echo "❌ Test setup failed"
        echo "Check that all dependencies are available"
    else
        echo "❌ Tests failed with exit code ${EXIT_CODE}"
    fi
    exit $EXIT_CODE
fi


