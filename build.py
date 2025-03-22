#!/usr/bin/env python3

import os
import sys
import time
import signal
import subprocess
import argparse
import shutil
from pathlib import Path

def run_command(cmd, timeout_seconds=30, activity_timeout=True):
    """Run a command with an activity-based timeout, preserving interactive output."""
    print(f"Running: {' '.join(cmd)}")
    
    # For interactive output, we'll just use subprocess.run directly
    # with the timeout parameter, and let the output go directly to the terminal
    if not activity_timeout:
        try:
            result = subprocess.run(
                cmd, 
                timeout=timeout_seconds,
                check=False
            )
            return result.returncode == 0
        except subprocess.TimeoutExpired:
            print(f"\nCommand timed out after {timeout_seconds} seconds.")
            return False
        except KeyboardInterrupt:
            print("\nProcess interrupted by user.")
            return False
    
    # For activity-based timeout, we need to be more careful
    # Use separate pipes for stdout and stderr
    process = subprocess.Popen(
        cmd, 
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        universal_newlines=True,
        bufsize=1
    )
    
    last_activity = time.time()
    
    # Use non-blocking reads for both stdout and stderr
    import select
    import fcntl
    import os
    
    # Set stdout and stderr to non-blocking mode
    for pipe in [process.stdout, process.stderr]:
        flags = fcntl.fcntl(pipe.fileno(), fcntl.F_GETFL)
        fcntl.fcntl(pipe.fileno(), fcntl.F_SETFL, flags | os.O_NONBLOCK)
    
    try:
        while process.poll() is None:
            # Wait for data on either stdout or stderr
            ready, _, _ = select.select([process.stdout, process.stderr], [], [], 0.1)
            
            activity_detected = False
            
            for pipe in ready:
                output = pipe.read()
                if output:
                    activity_detected = True
                    # Write to appropriate stream
                    if pipe == process.stdout:
                        sys.stdout.write(output)
                        sys.stdout.flush()
                    else:
                        sys.stderr.write(output)
                        sys.stderr.flush()
            
            if activity_detected:
                last_activity = time.time()
            
            # Check if we've exceeded the inactivity timeout
            if time.time() - last_activity > timeout_seconds:
                print(f"\nNo activity for {timeout_seconds} seconds. Terminating process.")
                process.terminate()
                try:
                    process.wait(timeout=5)  # Give it 5 seconds to terminate gracefully
                except subprocess.TimeoutExpired:
                    process.kill()  # Force kill if it doesn't terminate
                return False
    
    except KeyboardInterrupt:
        print("\nProcess interrupted by user.")
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
        return False
    
    # Read any remaining output
    stdout, stderr = process.communicate()
    if stdout:
        sys.stdout.write(stdout)
    if stderr:
        sys.stderr.write(stderr)
    
    return process.returncode == 0

def check_std4_repo():
    """Check and manage the std4 repository."""
    repo_root = os.getcwd()
    std4_dir = os.path.join(repo_root, ".lake", "packages", "std")
    
    print(f"Checking for std4 repository at: {std4_dir}")
    
    if os.path.isdir(std4_dir) and os.path.isdir(os.path.join(std4_dir, ".git")):
        print("Found existing std4 repository. Skipping clone.")
        return True
    
    print("std4 repository not found or incomplete. Cloning...")
    # Make sure the parent directory exists
    os.makedirs(os.path.join(repo_root, ".lake", "packages"), exist_ok=True)
    
    # Clean up any partial directory that might exist
    if os.path.isdir(std4_dir):
        shutil.rmtree(std4_dir)
    
    # Clone the repository - don't use activity timeout for this
    return run_command(["git", "clone", "https://github.com/leanprover/std4.git", std4_dir], 
                      timeout_seconds=180, activity_timeout=False)

def main():
    parser = argparse.ArgumentParser(description="Build script for Amadeus project")
    parser.add_argument("--skip-deps", action="store_true", help="Skip dependency management")
    args = parser.parse_args()
    
    # Run repomix - don't use activity timeout for this
    print("Running repomix...")
    run_command(["repomix"], timeout_seconds=60, activity_timeout=False)
    
    if not args.skip_deps:
        # Handle std4 repository
        if not check_std4_repo():
            print("Warning: Issue with std4 repository management")
        
        # Run lake update without activity timeout to preserve progress output
        print("Running lake update (timeout: 10 minutes)...")
        lake_update_success = run_command(["lake", "update"], timeout_seconds=600, activity_timeout=False)
        
        if not lake_update_success:
            print("Warning: lake update terminated (timeout or user interrupt)")
            # Check if the manifest file exists despite the timeout
            if os.path.isfile("lake-manifest.json"):
                print("lake-manifest.json exists, continuing with build...")
            else:
                print("Critical error: No manifest file generated. Build will likely fail.")
    
    # Build the project - don't use activity timeout for this
    print("Building project...")
    build_success = run_command(["lake", "build"], timeout_seconds=300, activity_timeout=False)
    
    if build_success:
        print("\nBuild completed successfully. To run the executable, use: lake exe amadeus")
    else:
        print("\nBuild failed or was interrupted.")
        return 1
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
