# OBI - OCaml Build Infrastructure

## Overview

OBI (OCaml Build Infrastructure) is a comprehensive bulk build testing tool for the OCaml ecosystem. It systematically tests OCaml packages from the opam repository across multiple compiler versions, using container isolation to ensure clean, reproducible builds.

## Purpose

This tool performs automated health checks of OCaml packages by:
- Testing packages against multiple OCaml compiler versions (currently 5.3.0, 4.14.2, 4.08.2)
- Building packages in isolated container environments using `runc`
- Tracking build successes and failures
- Generating dependency graphs and HTML reports
- Using incremental builds with filesystem overlays for efficiency

## Architecture

### Core Components

#### Main Entry Point (`bin/main.ml`)
- Orchestrates the entire build and testing process
- Manages package dependency resolution using `opam-0install`
- Implements parallel build execution across multiple packages
- Generates HTML reports and dependency visualizations

#### Configuration (`bin/config.ml`)
- Central configuration for paths and environment variables
- Defines opam repository location, download cache, and working directories

#### Container Management (`bin/json_config.ml`)
- Creates OCI runtime specifications for container execution
- Manages mount points, capabilities, and security configurations
- Implements filesystem overlays for incremental builds

#### Utilities (`bin/os.ml`)
- File I/O operations
- Process forking and parallel execution management
- System command execution wrappers

#### Data Persistence
- `bin/json_solution.ml` - Saves/loads dependency resolution results as JSON
- `bin/dot_solution.ml` - Generates GraphViz DOT files for dependency visualization

#### Git Integration (`bin/git_context.ml[i]`)
- Interface for reading packages from git repositories
- Context creation for dependency resolution

## Key Features

### 1. Multi-Version Testing
Tests packages against multiple OCaml compiler versions simultaneously to ensure compatibility across the ecosystem.

### 2. Container Isolation
Uses `runc` (OCI runtime) to create isolated build environments, ensuring:
- Clean builds without system contamination
- Reproducible results
- Security through namespace isolation

### 3. Incremental Builds
Employs filesystem overlays to:
- Reuse successful build layers
- Reduce redundant compilation
- Speed up the overall testing process

### 4. Parallel Execution
Utilizes process forking to run multiple builds concurrently, limited by available CPU cores.

### 5. Comprehensive Reporting
Generates:
- HTML reports for each package build
- Dependency graphs in DOT format
- Build status tracking (success/failure)
- Timing information for each build phase

## Build System

### Dependencies
- `opam-0install` - For dependency resolution
- `yojson` - JSON parsing and generation
- `tyxml` - HTML generation
- `ppx_deriving_yojson` - JSON serialization
- `cmdliner` - Command-line argument parsing

### Build Commands
```bash
dune build             # Build the project
obi setup              # Initialize container environment
obi solve              # Solve dependencies for all packages
obi build              # Build all packages
obi report             # Generate HTML reports
obi run                # Run complete pipeline
obi serve              # Serve HTML reports on port 8080
```

## Workflow

1. **Setup Phase**: Initializes container rootfs with Debian base image and opam
2. **Resolution Phase**: For each package, resolves dependencies using opam-0install
3. **Build Phase**: 
   - Creates topologically sorted build order
   - Builds packages incrementally using overlayfs
   - Tracks success/failure for each package
4. **Reporting Phase**: Generates HTML reports and dependency graphs

## Module Structure for Release

### Current Module Structure

The codebase is now organized into logical modules:

1. **Core library modules** (`lib/`):
   - `Obi_config` - Configuration management with cmdliner integration
   - `Obi_container` - Container and OCI runtime management
   - `Obi_solver` - Package dependency resolution using opam-0install
   - `Obi_builder` - Build orchestration and incremental builds
   - `Obi_os` - System operations and parallel execution
   - `Obi_solution` - JSON/DOT persistence and build result types
   - `Obi_report` - HTML report and visualization generation
   - `Obi` - Main library interface

2. **CLI executable** (`bin/`):
   - `main.ml` - Command-line interface using cmdliner

### Configuration

OBI uses a layered configuration approach:
1. Command-line arguments (highest priority)
2. Environment variables  
3. Default values (lowest priority)

Key configuration options:
- `--opam-repository PATH` - Path to opam repository
- `--download-cache PATH` - Download cache directory
- `--work-dir PATH` - Working directory for builds
- `--ocaml-versions VERSIONS` - OCaml versions to test
- `--hostname NAME` - Container hostname

## Testing Strategy

The tool implements a comprehensive testing approach:
- **Isolation**: Each build runs in a fresh container
- **Dependency tracking**: Ensures correct build order
- **Failure propagation**: Marks packages as bad if dependencies fail
- **Result caching**: Avoids rebuilding successful packages

## Future Enhancements

- Support for more OCaml compiler versions
- Configurable package filters
- Enhanced error reporting and diagnostics
- Integration with CI/CD systems
- Performance metrics and trend analysis