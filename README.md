# OBI - OCaml Build Infrastructure

OBI (OCaml Build Infrastructure) is a comprehensive build testing tool for the OCaml ecosystem. It systematically tests OCaml packages from the opam repository across multiple compiler versions, using container isolation to ensure clean, reproducible builds.

## Features

- **Multi-version testing**: Tests packages against multiple OCaml compiler versions
- **Container isolation**: Uses `runc` (OCI runtime) for clean, isolated builds
- **Incremental builds**: Employs filesystem overlays to reuse build layers
- **Parallel execution**: Utilizes process forking for concurrent builds
- **Comprehensive reporting**: Generates HTML reports and dependency graphs

## Installation

```bash
# Build from source
dune build
dune install

# Or using opam (when published)
opam install obi
```

## Usage

### Prerequisites

OBI requires the following to be available on your system:
- **Docker** - for creating the initial container rootfs
- **runc** - OCI container runtime for build isolation
- **sudo** access - for container operations and file ownership management

### Command Line Interface

OBI provides several commands for different stages of the build pipeline:

```bash
# Set up container environment (required first time)
obi setup --work-dir /path/to/work

# Solve dependencies for all packages
obi solve --work-dir /path/to/work --ocaml-versions 5.3.0,4.14.2

# Build all packages
obi build --work-dir /path/to/work

# Generate HTML reports
obi report --work-dir /path/to/work

# Run complete pipeline
obi run --work-dir /path/to/work

# Serve reports on localhost:8080
obi serve --work-dir /path/to/work

# Run with limited packages for testing
obi run --work-dir /path/to/work --limit 15
```

### Configuration

OBI supports multiple ways to configure settings, with the following precedence (highest to lowest):

1. **Command-line arguments** (highest priority)
2. **Environment variables** (`OBI_*`)
3. **Configuration file** (TOML format)
4. **Default values** (lowest priority)

#### Configuration Options

- `--opam-repository PATH`: Path to the opam repository
- `--download-cache PATH`: Download cache directory
- `--work-dir PATH`: Working directory for builds
- `--ocaml-versions VERSIONS`: Comma-separated list of OCaml versions to test
- `--hostname NAME`: Container hostname
- `--config FILE`: Path to TOML configuration file
- `--limit N`: Limit number of packages to build for testing (default: unlimited)

#### Configuration File

OBI will automatically look for config files in these locations:
- `.obi.toml` (current directory)
- `$HOME/.config/obi.toml`

Create a config file with `obi config init` or manually:

```toml
opam_repository = "/path/to/opam-repository"
download_cache = "/path/to/download-cache"
work_dir = "/path/to/work-dir"
ocaml_versions = ["5.3.0", "4.14.2", "4.08.2"]
hostname = "builder"
limit = 15  # Optional: limit packages for testing

[env]
PATH = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
HOME = "/home/opam"
OPAMYES = "1"
OPAMCONFIRMLEVEL = "unsafe-yes"
OPAMERRLOGLEN = "0"
OPAMPRECISETRACKING = "1"
```

#### Config Management Commands

```bash
obi config show                    # Show current configuration
obi config init                    # Create .obi.toml with defaults
obi config init -o custom.toml     # Create config file at custom path  
obi config save -o backup.toml     # Save current config to file
```

#### Environment Variables

All command-line options can also be set via environment variables by prefixing with `OBI_` and converting to uppercase:

- `OBI_OPAM_REPOSITORY`
- `OBI_DOWNLOAD_CACHE`
- `OBI_WORK_DIR`
- `OBI_OCAML_VERSIONS` (comma-separated list)
- `OBI_HOSTNAME`
- `OBI_LIMIT` (number of packages)

### Legacy Makefile Support

For compatibility with the existing setup, several make targets are provided:

```bash
make build          # Build OBI
make obi-setup      # Run obi setup
make obi-solve      # Run obi solve
make obi-build      # Run obi build
make obi-report     # Run obi report
make obi-run        # Run complete pipeline
make obi-serve      # Serve reports
```

## Library Usage

OBI can also be used as a library in OCaml programs:

```ocaml
open Obi

let config = Config.default in
let config = { config with work_dir = "/tmp/obi-work" } in

(* Run the complete pipeline *)
run config

(* Or run individual steps *)
let commit = setup_directories config in
solve_packages config commit;
build_packages config commit;
generate_reports config commit
```

## Architecture

The codebase is organized into logical modules:

- **`Obi_config`**: Configuration management with cmdliner integration
- **`Obi_container`**: Container and OCI runtime management
- **`Obi_solver`**: Package dependency resolution using opam-0install
- **`Obi_builder`**: Build orchestration and incremental builds
- **`Obi_os`**: System operations and parallel execution
- **`Obi_solution`**: JSON/DOT persistence and build result types
- **`Obi_report`**: HTML report and visualization generation

## Development

### Building

```bash
dune build
```

### Testing

```bash
dune exec -- obi --help
dune exec -- obi solve --help
```

### Documentation

See [CLAUDE.md](CLAUDE.md) for detailed technical documentation.

## License

ISC License