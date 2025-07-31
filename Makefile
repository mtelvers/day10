# Makefile for running health-checks on all available OPAM packages in parallel
# Usage: make -j<N> all                                    (where N is the number of parallel jobs)
#        make OUTPUT_DIR=/path/to/output all               (to specify custom output directory)
#        make OPAM_REPO=/path/to/packages all              (to specify custom opam repository)
#        make clean                                        (to remove markdown files)


# OS target
SYSTEM := debian-12

# Output directory - can be overridden on command line: make OUTPUT_DIR=/path/to/output
OUTPUT_DIR := .

# Path to the opam repository root (for git operations) - can be overridden
OPAM_REPO := /home/mtelvers/opam-repository
#
# Output directory - can be overridden on command line: make OUTPUT_DIR=/path/to/output
CACHE_DIR := /home/mtelvers/cache

# Get the git commit SHA of the opam repository
OPAM_SHA := $(shell git -C "$(OPAM_REPO)" rev-parse HEAD 2>/dev/null || echo "unknown")

# Get the list of packages from opam
PACKAGES := $(shell ./_build/install/default/bin/day10 list --opam-repository "$(OPAM_REPO)")

# Create target names using .json suffix for markdown output in OPAM_SHA subdirectory
TARGETS := $(addprefix $(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)/, $(addsuffix .json, $(PACKAGES)))

# Default target - depends on all package health-checks
all: $(TARGETS)

# Pattern rule for running health-check on each package and generating markdown
# Extract package name from the full path: $(OUTPUT_DIR)/_packages/package.json -> package
$(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)/%.json:
	@mkdir -p $(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)
	./_build/install/default/bin/day10 health-check --cache-dir "$(CACHE_DIR)" --opam-repository "$(OPAM_REPO)" --json $@ $(basename $(notdir $@))

# Clean up markdown files
clean:
	rm -rf $(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)

# Show the list of packages that will be processed
list:
	@echo "Packages to process (from $(OPAM_REPO)/packages, output to $(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)/):"
	@echo $(PACKAGES) | tr ' ' '\n'

# Count total packages
count:
	@echo "Total packages: $(words $(PACKAGES))"

.PHONY: all clean list count
