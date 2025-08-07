# Makefile for running health-checks on all available OPAM packages in parallel
# Usage: make -j<N> all                                    (where N is the number of parallel jobs)
#        make OUTPUT_DIR=/path/to/output all               (to specify custom output directory)
#        make OPAM_REPO=/path/to/packages all              (to specify custom opam repository)
#        make clean                                        (to remove markdown files)

# OS target
SYSTEM := debian-12

# Compiler versions - can be overridden on command line
COMPILERS := 5.3.0 4.14.2 4.08.1

# Output directory - can be overridden on command line: make OUTPUT_DIR=/path/to/output
OUTPUT_DIR := .

# Path to the opam repository root (for git operations) - can be overridden
OPAM_REPO := /home/mtelvers/opam-repository

# Output directory - can be overridden on command line: make OUTPUT_DIR=/path/to/output
CACHE_DIR := /home/mtelvers/cache

# Get the git commit SHA of the opam repository
OPAM_SHA := $(shell git -C "$(OPAM_REPO)" rev-parse HEAD 2>/dev/null || echo "unknown")

# Get the list of packages from opam
PACKAGES := $(shell ./_build/install/default/bin/day10 list --opam-repository "$(OPAM_REPO)")
# PACKAGES := 0install.2.18 diffast-api.0.2 alcotest.1.9.0 bos.0.2.1 ansi.0.7.0

# Template to generate rules for each compiler version
define COMPILER_TEMPLATE
$$(OUTPUT_DIR)/$$(OPAM_SHA)/$$(SYSTEM)/$(1)/%.json: | $$(CACHE_DIR)
	@mkdir -p $$(OUTPUT_DIR)/$$(OPAM_SHA)/$$(SYSTEM)/$(1)
	./_build/install/default/bin/day10 health-check \
		--cache-dir "$$(CACHE_DIR)" \
		--opam-repository "$$(OPAM_REPO)" \
		--ocaml-version $(1) \
		--dot $$(basename $$@).dot \
		--json $$@ $$(basename $$(notdir $$@))
endef

# Generate pattern rules for each compiler
$(foreach compiler,$(COMPILERS),$(eval $(call COMPILER_TEMPLATE,$(compiler))))

# Generate all targets for all compiler/package combinations
# Order by package first, then compiler (better resource distribution)
TARGETS := $(foreach package,$(PACKAGES),$(foreach compiler,$(COMPILERS),$(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)/$(compiler)/$(package).json))

# Default target - depends on all package health-checks for all compilers
all: $(TARGETS)

$(CACHE_DIR):
	mkdir -p $(CACHE_DIR)

# Clean up json files for all compilers
clean:
	rm -rf $(foreach compiler,$(COMPILERS),$(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)/$(compiler))

# Show the list of packages that will be processed for each compiler
list:
	@echo "Packages to process (from $(OPAM_REPO)/packages):"
	@$(foreach compiler,$(COMPILERS),echo "Compiler $(compiler): $(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)/$(compiler)";)
	@echo "Packages:"
	@echo $(PACKAGES) | tr ' ' '\n'

# Count total packages across all compilers
count:
	@echo "Total packages per compiler: $(words $(PACKAGES))"
	@echo "Total compilers: $(words $(COMPILERS))"
	@echo "Total targets: $(words $(TARGETS))"

# Targets for building with specific compilers
$(foreach compiler,$(COMPILERS),$(eval $(compiler): $(addprefix $(OUTPUT_DIR)/$(OPAM_SHA)/$(SYSTEM)/$(compiler)/, $(addsuffix .json, $(PACKAGES)))))

.PHONY: all clean list count $(COMPILERS)
