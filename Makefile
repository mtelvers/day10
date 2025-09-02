# Makefile for running health-checks on all available OPAM packages in parallel
# Usage: make -j<N> all                                    (where N is the number of parallel jobs)
#        make OUTPUT_DIR=/path/to/output all               (to specify custom output directory)
#        make OPAM_REPO=/path/to/packages all              (to specify custom opam repository)
#        make clean                                        (to remove markdown files)

# OS target
SYSTEM := debian-12

# Compiler versions - can be overridden on command line
COMPILERS := ocaml.4.08.1 ocaml.4.09.1 ocaml.4.10.2 ocaml.4.11.2 ocaml.4.12.1 ocaml.4.13.1 ocaml.4.14.2 ocaml.5.0.0 ocaml.5.1.1 ocaml.5.2.1 ocaml.5.3.0

# Output directory - can be overridden on command line: make OUTPUT_DIR=/path/to/output
OUTPUT_DIR := output

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

$(OUTPUT_DIR)/commits.json:
	@echo "[]" > $@.tmp
	@for dir in $(OUTPUT_DIR)/*/; do \
		if [ -d "$$dir" ]; then \
			sha=$$(basename "$$dir"); \
			echo "Processing SHA: $$sha"; \
			git -C $(OPAM_REPO) show --pretty=format:'%H%x00%aI%x00%s%x00' -s "$$sha" 2>/dev/null | \
			jq -R -s 'if . == "" then empty else split("\n")[0] | split("\u0000") | {"sha": .[0], "date": .[1], "message": .[2]} end' | \
			jq -s 'if length > 0 then .[0] else {"sha": "'$$sha'", "date": null, "message": "Unknown commit"} end' > $@.entry && \
			jq --slurpfile entry $@.entry '. += $$entry' $@.tmp > $@.tmp2 && \
			mv $@.tmp2 $@.tmp; \
			rm -f $@.entry; \
		fi; \
	done
	@mv $@.tmp $@
	@echo "JSON file generated: $@"

$(OUTPUT_DIR)/%/commit.json:
	@echo "Generating $@"
	@{ \
		echo '{}'; \
		for os_dir in $(@D)/*/; do \
			if [ -d "$$os_dir" ]; then \
				os=$$(basename "$$os_dir"); \
				for compiler_dir in "$$os_dir"*/; do \
					if [ -d "$$compiler_dir" ]; then \
						compiler=$$(basename "$$compiler_dir"); \
						json_files="$$compiler_dir"*.json; \
						if ls $$json_files >/dev/null 2>&1; then \
							cat $$json_files | jq -s --arg os "$$os" --arg compiler "$$compiler" \
								'{"os": $$os, "compiler": $$compiler, "data": .}'; \
						fi; \
					fi; \
				done; \
			fi; \
		done; \
	} | jq -s '.[1:] | reduce .[] as $$item ({}; .[$$item.os][$$item.compiler] = $$item.data)' > $@

json: $(OUTPUT_DIR)/commits.json $(foreach dir,$(wildcard output/*),$(dir)/commit.json)

copy:
	@find $(CACHE_DIR) -maxdepth 2 \( -name "layer.json" -o -name "build.log" \) -print0 | \
		xargs -0 -P $(shell nproc) -I {} sh -c 'd=$${1%/*}; d=$${d##*/}; mkdir -p $(OUTPUT_DIR)/cache/$$d; cp -l "$$1" $(OUTPUT_DIR)/cache/$$d/' _ {}

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

next:
	git -C $(OPAM_REPO) fetch --all
	next_merge=$$(git -C $(OPAM_REPO) log --merges --format="%H" --reverse HEAD..upstream/master | head -1); \
	if [ -z "$$next_merge" ]; then \
		echo "No merge commits found ahead of current position in upstream/master"; \
		exit 1; \
	fi; \
	echo "Moving to next merge commit: $$next_merge"; \
	git -C $(OPAM_REPO) log --oneline -1 $$next_merge; \
	git -C $(OPAM_REPO) checkout $$next_merge

.PHONY: all clean list count $(COMPILERS)
