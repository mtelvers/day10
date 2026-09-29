#!/bin/bash
# Differential harness for revdeps.
#
# The reference is the three "opam list" commands opam-repo-ci runs, unioned.
# They are not one command with all the flags: --recursive composed with
# --depopts and --with-test walks the closure through optional and test edges,
# which for coq.9.2.0 turns 11 packages into 20.
#
# The candidate is "day10 revdeps".  Set CANDIDATE empty to record the
# reference alone.
#
# The switches only need the right invariant, not a built compiler: what the
# variant changes is which versions of a dependent are installable at all, and
# that comes from the universe.  Make them with
#
#   opam switch create revdeps-4.14.2 --empty
#   opam switch set-invariant --switch revdeps-4.14.2 ocaml-base-compiler.4.14.2
#
# Run: SWITCHES=... TARGETS=... ./revdeps-harness.sh
set -u

OUT=${OUT:-./revdeps-corpus}
SWITCHES=${SWITCHES:-"revdeps-4.14.2 revdeps-5.5.1"}
TARGETS=${TARGETS:-"coq.9.2.0 dune.3.20.2"}
CANDIDATE=${CANDIDATE:-day10}          # empty to record the reference alone

OPAM_REPOSITORY=${OPAM_REPOSITORY:-$HOME/opam-repository}
OS_DISTRIBUTION=${OS_DISTRIBUTION:-ubuntu}
OS_VERSION=${OS_VERSION:-24.04}
FORK=${FORK:-20}

mkdir -p "$OUT"

# Each query is --depends-on first and --coinstallable-with second for a
# reason: alone, the coinstallable filter tests every package in the universe
# and takes 264s to answer that 88% of them qualify.  Narrowing to the
# dependents first leaves it 22 to check, and that is the whole difference
# between 8s and 264s.
reference() {
  local switch=$1 target=$2
  for flags in "--depopts" "--recursive" "--with-test --depopts"; do
    opam list -s --color=never --switch "$switch" \
      --depends-on "$target" --coinstallable-with "$target" \
      --all-versions $flags 2>/dev/null
  done | sort -u | grep -Fvx "$target"    # opam lists the target among its own
                                          # dependents and day10 deliberately
                                          # does not, so drop it rather than
                                          # report it every run
}

# What the filter had to choose from, so a run reports how much work the
# coinstallable check was asked to do rather than only what survived it.
candidates() {
  local switch=$1 target=$2
  for flags in "--depopts" "--recursive" "--with-test --depopts"; do
    opam list -s --color=never --switch "$switch" \
      --depends-on "$target" --all-versions $flags 2>/dev/null
  done | sort -u
}

# day10's own answer: it enumerates the dependents and solves, so this
# compares both halves against opam's, not just the filter.  It needs no
# cache, base image or container -- solving is all it does.
day10_candidate() {
  local switch=$1 target=$2
  local ocaml=${switch#revdeps-}
  day10 revdeps --fork "$FORK" \
    --opam-repository "$OPAM_REPOSITORY" \
    --ocaml-version "ocaml.$ocaml" --arch x86_64 --os linux \
    --os-distribution "$OS_DISTRIBUTION" --os-version "$OS_VERSION" \
    "$target" 2>/dev/null
}

printf '%-16s %-14s %8s %8s %8s %9s  %s\n' \
  SWITCH TARGET CANDS REF CAND DIFF SECONDS

for switch in $SWITCHES; do
  for target in $TARGETS; do
    tag="$OUT/${switch}__${target}"

    start=$(date +%s.%N)
    reference "$switch" "$target" > "$tag.reference"
    end=$(date +%s.%N)
    secs=$(echo "$end - $start" | bc)

    candidates "$switch" "$target" > "$tag.candidates"
    ncand=$(wc -l < "$tag.candidates")
    nref=$(wc -l < "$tag.reference")

    # Both sides have to be reading the same packages.  opam answers from the
    # switch's repository and day10 from a directory, and when those were a
    # different checkout the run still produced a tidy table -- of packages one
    # side had never heard of.  Every disagreement looked like a solver
    # difference.  A candidate missing from day10's repository is that fault,
    # not a result, so say so and move on rather than report a number.
    missing=$(while read -r c; do
                [ -n "$c" ] || continue
                [ -d "$OPAM_REPOSITORY/packages/${c%%.*}/$c" ] || echo "$c"
              done < "$tag.candidates" | tee "$tag.missing" | wc -l)
    if [ "$missing" -gt 0 ]; then
      printf '%-16s %-14s %8s  SKIPPED: %s candidates absent from %s (see %s)\n' \
        "$switch" "$target" "$ncand" "$missing" "$OPAM_REPOSITORY" "$tag.missing"
      continue
    fi

    if [ -n "$CANDIDATE" ]; then
      cstart=$(date +%s.%N)
      day10_candidate "$switch" "$target" | sort -u > "$tag.candidate"
      cend=$(date +%s.%N)
      csecs=$(echo "$cend - $cstart" | bc)
      nnew=$(wc -l < "$tag.candidate")
      # Reported separately: agreeing on a count is not agreeing on a set.
      only_ref=$(comm -23 "$tag.reference" "$tag.candidate" | tee "$tag.only-reference" | wc -l)
      only_cand=$(comm -13 "$tag.reference" "$tag.candidate" | tee "$tag.only-candidate" | wc -l)
      verdict="-$only_ref/+$only_cand"
    else
      nnew="-"; verdict="-"; csecs=0
    fi

    printf '%-16s %-14s %8s %8s %8s %9s  %6.1f/%.1f\n' \
      "$switch" "$target" "$ncand" "$nref" "$nnew" "$verdict" "$secs" "$csecs"
  done
done

echo
echo "corpus in $OUT"
[ -n "$CANDIDATE" ] || echo "no CANDIDATE set: reference recorded, nothing compared"
