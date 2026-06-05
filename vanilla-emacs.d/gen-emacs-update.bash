#!/usr/bin/env bash
#
# Generates the `emacs-update/` dir used to review an Emacs (packages) update.
#
# It compares the *currently committed* nixpkgs rev (HEAD:flake.lock) against the
# *working-tree* nixpkgs rev (flake.lock, after `nix flake update`), and for every Emacs
# package produces git log + changelog diffs between the two pinned upstream revs. It also
# diffs Emacs' own NEWS when the Emacs version changes.
#
# Outputs (all under emacs-update/, which is gitignored):
#   - old-versions.json : per-package info ({version, rev, cloneUrl, ...}) at the OLD nixpkgs rev
#   - new-versions.json : same, at the NEW nixpkgs rev
#   - repos/<pkg>/      : clone of each package repo (left around for manual/AI inspection)
#   - analysis.org      : the human/AI-readable report
#
# Expected workflow (run from a throwaway `_emacs-update` worktree):
#   nix flake update          # moves flake.lock to latest nixos-unstable
#   ./gen-emacs-update.bash    # this script  (both wrapped by `make update`)
#
# Env:
#   PKGS="magit vundo ..."  restrict to a subset of packages (for testing).
#   OLD_REV / NEW_REV       override either nixpkgs rev (a full rev or branch ref) to preview
#                           against arbitrary nixpkgs, instead of the HEAD/working-tree locks.
#
set -euo pipefail
cd "$(dirname "$0")" # so relative paths (flake.lock, the .nix files) resolve regardless of cwd.

OUT="emacs-update"
REPOS="$OUT/repos"
ORG="$OUT/analysis.org"
mkdir -p "$REPOS"

# --- 1. OLD vs NEW nixpkgs revs ------------------------------------------------
# `HEAD:./flake.lock` resolves relative to cwd (the flake may live in a subdir of the repo).
new_nixpkgs="${NEW_REV:-$(jq -r '.nodes.nixpkgs.locked.rev' flake.lock)}"
old_nixpkgs="${OLD_REV:-$(git show HEAD:./flake.lock | jq -r '.nodes.nixpkgs.locked.rev')}"
echo "old nixpkgs: $old_nixpkgs"
echo "new nixpkgs: $new_nixpkgs"
if [[ "$old_nixpkgs" == "$new_nixpkgs" ]]; then
  echo "WARNING: old and new nixpkgs revs are identical."
  echo "         Did you forget to run 'nix flake update'? Nothing will have changed."
fi

# --- 2. Evaluate package info at a given nixpkgs rev ---------------------------
# The eval logic lives in get-emacs-packages-info.nix (pure evaluation, no build). By default it
# uses every package in emacs-packages.nix; PKGS overrides that with a subset, for testing.
names_arg=""
[[ -n "${PKGS:-}" ]] && names_arg="names = [ $(printf '"%s" ' ${PKGS}) ];"

eval_pkg_info() { # $1 = nixpkgs rev  ->  json on stdout
  nix eval --json --impure --expr \
    'import ./get-emacs-packages-info.nix { rev = "'"$1"'"; '"$names_arg"' }'
}

echo "Evaluating OLD versions ($old_nixpkgs)..."
eval_pkg_info "$old_nixpkgs" > "$OUT/old-versions.json"
echo "Evaluating NEW versions ($new_nixpkgs)..."
eval_pkg_info "$new_nixpkgs" > "$OUT/new-versions.json"

# Merge into one array of new-package records, each with `.old` = the matching old record.
# Names (and thus report order) come from new-versions.json, preserving emacs-packages.nix order.
merged=$(jq -s '
  (.[0].packages | map({ (.name): . }) | add) as $old
  | .[1].packages | map(. + { old: ($old[.name] // {}) })
' "$OUT/old-versions.json" "$OUT/new-versions.json")
names=$(jq -r '.packages[].name' "$OUT/new-versions.json")

# --- 3. Git helpers ------------------------------------------------------------
clone_repo() { # $1=name $2=cloneUrl  (partial clone: history without blobs, fetched on demand)
  local dir="$REPOS/$1"
  [[ -d "$dir/.git" ]] && return 0
  git clone --quiet --filter=blob:none "$2" "$dir" 2>/dev/null
}
ensure_rev() { # $1=name $2=rev  (make sure the commit is present locally)
  local dir="$REPOS/$1"
  git -C "$dir" rev-parse --verify --quiet "$2^{commit}" >/dev/null 2>&1 && return 0
  git -C "$dir" fetch --quiet --filter=blob:none origin "$2" 2>/dev/null
}

# --- 4. Emacs NEWS -------------------------------------------------------------
# Extract etc/NEWS from the GNU release tarball (only used when the Emacs version changes).
fetch_news() { # $1=version  ->  NEWS text on stdout
  local ver="$1"
  curl -fsSL --max-time 300 "https://ftp.gnu.org/gnu/emacs/emacs-$ver.tar.xz" \
    | tar -xJ --wildcards -O "emacs-$ver/etc/NEWS" 2>/dev/null
}

# --- 5. Render analysis.org ----------------------------------------------------
old_emacs=$(jq -r '.emacsVersion' "$OUT/old-versions.json")
new_emacs=$(jq -r '.emacsVersion' "$OUT/new-versions.json")

{
  echo "#+TITLE: Emacs update report"
  echo "#+STARTUP: overview"
  echo "# nixpkgs ${old_nixpkgs:0:12} -> ${new_nixpkgs:0:12}"
  echo
  echo "* Emacs"
} > "$ORG"

if [[ "$old_emacs" == "$new_emacs" ]]; then
  echo "Version: $new_emacs (unchanged)" >> "$ORG"
else
  {
    echo "Version: $old_emacs -> $new_emacs"
    echo
    echo "** NEWS diff"
  } >> "$ORG"
  echo "Fetching Emacs NEWS for $old_emacs and $new_emacs..."
  old_news=$(mktemp); new_news=$(mktemp)
  if fetch_news "$old_emacs" > "$old_news" && fetch_news "$new_emacs" > "$new_news"; then
    {
      echo "#+begin_src diff"
      diff -u --label "NEWS ($old_emacs)" --label "NEWS ($new_emacs)" "$old_news" "$new_news" || true
      echo "#+end_src"
    } >> "$ORG"
  else
    echo "(Could not fetch NEWS tarball(s) automatically.)" >> "$ORG"
  fi
  rm -f "$old_news" "$new_news"
fi

echo >> "$ORG"
echo "* Emacs Packages" >> "$ORG"

changed=0 unchanged=0 noversion=0
for name in $names; do
  # Pull every field we need for this package (one line each, so empty fields stay aligned).
  # rev/cloneUrl are already resolved (forge fallbacks handled) by get-emacs-packages-info.nix.
  {
    read -r new_ver; read -r old_ver
    read -r new_rev; read -r old_rev
    read -r clone_url; read -r tarball; read -r homepage
  } < <(jq -r --arg n "$name" '
    .[] | select(.name == $n)
    | (.version // "?"), (.old.version // "(absent)"),
      (.rev // ""), (.old.rev // ""),
      (.cloneUrl // ""), (.tarball // ""), (.homepage // "")' <<<"$merged")

  # Changed? Compare rev when present, else fall back to version.
  if [[ "${old_rev}|${old_ver}" == "${new_rev}|${new_ver}" ]]; then
    unchanged=$((unchanged + 1)); continue
  fi
  changed=$((changed + 1))

  {
    echo
    echo "** $name"
    echo "- Old: $old_ver ${old_rev:0:12}"
    echo "- New: $new_ver ${new_rev:0:12}"
  } >> "$ORG"

  # No clone url -> version-only (e.g. GNU ELPA) bump. Surface homepage + tarball for hand digging.
  if [[ -z "$clone_url" ]]; then
    noversion=$((noversion + 1))
    echo "- (No upstream git source available; version-only bump.)" >> "$ORG"
    [[ -n "$homepage" ]] && echo "- Homepage: $homepage" >> "$ORG"
    [[ -n "$tarball"  ]] && echo "- Tarball: $tarball" >> "$ORG"
    continue
  fi

  echo "- Upstream: $clone_url" >> "$ORG"
  echo "- Local repo: [[file:repos/$name/]]" >> "$ORG"

  if ! { clone_repo "$name" "$clone_url" \
         && [[ -n "$new_rev" ]] && ensure_rev "$name" "$new_rev" \
         && [[ -n "$old_rev" ]] && ensure_rev "$name" "$old_rev"; }; then
    echo "- (Could not clone/fetch both revs to compute log.)" >> "$ORG"
    continue
  fi

  {
    echo "*** Git log"
    echo "#+begin_src text"
    git -C "$REPOS/$name" log --no-merges --date=short \
        --pretty='%h %ad %s' "$old_rev".."$new_rev" 2>/dev/null || echo "(could not compute log)"
    echo "#+end_src"
  } >> "$ORG"

  # Changelog-ish files that changed between the revs.
  cl_files=$(git -C "$REPOS/$name" ls-tree -r --name-only "$new_rev" 2>/dev/null \
             | grep -iE '(^|/)(change(log|s)|news|history)(\.[a-z]+)?$' || true)
  for f in $cl_files; do
    d=$(git -C "$REPOS/$name" diff "$old_rev" "$new_rev" -- "$f" 2>/dev/null || true)
    [[ -z "$d" ]] && continue
    {
      echo "*** Changelog diff: $f"
      echo "#+begin_src diff"
      echo "$d"
      echo "#+end_src"
    } >> "$ORG"
  done
done

echo "Generated $ORG"
echo "Packages: $changed changed, $unchanged unchanged, $noversion version-only" \
     "(of $(jq 'length' <<<"$merged") total)."
