#!/usr/bin/env bash
#
# Generates the `emacs-update/analysis.org`, by comparing version of emacs and packages
# from HEAD with the current worktree (based on the flake.lock).
# Idea is that you run `nix flake update` and then this script to review the upgrade.
# Also leaves behind the git repos of all the packages under `emacs-update/repos/`.
# Env vars:
#   OLD_REV / NEW_REV       Optional, overrides HEAD / worktree rev from flake.lock.
#
set -euo pipefail
cd "$(dirname "$0")"

OUT_DIR="emacs-update"
REPOS="$OUT_DIR/repos"
ANALYSIS_FILE="$OUT_DIR/analysis.org"
AF="$ANALYSIS_FILE"

main() {
  mkdir -p "$REPOS"

  new_nix_rev="${NEW_REV:-$(cat flake.lock             | jq -r '.nodes.nixpkgs.locked.rev')}"
  old_nix_rev="${OLD_REV:-$(git show HEAD:./flake.lock | jq -r '.nodes.nixpkgs.locked.rev')}"
  echo "old rev: $old_nix_rev, new rev: $new_nix_rev"
  if [[ "$old_nix_rev" == "$new_nix_rev" ]]; then
    echo "WARNING: old and new nixpkgs revs are identical."
  fi

  # Merge into one array of new-package records, each with `.old` = the matching old record.
  # Order (and thus report order) comes from the new info, preserving used-emacs-pkgs.nix order.
  echo "Evaluating package info for old and new revs (this can take a while)..."
  pkgs_info=$(jq -n \
    --argjson old "$(used_emacs_pkgs_info "$old_nix_rev")" \
    --argjson new "$(used_emacs_pkgs_info "$new_nix_rev")" '
    ($old | map({ (.name): . }) | add) as $oldByName
    | $new | map(. + { old: ($oldByName[.name] // {}) })
  ')
  pkgs_names=$(jq -r '.[].name' <<<"$pkgs_info")

  # TODO(later): Add Emacs NEWS.

  {
    echo "#+TITLE: Emacs update analysis"
    echo "#+STARTUP: overview"
    echo "# nixpkgs ${old_nix_rev:0:12} -> ${new_nix_rev:0:12}"
    echo
    echo "* Emacs"
    echo "  TO YET IMPLEMENT"
  } > "$AF"

  echo >> "$AF"

  echo "* Emacs Packages" >> "$AF"

  changed=0 unchanged=0
  for name in $pkgs_names; do
    {
      read -r new_ver; read -r old_ver
      read -r new_rev; read -r old_rev
      read -r git_repo_url; read -r homepage
    } < <(jq -r --arg n "$name" '
      .[] | select(.name == $n)
      | (.version // "?"), (.old.version // "(absent)"),
        (.rev // ""), (.old.rev // ""),
        (.gitRepoUrl // ""), (.homepage // "")' <<<"$pkgs_info")

    if [[ -n "$old_rev" && -n "$new_rev" ]]; then
      [[ "$old_rev" == "$new_rev" ]] && { unchanged=$((unchanged + 1)); continue; }
    else
      [[ "$old_ver" == "$new_ver" ]] && { unchanged=$((unchanged + 1)); continue; }
    fi
    changed=$((changed + 1))

    {
      echo
      echo "** $name"
      echo "- Old: $old_ver ${old_rev:0:12}"
      echo "- New: $new_ver ${new_rev:0:12}"
    } >> "$AF"
    [[ -n "$homepage" ]] && echo "- Homepage: $homepage" >> "$AF"

    if [[ -z "$git_repo_url" ]]; then
      echo "- Upstream repo: unknown" >> "$AF"
      continue
    fi

    echo "- Upstream repo: $git_repo_url" >> "$AF"
    echo "- Local repo: [[file:repos/$name/]]" >> "$AF"

    if ! { clone_package_repo "$name" "$git_repo_url" \
           && [[ -n "$new_rev" ]] && ensure_package_repo_has_rev "$name" "$new_rev" \
           && [[ -n "$old_rev" ]] && ensure_package_repo_has_rev "$name" "$old_rev"; }; then
      echo "- (Could not clone/fetch both revs to compute log.)" >> "$AF"
      continue
    fi

    {
      echo "*** Git log"
      echo "#+begin_src text"
      git -C "$REPOS/$name" log --no-merges --date=short \
          --pretty='%h %ad %s' "$old_rev".."$new_rev" 2>/dev/null || echo "(could not compute log)"
      echo "#+end_src"
    } >> "$AF"

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
      } >> "$AF"
    done
  done

  echo "Generated $AF"
  echo "Packages: $changed changed, $unchanged unchanged (of $(jq 'length' <<<"$pkgs_info") total)."
}

# --- Helper functions --------------------------------------------------------

used_emacs_pkgs_info() {
  local rev=$1
  nix eval --json --impure --expr \
    "(import ./used-emacs-pkgs-info.nix { rev = \"$rev\"; }).pkgsInfo"
}

clone_package_repo() {
  local package_name=$1
  local git_repo_url=$2
  local dir="$REPOS/$package_name"
  [[ -d "$dir/.git" ]] && return 0
  git clone --quiet --filter=blob:none "$git_repo_url" "$dir" 2>/dev/null
}

ensure_package_repo_has_rev() {
  local package_name=$1
  local rev=$2
  local dir="$REPOS/$package_name"
  git -C "$dir" rev-parse --verify --quiet "$rev^{commit}" >/dev/null 2>&1 && return 0
  git -C "$dir" fetch --quiet --filter=blob:none origin "$rev" 2>/dev/null
}

main "$@"
