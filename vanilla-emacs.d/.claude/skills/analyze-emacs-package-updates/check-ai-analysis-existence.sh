#!/usr/bin/env bash
# Exit 0 if emacs-update-ai-analysis/<pkg>.org exists and is newer than
# emacs-update.org (analysis is up-to-date, skip). Exit 1 otherwise.
# Run from the project root.
#
# Usage: check-ai-analysis-existence.sh <pkg>

set -u
pkg=${1:?usage: $0 <pkg>}
file="emacs-update-ai-analysis/${pkg}.org"
[ -f "$file" ] && [ "$file" -nt emacs-update.org ]
