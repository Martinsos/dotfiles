.PHONY: install remove build update

# Because `nix profile` always uses current dir as a profile element name when installing it from a
# local flake. So we can't choose the name, instead I just capture that knowledge here.
# I hardcode it so if the dir name changes, I am aware.
PRF_EL_NAME := vanilla-emacs.d

install:
	@if [ "$(notdir $(CURDIR))" != "$(PRF_EL_NAME)" ]; then \
	  echo "ERROR: current dir name does not match nix profile element name ($(PRF_EL_NAME))."; \
	  echo "  I should probably delete old nix profile and update the PRF_EL_NAME here in the Makefile."; \
	  exit 1; \
	fi
	@if nix profile list --json | jq -e '.elements | has("$(PRF_EL_NAME)")' >/dev/null; then \
          echo "Upgrading"; \
	  nix profile upgrade $(PRF_EL_NAME); \
	else \
          echo "Adding/installing"; \
	  nix profile add .; \
	fi

remove:
	nix profile remove $(PRF_EL_NAME)

build:
	nix build

# Move the nix lock to latest nixos-unstable, then generate the emacs-update/ review dir (old/new
# versions, package repos, analysis.org).
# Meant to be run from a throwaway `_emacs-update` git worktree.
update:
	nix flake update
	./gen-emacs-update.bash
