{
  description = "My Emacs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    # Relevant functionality emacs-overlay brings:
    #  - latest (fresher) packages from Elpa and Melpa
    #  - `emacsWithPackagesFromUsePackage` -> parses init.el and builds packages.
    #  - `treesit-grammars.with-grammars` for bundling tree-sitter grammars.
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, emacs-overlay, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };

        emacsPkg = pkgs.emacs-pgtk;

        # TODO(later): treesit-grammars.with-grammars to have nix handle them instead of elisp.
        #  Once we do that, we can delete the list of them from Emacs.org allegedly and there should
        #  be no duplication.  The only problem is that there is no for prisma in nixpkgs so we
        #  would have to define that one on our own, but we can do that.

        # TODO/NOTE(Martin) Understanding :ensure and who does what, this is what I understood so far is the best:
        # - use emacs overlay with parsing use-package.
        # - :ensure t by default in emacs, match the setting here so overlay knows.
        # - Put :ensure nil if built-in, nothing (there ensure t) if Nix should pick itup.
        # - If I want to experiment, I can do it in flake/overlay potentially via override, or via extraEmacsPackages, I think. Or I can do :ensure nil to stop nix from picking it up, and use :vc to have package.el do it (but will :ensure nil also stop package.el? Not sure should test.). One option is also to not use use-package for that package but go lower level wit hpackage-vs-install and require.
        # - package-enable-at-startup -> this needs to be `t` or package.el wont' pick up package sinstalled by nix and use-package will make package.el install them! This makes sense to me.

        # Replacement for elpaca: parse `(use-package ...)` declarations from
        # init.el and produce an emacs with all those packages (plus our
        # grammars) installed.
        #
        # NOTE: init.el today still bootstraps elpaca and sets
        # `use-package-always-ensure t`. With this flake, packages come from
        # nix instead, so the migration also requires editing init.el /
        # Emacs.org to:
        #   - remove the elpaca bootstrap and lock-file plumbing,
        #   - set `use-package-always-ensure nil` (packages are already on
        #     load-path; :ensure is a no-op signal for nix's parser),
        #   - replace any `:ensure (:host github :repo "...")` recipes with
        #     entries in the `override` arg below (for packages not on MELPA).
        # Until those edits land, running this emacs will likely still try to
        # bootstrap elpaca at startup. We'll iterate.
        # TODO: Understand this better. Especially what I do with :ensure
        #   If I want to have package.el enabled. Because I hvae three types of packages now:
        #   built in, installed by nix, and installed by package.el. Or do I want to drop
        #   the package.el? How do I test new packages easily then, or refer to packages
        #   directly that are in some repo or whatever or local? I think I do want to have
        #   package.el on.
        myEmacs = pkgs.emacsWithPackagesFromUsePackage {
          package = emacsPkg;
          config = ./init.el; # TODO: What if we put Emacs.org? But this sound safer.
          # init.el sets `use-package-always-ensure t`, so the parser should
          # pull packages even when `:ensure` isn't explicit.
          alwaysEnsure = true;
          # Don't have the derivation write its own init file — keep using
          # the existing Emacs.org → init.el flow.
          # TODO: Why would it want to write its own derviation file?
          defaultInitFile = false;
          # TODO: What is this one about?
          extraEmacsPackages = epkgs: [];
          # TODO: oh cool can you explain this more? So maybe I don't need package.el? What i I am working on a local package?
          # `override` is where to add custom recipes for packages not on
          # MELPA, or to pin specific revisions. Fill in as parse failures
          # surface during `nix build`.
          # override = epkgs: epkgs // { };
        };

      in {
        # TODO: I saw them doing `= self.packages.emacs` here, sounds nicer?
        packages.default = myEmacs;
        packages.emacs = myEmacs;

        # TODO: Aha why do we need this?
        apps.default = {
          type = "app";
          program = "${myEmacs}/bin/emacs";
        };

        # TODO — external CLI tools the config touches. Decide per tool
        # whether the flake should wrap them onto emacs's PATH, or whether
        # we rely on the system / per-project nix shells.
        #
        # First-pass classification (revisit together):
        #
        #   Emacs-specific — probably belongs here:
        #     - emacs-lsp-booster   (LSP perf wrapper invoked from emacs)
        #     - gitstatusd          (used by ... TBD; also useful in shells)
        #     - lychee              (org-mode link checking)
        #     - editorconfig-core-c (used by editorconfig.el on some setups)
        #
        #   General CLI tools — probably leave to the system:
        #     - git, ripgrep, fd, delta, fzf
        #
        #   Language-/project-specific — do NOT bundle, control per-project:
        #     - haskell-language-server, rust-analyzer, gopls, pyright, ...
        #     - ghc, cargo, node, python, ...
        #
        # Once we agree, wrap `myEmacs` with `pkgs.symlinkJoin` +
        # `makeWrapper --prefix PATH : ${lib.makeBinPath [...]}` so the
        # selected tools are visible to emacs only.
      });
}
