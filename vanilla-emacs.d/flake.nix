{
  description = "My Emacs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    # Relevant functionality emacs-overlay brings:
    #  - Updates emacs packages with latest (daily) packages from Elpa and Melpa.
    #    This happens regardless of us using ...FromUsePackage or not.
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

        emacs = pkgs.emacs-pgtk;

        # TODO(later): treesit-grammars.with-grammars to have nix handle them
        #  instead of elisp.  Once we do that, we can delete the list of them
        #  from Emacs.org allegedly and there should be no duplication.
        #  The only problem is that there is no for prisma in nixpkgs so we
        #  would have to define that one on our own, but we can do that.

        emacsPkgs = (pkgs.emacsPackagesFor emacs).overrideScope (final: prev: {
          # Here I can pin/patch/add emacs packages.
          #
          # Example:
          # magit = prev.magit.overrideAttrs (old: {
          #   src = pkgs.fetchFromGitHub {
          #     owner = "magit"; repo = "magit";
          #     rev = "abc123..."; sha256 = "...";
          #   };
          # });
        });

        # TODO: Consider adding use-package keyword :from 'nix or smth similar
        # in Emacs.config, both for documentation but also maybe it can be used
        # to then automaticaly generate this list below?
        myEmacs = emacsPkgs.emacsWithPackages (emacsPkgs: with emacsPkgs; [
          ace-window
          all-the-icons
          all-the-icons-dired
          amx
          avy
          colorful-mode
          column-enforce-mode
          company
          counsel
          counsel-projectile
          diff-hl
          dired-hide-dotfiles
          doom-modeline
          doom-themes
          elfeed
          ethan-wspace
          evil
          evil-collection
          evil-escape
          evil-org
          flycheck
          flycheck-posframe
          general
          gptel
          haskell-mode
          helpful
          hl-todo
          htmlize
          hydra
          imenu-list
          ivy
          ivy-rich
          jinx
          lsp-haskell
          lsp-ivy
          lsp-mode
          lsp-treemacs
          lsp-ui
          magit
          markdown-mode
          nix-mode
          olivetti
          org-appear
          org-download
          org-gcal
          org-modern
          org-present
          org-rainbow-tags
          org-super-agenda
          org-superstar
          org-tidy
          powershell
          projectile
          quick-peek
          rainbow-delimiters
          sideline
          sideline-flycheck
          sideline-lsp
          swiper
          undo-fu
          visual-fill-column
          vterm
          vterm-toggle
          vundo
          which-key
          winum
          xclip
        ]);

        # TODO/NOTE(Martin) Understanding :ensure and who does what, this is what I understood so far is the best:
        # - :ensure nil by default in emacs, since they are either built in or coming from nix.
        # - package-enable-at-startup -> try with nil, allegedly nix modifies load path itself, and package.el could just potentially load packages we are not aware of.
        #   Another theory is it that it has to be t to load nix packages -> but I doubt that.
        #   Finally, we might want to make it t to support any packages we handle via package.el.
        # - There might be packages that are built-in but I install them by accident, recognize them and make them internal.
        # - Update docs in Emacs.config to document new stuff.
        # - Come up with new workflow for updating emacs, for getting the logs Come up with new workflow for updating emacs, for getting the logs to read through.

      in {
        packages = rec {
          emacs = myEmacs;
          default = emacs;
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
