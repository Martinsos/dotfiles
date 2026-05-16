{
  description = "My Emacs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # TODO
  # - [ ] Add some of the external system tools that Emacs needs to the Nix flake. Check TODO below
  #       in this file for more details.
  # - [ ] Come up with new workflow for updating emacs, for getting the diff logs and Emacs NEWS to
  #       read through when updating the packages. I had this for Elpaca, now need it for Nix also.
  # - [ ] Have Nix install treesit grammars. Check TODO below in this file for more details.
  # - [ ] Consider using Home Manager instead of my Makefile that runs nix profile commands.
  #       - Seems like a great tutorial: https://github.com/Evertras/simple-homemanager/tree/main .

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        baseEmacs = pkgs.emacs-pgtk;

        # TODO(later): treesit-grammars.with-grammars (if using overlay) to have nix handle them
        #  instead of elisp.  Once we do that, we can delete the list of them from Emacs.org
        #  allegedly and there should be no duplication.  The only problem is that there is no for
        #  prisma in nixpkgs so we would have to define that one on our own, but we can do that.
        #  Related: https://github.com/nix-community/emacs-overlay/issues/341 .
        #  I am actually not sure if this is just overlay specific or can be also used without overlay.
        #  Yeah quick googling seems to show this has nothing to do with overlay, but is part of nixpkgs.emacsPackages.

        emacsPkgs = (pkgs.emacsPackagesFor baseEmacs).overrideScope (final: prev:
          let
            # In order for lsp-booster elisp package to be more performant, in my emacs config
            # I enabled usage of plists, which then requires lsp-mode and packages
            # dependent on lsp-mode to compile with env var LSP_USE_PLISTS set to true.
            withLspUsePlists = drv: drv.overrideAttrs (old: {
              env = (old.env or {}) // { LSP_USE_PLISTS = "true"; };
            });
          in {
            lsp-mode     = withLspUsePlists prev.lsp-mode;
            lsp-ui       = withLspUsePlists prev.lsp-ui;
            lsp-treemacs = withLspUsePlists prev.lsp-treemacs;
            lsp-ivy      = withLspUsePlists prev.lsp-ivy;
            lsp-haskell  = withLspUsePlists prev.lsp-haskell;
            sideline-lsp = withLspUsePlists prev.sideline-lsp;
          } // {
            # NOTE: Here I can pin/patch/add emacs packages beyond what is in the packageset.
            #
            # Example:
            # magit = prev.magit.overrideAttrs (old: {
            #   src = pkgs.fetchFromGitHub {
            #     owner = "magit"; repo = "magit";
            #     rev = "abc123..."; sha256 = "...";
            #   };
            # });
          }
        );

        myEmacs = emacsPkgs.emacsWithPackages (import ./emacs-packages.nix);
      in {
        packages = rec {
          default = emacs;
          emacs = myEmacs;
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
