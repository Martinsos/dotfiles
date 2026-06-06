{ pkgs }: rec {
  baseEmacs = pkgs.emacs-pgtk;
    # TODO: Move defining of these to Emacs.org, same like I did for external CLI tools?
    #       I could define withEnv helper and use that instead of overly specific withLspUsePlists.
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
}
