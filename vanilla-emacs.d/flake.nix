{
  description = "My Emacs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # TODO
  # - [ ] Come up with new workflow for updating emacs, for getting the diff logs and Emacs NEWS to
  #       read through when updating the packages. I had this for Elpaca, now need it for Nix also.
  # - [ ] Consider using Home Manager instead of my Makefile that runs nix profile commands.
  #       - Seems like a great tutorial: https://github.com/Evertras/simple-homemanager/tree/main .

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

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

        emacsWithPkgs = emacsPkgs.emacsWithPackages (epkgs:
          (import ./emacs-packages.nix epkgs)
          ++ [ (epkgs.treesit-grammars.with-grammars (import ./emacs-treesit-grammars.nix)) ]
        );

        emacsCliTools = import ./emacs-cli-tools.nix { inherit pkgs; };

        emacsWithPkgsAndCliTools = pkgs.symlinkJoin {
          name = "emacs";
          paths = [ emacsWithPkgs ];
          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/emacs \
              --prefix PATH : ${pkgs.lib.makeBinPath emacsCliTools}
          '';
        };
      in {
        packages = rec {
          default = emacs;
          emacs = emacsWithPkgsAndCliTools;
        };
      });
}
