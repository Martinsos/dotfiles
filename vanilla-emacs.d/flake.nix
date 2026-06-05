{
  description = "My Emacs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # TODO
  # - [ ] Establish update workflow.
  # My prompt, try with it:
  # Check out the TODO in flake.nix -> what I want is a workflow for updating my emacs (packages). So now I use nix and that is great, my eamcs and packages and all are frozen. Nothing can surprise me. But then, at some point, I
  # want to upgrade it all. Now, to do that in emacs, since my emacs config is going quite deep/internal on some things, and configuring a lot (well that is the point of emacs hehe), one wants to know what is being upgraded
  # exactly, so I can adjust my config -> breaking changes in the packages, in emacs, package becoming built-in, adding new functionality I can use, changing how some internal was done, ... . With elpaca, would I would do, I would
  # have it "fetch" the new updates, then check the current versions of packages as defined by the recipes in the lockfile, check the latest versions, and generate a git log of differences. I think I want the same thing now. So for
  # each package, I would get a git log between the current version and then one I will update it to. On top of that it would be great if I can also get a peek into Changelog since that usually covers it even better. Now that is
  # for elisp packages that are not built-in. For built-in ones, and changes in emacs, I read I should check emacs NEWS. So I guess I need that also, emacs NEWS since my last update. I am not sure how to do this with nix lock files
  # -> I guess we somehow find out current versions of packges, be it git commit hashes or actual version numbers or whatever, then figure out what will those be once we do the update (can we know that without actually doing the
  # udpate? Or do we do it somewhere on the side and check?), ... .
  # Anyway, for the final result, I need an emacs-update-report.org document that has all this info in it, by package -> each package a top level heading, and one extra which is called news I guess. Or maybe packages are second
  # level headings under "third party packages" and news is next to that one at top level hm. Once I have that, the report, I can go on and do AI analysis on it ("read it and warn me of any changes I need to do") plus I can also
  # analyze it manually. The process of obtaining this report should be deterministic though, ideally, so no AI. Maybe a tiny bit if that is the only way to figure out how to obtain CHangelog, although I hope we can do that
  # determenistically also, even if it is some kind of heuristic that works in 90% cases.
  # What I would like you to do is to think hard (ultra hard I guess) about this, propose how you would go about it while explaining the reasoning and steps, and then you can also go and try implementing it. You are in the worktree
  # I created for this on a branch for this.

  # TODO
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
