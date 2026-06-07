{
  description = "My Emacs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  # TODO
  # - [ ] Establish update workflow.
  # Workflow will be (I will document it in Emacs.org):
  # - Create new git worktree (_emacs-update) and cd there.
  # - Run ~make update~
  #   - Runs ~nix update~ -> updates only lock file.
  #   - Runs our bash script(s?) that in =emacs-update/= dir generate old-versions.json, new-versions.json, and git repos of all packages for which they are available.
  #     - It then also generates analysis.org which contains info for each emacs package (git log and change(log) diff) + NEWS for emacs.
  #       - First top heading is "Emacs", second is "Emacs Packages", where for each package we have old rev and/or version, new, git repo url, and then git logs, and also changelog (or similar file) diff if available, oh and also link to it repo on the disk so I can easily jump there.
  #     - We don't bother with treesitter grammars nor external CLI tools.
  # - Run AI on the analysis.org to add its analysis to each package under "AI analysis" subheading.
  # - Go through analysis.org manually.
  # - Do needed changes on the config.
  # - Test with ~nix build~ and running the result/bin/emacs with specified that updated config (--init-directory).
  # - Once all good, merge back into ~main~, and ~make install~.
  #
  # NEXT:
  # - [X] Extract pkg overrides.
  # - [X] Review in detail how info is extracted in used-emacs-pkgs-info.nix.
  # - [ ] Review in detail the gen-emacs-update.bash script.
  # - [ ] Document the workflow in Emacs.org.

  # TODO
  # - [ ] Consider using Home Manager instead of my Makefile that runs nix profile commands.
  #       - Seems like a great tutorial: https://github.com/Evertras/simple-homemanager/tree/main .

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      emacsPkgs = (import ./emacs-pkgs.nix { inherit pkgs; }).emacsPkgs;

      emacsWithPkgs = emacsPkgs.emacsWithPackages (epkgs:
        (import ./used-emacs-pkgs.nix epkgs)
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
      packages.${system} = rec {
        default = emacs;
        emacs = emacsWithPkgsAndCliTools;
      };
    };
}
