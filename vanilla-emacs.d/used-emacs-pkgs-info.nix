# A function that, for a given nixpkgs rev, returns the version + source (repo) info
# of each Emacs package that I use (used-emacs-pkgs.nix).
{ rev }:
let
  nixpkgs = builtins.getFlake "github:NixOS/nixpkgs/${rev}";
  system = "x86_64-linux";
  pkgs = import nixpkgs { inherit system; };

  inherit (import ./emacs-pkgs.nix { inherit pkgs; }) baseEmacs emacsPkgs;
  usedEmacsPkgs = import ./used-emacs-pkgs.nix emacsPkgs;

  getEmacsPkgInfo = p: {
    name     = p.pname;
    version  = p.version or null;
    homepage = (p.meta or {}).homepage or null;
    inherit (getEmacsPkgGitRevAndRepo p)
      rev
      gitRepoUrl;
  };

  getEmacsPkgGitRevAndRepo = p:
    let
      # Most popular forges (github, gitlab, codeberg, ...) follow similar shape for
      # their "archive" urls, which point to tarballs of specific ref, so we can use
      # that to obtain ref(rev) and git url, assuming src url of packages is archive url,
      # which it normally is.
      # This is useful if package doesn't have rev and git url specified directly.
      # Github/codeberg shape is ".../<owner>/<repo>/archive/<rev>.tar.gz", while gitlab
      # inserts a "/-" before "archive" and appends "/<filename>" after the rev, hence the
      # optional "(-/)?" and "(/[^/]+)?" groups below.
      archiveUrlPieces =
        builtins.match
          "(https?://[^/]+/[^/]+/[^/]+)/(-/)?archive/([^/]+)(/[^/]+)?\\.tar\\.gz"
          (p.src.url or "");
    in {
      rev      = p.src.rev or (if archiveUrlPieces == null
                               then null
                               else builtins.elemAt archiveUrlPieces 2);
      gitRepoUrl = p.src.gitRepoUrl or (if archiveUrlPieces == null
                                      then null
                                      else builtins.elemAt archiveUrlPieces 0 + ".git");
    };
in {
  emacsVersion = baseEmacs.version;
  pkgsInfo = map getEmacsPkgInfo usedEmacsPkgs;
}
