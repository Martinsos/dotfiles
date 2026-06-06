# Evaluates, for a given nixpkgs rev, the version + source info of each Emacs package from
# used-emacs-pkgs.nix. Useful for figuring out which versions of emacs packages am I using,
# which is useful when updating them.
{ rev }:
let
  nixpkgs = builtins.getFlake "github:NixOS/nixpkgs/${rev}";
  system = "x86_64-linux";
  pkgs = import nixpkgs { inherit system; };

  inherit (import ./emacs-pkgs.nix { inherit pkgs; }) baseEmacs emacsPkgs;
  usedEmacsPkgs = import ./used-emacs-pkgs.nix emacsPkgs;

  getEmacsPkgInfo = d:
    let
      src = d.src;
      meta = d.meta or { };
      urls = src.urls or [ ];
      # Forge-agnostic fallback: packages like those on Codeberg expose no rev/gitRepoUrl, but
      # both are embedded in the archive url ".../<owner>/<repo>/archive/<rev>.tar.gz".
      archive = builtins.match "(https?://[^/]+/[^/]+/[^/]+)/archive/([0-9a-f]+)\\.tar\\.gz" (src.url or "");
    in {
      name     = d.pname;
      version  = d.version or null;
      # Effective upstream rev and cloneable git url (both null when there is no git source).
      rev      = src.rev or (if archive == null then null else builtins.elemAt archive 1);
      cloneUrl = src.gitRepoUrl or (if archive == null then null else builtins.elemAt archive 0 + ".git");
      # GNU ELPA & friends have no git source: the tarball is under src.urls and meta.homepage
      # points at the (navigable) package page. Useful for manual digging.
      tarball  = if urls == [ ] then null else builtins.head urls;
      homepage = meta.homepage or null;
    };
in {
  emacsVersion = baseEmacs.version;
  packages = map getEmacsPkgInfo usedEmacsPkgs;
}
