# NOTE: Hand-written (unlike emacs-packages.nix etc. which are generated from Emacs.org).
#
# Evaluates, for a given nixpkgs rev, the version + source info of each Emacs package.
# Pure evaluation: reads attributes off the derivations, nothing is built.
# Used by gen-emacs-update.bash to diff an Emacs (packages) update.
#
# Which packages? By default, exactly those in emacs-packages.nix (read from the working tree
# next to this file, i.e. the *current* package list). The same list is used for both the old
# and the new nixpkgs rev: we look up each current package's version in each rev. `names` can
# override the list with a subset (used for testing).
#
# Example:
#   nix eval --json --impure --expr \
#     'import ./get-emacs-packages-info.nix { rev = "<nixpkgs-rev>"; }'
{ rev, system ? "x86_64-linux", names ? null }:
let
  nixpkgs = builtins.getFlake "github:NixOS/nixpkgs/${rev}";
  pkgs = import nixpkgs { inherit system; };
  ep = pkgs.emacsPackagesFor pkgs.emacs-pgtk;

  # Package names parsed straight from emacs-packages.nix: every line that is just an indented
  # identifier is a package. We read the names *textually* (rather than resolving the derivations)
  # so that a package absent in this nixpkgs rev yields a `missing` note via the guard below,
  # instead of breaking the whole evaluation.
  namesFromFile =
    let
      lines = builtins.filter builtins.isString
        (builtins.split "\n" (builtins.readFile ./emacs-packages.nix));
      nameOf = l:
        let m = builtins.match "[[:space:]]+([a-z][a-z0-9-]*)[[:space:]]*" l;
        in if m == null then null else builtins.head m;
    in builtins.filter (x: x != null) (map nameOf lines);

  theNames = if names == null then namesFromFile else names;

  probe = name:
    if ep ? ${name}
    then
      let
        src = ep.${name}.src;
        meta = ep.${name}.meta or { };
        urls = src.urls or [ ];
        # Forge-agnostic fallback: packages like those on Codeberg expose no rev/gitRepoUrl, but
        # both are embedded in the archive url ".../<owner>/<repo>/archive/<rev>.tar.gz".
        archive = builtins.match "(https?://[^/]+/[^/]+/[^/]+)/archive/([0-9a-f]+)\\.tar\\.gz" (src.url or "");
      in {
        inherit name;
        version  = ep.${name}.version or null;
        # Effective upstream rev and cloneable git url (both null when there is no git source).
        rev      = src.rev or (if archive == null then null else builtins.elemAt archive 1);
        cloneUrl = src.gitRepoUrl or (if archive == null then null else builtins.elemAt archive 0 + ".git");
        # GNU ELPA & friends have no git source: the tarball is under src.urls and meta.homepage
        # points at the (navigable) package page. Useful for manual digging.
        tarball  = if urls == [ ] then null else builtins.head urls;
        homepage = meta.homepage or null;
      }
    else { inherit name; missing = true; };
in {
  emacsVersion = pkgs.emacs-pgtk.version;
  packages = map probe theNames;
}
