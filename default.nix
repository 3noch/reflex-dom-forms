let
  reflex-platform = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner  = "reflex-frp";
    repo   = "reflex-platform";
    rev    = "1482d393669711a8a528dab2ff0f174d3d768ce2";
    sha256 = "0qpa5s13hplkfdxihk9gfxcz0pv0si2ghghskg3rrf7f7xl5r67f";
  }) {};
in
reflex-platform.project ({ pkgs, ... }: {
  packages = {
    reflex-dom-forms = ./.;
  };

  shells = {
    ghc = ["reflex-dom-forms"];
    ghcjs = ["reflex-dom-forms"];
  };
})
