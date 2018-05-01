let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "f003577699ad5a47f8275dad4f05cdb15c4bcdf5";
      sha256 = "1fwg9cfz6p6zrlk1j5648r9hc5s2m62cwwv036sc7byb3pdhlxdr";
    }) {};
  nixpkgs = reflex-platform.nixpkgs;
in reflex-platform.project ({ pkgs, ... }: {
      packages = {
        domconv-webkit = ./.;
        leksah-server = pkgs.fetchFromGitHub {
          owner = "leksah";
          repo = "leksah-server";
          rev = "fd079941f04d637fd058474278290d78fa7b8bb1";
          sha256 = "1ydkd06i9dpmpfb6zck54vdrzxg79z72cvys7af6mdliln208cbi";
        };
      };

      overrides = self: super: {
        domconv-webkit = pkgs.haskell.lib.addBuildTool super.domconv-webkit self.happy;
      };

      tools = ghc: [ghc.happy];

      shells = {
        ghc = ["domconv-webkit"];
        ghcjs = [];
      };
  })

