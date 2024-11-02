{
  description = "awsspendsummary";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

  outputs = { self, nixpkgs }:

    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {

      packages.x86_64-linux.default = (

        pkgs.haskellPackages.developPackage {
            root = ./.;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
                [ cabal-install
                  hlint
                  haskell-language-server
                ]);
        }).overrideAttrs(old: {
        buildInputs = old.buildInputs ++ [ pkgs.zlib ];
      });
    };
}

