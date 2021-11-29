{
  description = "A very basic flake";
  
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          nikoProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell = { 
                tools = { 
                    cabal = { };
                    ghcid = { };
                    haskell-language-server = { };
                    hlint = { };
                 };
                buildInputs = with pkgs; [ nixpkgs-fmt postgresql zlib dbmate glibc ];
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.nikoProject.flake { };
    in flake // { faultPackage = (flake.packages."nito:exe:nito");});
}
