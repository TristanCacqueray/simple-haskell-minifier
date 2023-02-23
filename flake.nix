{
  inputs = {
    hspkgs.url = "github:podenv/hspkgs/0062866e8a9c427964d69b3d38721f6b42d10534"
      # "path:///srv/github.com/podenv/hspkgs";
    ;
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      # Add the local package to the set.
      haskellExtend = hpFinal: hpPrev: {
        simple-haskell-minifier = hpPrev.callCabal2nix "simple-haskell-minifier" self { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      # The executable.
      pkg-exe = pkgs.haskell.lib.justStaticExecutables hsPkgs.simple-haskell-minifier;

      # The necessary tools to build/test the project.
      baseTools = with pkgs; [ cabal-install hlint fourmolu hsPkgs.doctest ];

    in {
      packages."x86_64-linux".default = pkg-exe;

      devShells."x86_64-linux".ci = hsPkgs.shellFor {
        packages = p: [ p.tiny-game-server ];
        buildInputs = baseTools;
      };

      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.simple-haskell-minifier ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
    };
}
