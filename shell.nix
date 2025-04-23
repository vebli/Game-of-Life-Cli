{ pkgs-unstable ? import <nixpkgs> {} }:

pkgs-unstable.mkShell {
    buildInputs = with pkgs-unstable.haskellPackages;[
        ghc
        cabal-install
    ];
    shellHook = ''
        cabal update
    '';
}
