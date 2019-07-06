{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "skellbot";
    buildInputs = [
        (haskell.packages.ghc865.ghcWithPackages (pkgs: [
            pkgs.hindent
            pkgs.hlint
            pkgs.hoogle
            pkgs.HUnit
            pkgs.regex-compat
            pkgs.wuss
        ]))
        jq
        rlwrap
    ];
    shellHook = ''
        . .env
        . .shellhook
    '';
}
