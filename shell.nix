{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Websocket";
    buildInputs = [
        (haskell.packages.ghc865.ghcWithPackages (pkgs: [
            pkgs.hindent
            pkgs.hlint
            pkgs.hoogle
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
