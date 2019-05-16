{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Websocket";
    buildInputs = [
        (haskell.packages.ghc865.ghcWithPackages (pkgs: [
            pkgs.hlint
            pkgs.hoogle
            pkgs.wuss
        ]))
        jq
    ];
    shellHook = ''
        . .env
        . .shellhook
    '';
}
