with import <nixpkgs> {};
mkShell {
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
        shellcheck
    ];
    shellHook = ''
        . .env
        . .shellhook
    '';
}
