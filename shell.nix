with (import ./nix { });

let
  watch-server = pkgs.writeScriptBin "watch-server" ''
    watchlist=$(${pkgs.ag}/bin/ag -g ./st-backup-shared)
    watchlist="$watchlist $(${pkgs.ag}/bin/ag -g ./st-backup-server/src)"
    echo "$watchlist" | ${pkgs.entr}/bin/entr \
      ${pkgs.ghcid}/bin/ghcid -c \
      '${pkgs.haskell.packages.ghc8104.cabal-install}/bin/cabal new-repl exe:st-backup-server' \
      -T ':main --config-file ../example-config.toml' \
      --restart=./st-backup-server/st-backup-server.cabal \
      --restart=./example-config.toml \
      $(for f in $watchlist; do echo "--restart=$f"; done)
  '';
  watch-web = pkgs.writeScriptBin "watch-web" ''
    watchlist=$(${pkgs.ag}/bin/ag -g ./st-backup-shared)
    ${pkgs.ghcid}/bin/ghcid -c \
      '${pkgs.haskell.packages.ghc8104.cabal-install}/bin/cabal new-repl st-backup-web' \
      -T ':main --config-file ../example-config.toml' \
      --restart=./st-backup-web/st-backup-web.cabal \
      --restart=./example-config.toml \
      $(for f in $watchlist; do echo "--restart=$f"; done)
  '';

  packages = haskellPackages.extend (haskell.lib.packageSourceOverrides {
    st-backup-shared = ./st-backup-shared;
    st-backup-server = ./st-backup-server;
    st-backup-web = ./st-backup-web;
  });

  shell = packages.shellFor {
    packages = p: [ p.st-backup-shared p.st-backup-server p.st-backup-web ];
    withHoogle = true;
    buildInputs = [
      # Watch scripts
      watch-server
      watch-web

      # Haskell dependencies
      pkgs.haskell.compiler.ghc8104
      pkgs.haskell.packages.ghc8104.cabal-install
      pkgs.haskell.packages.ghc8104.ghcid
      pkgs.haskell.packages.ghc8104.ghcide
      pkgs.haskell.packages.ghc8104.haskell-language-server

      # Command line utilities used by st-backup-server
      pkgs.file
      pkgs.gnupg1
      pkgs.gnutar
      pkgs.tsm-client.unwrapped
    ];
  };

in shell.overrideAttrs (super: {
  shellHook = super.shellHook + ''
    export LD_LIBRARY_PATH+=:${pkgs.zlib}/lib
  '';
})
