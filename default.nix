{ overlays ? [ ] }:

with (import ./nix { inherit overlays; });

let
  # ---
  # Shared code (types etc.)
  st-backup-shared =
    pkgs.haskell.packages.ghc8104.callCabal2nix "st-backup-shared"
    ./st-backup-shared { };
  st-backup-shared-ghcjs =
    pkgs.haskell.packages.ghcjs.callCabal2nix "st-backup-shared"
    ./st-backup-shared { };

  # ---
  # API server
  st-backup-server =
    pkgs.haskell.packages.ghc8104.callCabal2nix "st-backup-server"
    ./st-backup-server {
      inherit st-backup-shared;
      zlib = pkgs.zlib;
    };
  st-backup-server-docker = pkgs.dockerTools.buildLayeredImage {
    name = "st-backup-server";
    tag = "latest";
    contents = [
      st-backup-server
      pkgs.file
      pkgs.bash
      pkgs.gnupg1
      pkgs.gnutar
      pkgs.gzip
      pkgs.tsm-client.unwrapped
    ];
    config = {
      WorkingDir = "/home/st-backup";
      Entrypoint = [ "${st-backup-server}/bin/st-backup-server" ];
    };
  };

  # ---
  # Web client
  st-backup-web-dev =
    pkgs.haskell.packages.ghc8104.callCabal2nix "st-backup-web"
    ./st-backup-web { st-backup-shared = st-backup-shared; };
  st-backup-web-client =
    pkgs.haskell.packages.ghcjs.callCabal2nix "st-backup-web" ./st-backup-web {
      st-backup-shared = st-backup-shared-ghcjs;
    };
  st-backup-web-server =
    pkgs.haskell.packages.ghc8104.callCabal2nixWithOptions "st-backup-web"
    ./st-backup-web "-frelease" { inherit st-backup-shared; };
  st-backup-web-release = pkgs.runCommand "st-backup-web" { } ''
    mkdir -p $out/{bin,http}
    cp ${st-backup-web-server}/bin/* $out/bin
    ${pkgs.closurecompiler}/bin/closure-compiler \
      --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${st-backup-web-client}/bin/st-backup-web.jsexe/all.js.externs \
                ${st-backup-web-client}/bin/st-backup-web.jsexe/all.js \
      --js_output_file ./temp.js
    mv ./temp.js $out/http/all.js
    cp ${./st-backup-web/http}/* $out/http
  '';
  st-backup-web-docker = pkgs.dockerTools.buildLayeredImage {
    name = "st-backup-web";
    tag = "latest";
    contents = [ st-backup-web-release ];
    config = {
      WorkingDir = "${st-backup-web-release}/bin";
      Entrypoint = [ "st-backup-web-server" ];
    };
  };

in {
  inherit st-backup-server st-backup-server-docker;
  inherit st-backup-web-release st-backup-web-dev st-backup-web-docker;
}
