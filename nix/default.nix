{ overlays ? [ ], sources ? import ./sources.nix
, hostPkgs ? import sources.nixpkgs { config.allowUnfree = true; } }:

let
  servant-jsaddle-streams-src = hostPkgs.fetchFromGitHub {
    owner = "ludvb";
    repo = "servant-jsaddle-streams";
    rev = "c44f3b6c88004205651e2d83d657442f5ad52a47";
    sha256 = "112gbv3c97497xqvsqq7zwrh8lx18qia3mqqp6rrzicf7znv19dq";
  };

  miso-src = hostPkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "69f50b49adbff8217f8b51ae5f47727ee950f204";
    sha256 = "1nwj40k7xfz4cc54hqgz3x51y3w5ymqp4v8gdm6229z7k6n5bvhn";
  };

  cabalHashes = hostPkgs.fetchurl {
    url =
      "https://github.com/commercialhaskell/all-cabal-hashes/archive/55bc90b47b1013b145a71b0554d0193f6b435673.tar.gz";
    sha256 = "0zl3awz5f7lxi3bvw2rc414id4axlrhsiy743lw025dpn9v65zsx";
  };

  defaultOverlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {

        ghc8104 = super.haskell.packages.ghc8104.override {
          all-cabal-hashes = cabalHashes;
          overrides = selfGhc: superGhc: {
            co-log-polysemy = self.haskell.lib.doJailbreak
              (selfGhc.callHackage "co-log-polysemy" "0.0.1.2" { });

            http-proxy = self.haskell.lib.dontCheck
              (self.haskell.lib.doJailbreak
                (selfGhc.callHackage "http-proxy" "0.1.1.0" { }));

            servant = self.haskell.lib.dontCheck
              (selfGhc.callHackage "servant" "0.18.2" { });
            servant-client-core = self.haskell.lib.dontCheck
              (selfGhc.callHackage "servant-client-core" "0.18.2" { });
            servant-server = self.haskell.lib.dontCheck
              (selfGhc.callHackage "servant-server" "0.18.2" { });

            servant-jsaddle-streams = self.haskell.lib.dontCheck
              (selfGhc.callCabal2nix "servant-jsaddle-streams"
                servant-jsaddle-streams-src { });

            miso =
              selfGhc.callCabal2nixWithOptions "miso" miso-src "-fjsaddle" { };
          };
        };
      };
    };
    pkgs = super.pkgs // {
      tsm-client = import ./tsm-client { pkgs = hostPkgs; };
    };
  };

  hn = import sources.haskellNix { };
  pkgs_ = import hn.sources.nixpkgs-2009 { };
  ghcjsOverlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghcjs = pkgs_.pkgs.haskell.packages.ghcjs.override {
          all-cabal-hashes = cabalHashes;
          overrides = selfGhc: superGhc: {
            ghcjs-dom = selfGhc.callHackage "ghcjs-dom" "0.9.4.0" { };
            ghcjs-dom-jsffi =
              selfGhc.callHackage "ghcjs-dom-jsffi" "0.9.4.0" { };
            ghcjs-base = self.haskell.lib.dontCheck
              (selfGhc.callHackage "ghcjs-base" "0.2.0.0" { });
            primitive = selfGhc.callHackage "primitive" "0.6.4.0" { };
            hashable = selfGhc.callHackage "hashable" "1.2.7.0" { };

            servant = self.haskell.lib.dontCheck
              (selfGhc.callHackage "servant" "0.18.2" { });
            servant-client-core = self.haskell.lib.dontCheck
              (selfGhc.callHackage "servant-client-core" "0.18.2" { });

            jsaddle = selfGhc.callHackage "jsaddle" "0.9.7.1" { };
            jsaddle-warp = self.haskell.lib.dontCheck
              (selfGhc.callHackage "jsaddle-warp" "0.9.7.1" { });

            servant-jsaddle-streams = self.haskell.lib.dontCheck
              (self.haskell.lib.doJailbreak
                (selfGhc.callCabal2nix "servant-jsaddle-streams"
                  servant-jsaddle-streams-src { }));

            miso = selfGhc.callCabal2nix "miso" miso-src { };

            entropy = selfGhc.callHackage "entropy" "0.4.1.6" { };

            hex = selfGhc.callHackage "hex" "0.1.2" { };

            aeson = self.haskell.lib.dontCheck superGhc.aeson;
            base-compat-batteries =
              self.haskell.lib.dontCheck superGhc.base-compat-batteries;
            bsb-http-chunked =
              self.haskell.lib.dontCheck superGhc.bsb-http-chunked;
            comonad = self.haskell.lib.dontCheck superGhc.comonad;
            criterion = self.haskell.lib.dontCheck superGhc.criterion;
            foldl = self.haskell.lib.dontCheck superGhc.foldl;
            Glob = self.haskell.lib.dontCheck superGhc.Glob;
            http2 = self.haskell.lib.dontCheck superGhc.http2;
            http-date = self.haskell.lib.dontCheck superGhc.http-date;
            http-types = self.haskell.lib.dontCheck superGhc.http-types;
            iproute = self.haskell.lib.dontCheck superGhc.iproute;
            lens = self.haskell.lib.dontCheck superGhc.lens;
            network-byte-order =
              self.haskell.lib.dontCheck superGhc.network-byte-order;
            QuickCheck = self.haskell.lib.dontCheck superGhc.QuickCheck;
            scientific = self.haskell.lib.dontCheck superGhc.scientific;
            semigroupoids = self.haskell.lib.dontCheck superGhc.semigroupoids;
            tasty-quickcheck =
              self.haskell.lib.dontCheck superGhc.tasty-quickcheck;
            text-short = self.haskell.lib.dontCheck superGhc.text-short;
            time-compat = self.haskell.lib.dontCheck superGhc.time-compat;
            temporary = self.haskell.lib.dontCheck superGhc.temporary;
            vector = self.haskell.lib.dontCheck superGhc.vector;
          };
        };
      };
    };
  };
in (import sources.nixpkgs {
  overlays = [ defaultOverlay ghcjsOverlay ] ++ overlays;
})
