# Tsm-client built using a patched version of patchelf
# by Yarny0
#
# References:
#   - https://github.com/NixOS/nixpkgs/pull/106340
#   - https://github.com/NixOS/nixpkgs/issues/106257

{ pkgs ? (import <nixpkgs> {}).pkgs
, autoPatchelfHook ? pkgs.autoPatchelfHook
, patchelf ? pkgs.patchelf
, symlinkJoin ? pkgs.symlinkJoin
, tsm-client ? pkgs.callPackage ./tsm-client.nix {}
}:

let

  # This fixes patchelf, see
  # https://github.com/NixOS/patchelf/pull/230
  patchelf_ = patchelf.overrideAttrs (oldAttrs:
    oldAttrs // {
      patches = (oldAttrs.patches or [ ])
        ++ [ ./PR230-fix-false-alarm-for-non-overlapping-sections.patch ];
    });

  # As the unfixed patchelf is part of stdenv
  # (and can't be replaced without rebuilding everything),
  # this "injects" the fixed version and
  # uses `autoPatchelfHook` as a vehicle.
  # Luckily, this package's path (with the fixed patchelf)
  # takes precedence over stdenv's path.
  autoPatchelfHook_ = symlinkJoin {
    inherit (autoPatchelfHook) meta name;
    paths = [ autoPatchelfHook patchelf_ ];
  };

in tsm-client.override {
  autoPatchelfHook = autoPatchelfHook_;
}
